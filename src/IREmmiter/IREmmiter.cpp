#include "../semanticAnalyzer/typeChecker.cpp"
#include "ast.h"
#include "range/v3/view/enumerate.hpp"
#include "types.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include <iterator>
#include <string>
#include <variant>
#include <vcruntime.h>

struct FunctionMonomorph {
    llvm::Function* asLLVM;
    ast::FunctionDeclaration* definition;
    std::vector<Type> typeArguments;
    std::vector<Type> blockTypeArguments;
    std::optional<Type> thisType;
};

struct TypeMonomorph {
    llvm::Type* asLLVM;
};

struct Temp {
    size_t index;
};

struct Value {
    llvm::Value* asLLVM;
    std::variant<std::monostate, size_t, Temp> owner;
    Type type;

    constexpr Value(
        llvm::Value* _asLLVM, std::variant<std::monostate, size_t, Temp> _owner,
        Type _type
    )
        : asLLVM(_asLLVM), owner(_owner), type(_type) {}

    constexpr Value(llvm::Value* _asLLVM, size_t _owner, Type _type)
        : asLLVM(_asLLVM), owner(_owner), type(_type) {}

    constexpr Value(
        llvm::Value* _asLLVM, std::initializer_list<size_t> _owner, Type _type
    )
        : asLLVM(_asLLVM), owner(std::monostate{}), type(_type) {}

    constexpr Value() = default;

    operator llvm::Value*() {
        return asLLVM;
    }

    llvm::Value* operator->() {
        return asLLVM;
    }
};



struct IRVisitor {
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::unique_ptr<llvm::Module> currentModule;
    std::unordered_map<std::string, type::BuiltIn*>& builtInTypes;
    ImplementationScope implScope;

    std::vector<Type> const* typeArguments;
    std::vector<Type> const* blockTypeArguments;

    std::vector<FunctionMonomorph*> signatureStack{};
    ast::FunctionDeclaration* currentFunction;

    std::unordered_map<std::string, TypeMonomorph*> namedTypes{};
    std::unordered_map<std::string, FunctionMonomorph*> functions{};

    Function allocFunc;
    Function moveFunc;
    Function freeFunc;
    Function sliceFunc;

    ast::TypeDeclaration* vecDeclaration;
    ast::TypeDeclaration* stringDeclaration;
    ast::TraitDeclaration* copyDeclaration;
    ast::TraitDeclaration* dropDeclaration;
    llvm::Function* rtOobError;
    llvm::Function* rtZeroDivError;
    llvm::Function* rtSubUnderflowError;
    llvm::Function* rtAddOverflowError;
    llvm::Function* rtMulOverflowError;
    llvm::Function* rtStackOverflowError;
    llvm::GlobalVariable* stackSize;

    std::vector<Value> temporaryCleanup{};
    std::vector<ast::Binding> cleanupStack{};
    std::vector<size_t> moves{};

    std::vector<Module*>* modules;
     struct Lifetime;
    Lifetime* topLevelLifetime;



    void append(llvm::BasicBlock* block) {
        auto function = builder->GetInsertBlock()->getParent();
        function->insert(function->end(), block);
    }

    void appendAndSetInsertTo(llvm::BasicBlock* block) {
        append(block);
        builder->SetInsertPoint(block);
    }

    Value getInt(unsigned int bits, int value) {
        return {
            llvm::ConstantInt::get(*context, llvm::APInt(bits, value)),
            {},
            builtInTypes["int"]};
    }

    struct Lifetime {
        IRVisitor& ir;
        size_t cleanupSize;
        size_t temporarySize;

        Lifetime(IRVisitor& _self)
            : ir(_self), cleanupSize(ir.cleanupStack.size()),
              temporarySize(ir.temporaryCleanup.size()) {
            if (!ir.topLevelLifetime) ir.topLevelLifetime = this;
        }

        bool includes(Value value) {
            if (!std::holds_alternative<size_t>(value.owner))
                return false;

            int i = ir.cleanupStack.size() - 1;
            while (i >= static_cast<int>(cleanupSize)) {
                if (std::get<size_t>(value.owner) == i) {
                    return true;
                }
                --i;
            }
            return false;
        }

        void end() {
            size_t i = ir.temporaryCleanup.size();
            while (i > temporarySize) {
                auto value = ir.temporaryCleanup[--i];
                value.owner = std::monostate{};
                if (value.asLLVM) {
                    ir.drop(value);
                }
            }

            size_t j = ir.cleanupStack.size();
            while (j > cleanupSize) {
                auto drop = ir.cleanupStack[--j];
                auto found = ranges::find(ir.moves, j);
                if (found == ir.moves.end()) {
                    auto value = ir.builder->CreateLoad(
                        ir.asLLVM(drop.type), drop.value
                    );
                    ir.drop({value, {}, drop.type});
                } else {
                    ir.moves.erase(found);
                }
            }
        }

        void forget() {
            ir.temporaryCleanup.resize(temporarySize);
            ir.cleanupStack.resize(cleanupSize);
            if (ir.topLevelLifetime == this) ir.topLevelLifetime = nullptr;
        }

        ~Lifetime() {
            end();
            forget();
        }
    };

    struct LoopContext {
        Lifetime lifetime;
        llvm::BasicBlock* begin;
        llvm::BasicBlock* end;
        std::vector<std::pair<llvm::BasicBlock*, llvm::Value*>>* breaks;
    };

    LoopContext* loopCtx;

    bool isNever(Value value) {
        return value.type == Type{type::Never{}};
    }

    static constexpr Value never = Value{nullptr, {}, type::Never{}};

    Value null() {
        return Value{
            llvm::UndefValue::get(builtInTypes["null"]->asLLVM),
            {},
            builtInTypes["null"]};
    }

    llvm::BasicBlock* createBlock(std::string_view name) {
        return llvm::BasicBlock::Create(*context, name);
    }

    Value operator()(ast::Match&& match) {
        Value scrutinee = (*this)(*match.scrutinee);
        if (isNever(scrutinee))
            return never;
        llvm::BasicBlock* afterMatch = createBlock("afterMatch");
        scrutinee = suspend(scrutinee);
        std::vector<std::pair<llvm::BasicBlock*, llvm::Value*>> results{};
        for (auto& matchCase : match.body) {
            llvm::BasicBlock* noMatch = &matchCase == &match.body.back()
                                            ? nullptr
                                            : createBlock("noMatch");

            Value result;
            {
                auto lifetime = beginLifetime();
                BindPattern{
                    *this, noMatch, &lifetime}(matchCase.pattern, scrutinee);
                result = std::visit(*this, std::move(matchCase.value.value));
                if (isNever(result)) lifetime.forget();
                if (match.sameTypeResult) {
                    result = copy(result);
                }
            }
            builder->CreateBr(afterMatch);
            if (match.sameTypeResult && !isNever(result)) {
                results.push_back({builder->GetInsertBlock(), result});
            }
            if (noMatch) {
                appendAndSetInsertTo(noMatch);
            }
        }
        appendAndSetInsertTo(afterMatch);
        if (match.sameTypeResult) {
            auto phi = builder->CreatePHI(
                results.front().second->getType(), results.size()
            );
            for (auto [block, value] : results) {
                phi->addIncoming(value, block);
            }
            drop(release(scrutinee));
            return {phi, {}, match.type};
        }

        drop(release(scrutinee));
        return null();
    }

    Value operator()(ast::Break&& _break) {
        if (loopCtx->breaks) {
            loopCtx->breaks->push_back(
                {builder->GetInsertBlock(), (*this)(*_break.value)}
            );
        }
        loopCtx->lifetime.end();
        builder->CreateBr(loopCtx->end);
        return never;
    }

    Value operator()(ast::Return&& _return) {
        Value value = _return.value ? copy(take(*_return.value)) : null();
        topLevelLifetime->end();
        auto currentStackSize = builder->CreateLoad(type::integer.asLLVM, stackSize);
        auto newStackSize = builder->CreateSub(currentStackSize, getInt(32, 1));
        builder->CreateStore(newStackSize, stackSize);
        builder->CreateRet(value);
        return never;
    }

    Value operator()(ast::Continue&& _continue) {
        if (loopCtx->breaks) {
            loopCtx->breaks->push_back(
                {builder->GetInsertBlock(), (*this)(*_continue.value)}
            );
        }
        loopCtx->lifetime.end();
        builder->CreateBr(loopCtx->begin);
        return never;
    }

    Value operator()(lexer::CharLiteral&& character) {
        auto val= Value{getInt(8, character.value), {}, builtInTypes["char"]};
            return val;
    }

    Value operator()(lexer::StringLiteral&& string) {
        auto ptr = builder->CreateGlobalStringPtr(string.value, "strLiteral");
        auto size = getInt(32, string.value.size());
        llvm::Value* bufPtr =
            builder->CreateCall(functions["rtAlloc"]->asLLVM, {size});

        builder->CreateCall(functions["rtMove"]->asLLVM, {getInt(32, 0), bufPtr, ptr, size});
        llvm::Value* charVector = getEmptyVector(builtInTypes["char"], size, bufPtr);
        Type stringt{type::Named{stringDeclaration, {}}};
        auto s = asLLVM(stringt);
        auto undef = llvm::UndefValue::get(s);
        auto strIs = insert({undef, {}, stringt}, 0, charVector);
        return strIs;
    }

    llvm::Value* indexVector(Value vector, Value index, Span accessSpan) {
        llvm::Value* vecLen = builder->CreateExtractValue(vector, {0, 2});

        auto indexTooLarge = builder->CreateICmpSGE(index, vecLen);
        auto indexTooSmall = builder->CreateICmpSLT(index, getInt(32, 0));
        auto isOutOfBounds = builder->CreateOr({indexTooLarge, indexTooSmall});
        auto outOfBounds = createBlock("outofbounds");
        auto inBounds = createBlock("inbounds");
        builder->CreateCondBr(isOutOfBounds, outOfBounds, inBounds);
        appendAndSetInsertTo(outOfBounds);
        std::stringstream error{};
        logs::SpannedMessage msg{currentFunction->location->source, accessSpan, "", ""};
        msg.printBodyTo(error);
        auto errorMessage =
            builder->CreateGlobalStringPtr(error.str(), "oobError");
        builder->CreateCall(rtOobError, {errorMessage, index, vecLen});
        builder->CreateBr(inBounds);
        appendAndSetInsertTo(inBounds);

        llvm::Value* bufPtr = builder->CreateExtractValue(vector, {0, 0});
        auto elemType =
            asLLVM(std::get<type::Named>(with(*blockTypeArguments, *typeArguments, vector.type)).typeArguments[0]);
        return builder->CreateInBoundsGEP(elemType, bufPtr, {index});
    }

    Value operator()(ast::IndexAccess&& access) {
        auto lhs = (*this)(*access.lhs);
        auto index = (*this)(*access.index);
        if (isNever(lhs) || isNever(index))
            return never;

        llvm::Type* elementType = asLLVM(access.elementType);
        auto elementPtr = indexVector(lhs, index, access.span);
        llvm::Value* value = builder->CreateLoad(elementType, elementPtr);

        if (lhs.owner.index() == 0) {
            value = copy(Value{value, 0, access.elementType});
            drop(lhs);
        }

        return Value{value, lhs.owner, access.type};
    }

    llvm::Value* getEmptyVector(Type const& elemType, llvm::Value* size, llvm::Value* buffer) {
        llvm::Value* literal =    llvm::UndefValue::get(asLLVM(type::Named{
            vecDeclaration, {Type{elemType}}}));

        literal = builder->CreateInsertValue(literal, buffer, {0, 0});
        literal = builder->CreateInsertValue(literal, size, {0, 1});
        literal = builder->CreateInsertValue(literal, size, {0, 2});
        return literal;
    }

    Value operator()(ast::VectorLiteral&& vector) {
        auto elements = std::get_if<ast::VectorElements>(&vector.content);
        size_t nOfElems = elements ? elements->elements.size() : 0;
        llvm::Type* elemType = asLLVM(vector.elementType);

        std::vector<Value> values{};
        for (size_t i = 0; i < nOfElems; ++i) {
            auto value = (*this)(std::move(elements->elements[i]));
            if (isNever(value)) return never;
            values.push_back(suspend(value));
        }

        llvm::Value* elementSize = getInt(
            32, currentModule->getDataLayout().getTypeAllocSize(elemType)
        );
        std::vector<llvm::Value*> lengths{getInt(32, 0)};
        for (size_t i = 0; i < nOfElems; ++i) {
            if (auto spread =
                    std::get_if<ast::Spread>(&elements->elements[i].value)) {
                lengths.push_back(builder->CreateAdd(
                    lengths.back(), extract(values[i], {0, 2})
                ));
            } else {
                lengths.push_back(
                    builder->CreateAdd(lengths.back(), getInt(32, 1))
                );
            }
        }

        auto byteCap = builder->CreateMul(lengths.back(), elementSize);

        llvm::Value* ptr =
            builder->CreateCall(functions["rtAlloc"]->asLLVM, {byteCap});

        for (size_t i = 0; i < nOfElems; ++i) {
            if (auto spread =
                    std::get_if<ast::Spread>(&elements->elements[i].value)) {
                Value owned = copy(release(values[i]));
                auto offset = builder->CreateMul(lengths[i], elementSize);
                auto movedSize =
                    builder->CreateMul(extract(values[i], {0, 2}), elementSize);
                builder->CreateCall(
                    functions.at("rtMove")->asLLVM,
                    {offset, ptr, extract(owned, {0, 0}), movedSize}
                );
                drop(owned);
            } else {
                
                llvm::Value* element =
                    builder->CreateInBoundsGEP(elemType, ptr, {lengths[i]});
                builder->CreateStore(copy(release(values[i])), element);
            }
        }

        llvm::Value* literal = getEmptyVector(vector.elementType, lengths.back(), ptr);
        auto val = Value{literal, {}, vector.type};
        return val;
    }

    Value operator()(ast::VectorElement&& element) {
        return std::visit(std::move(*this), std::move(element.value));
    }

    Value operator()(ast::Spread&& spread) {
        return (*this)(spread.value);
    }

    Value operator()(ast::Expression&& expression) {
        return std::visit(std::move(*this), std::move(expression.value));
    }

    Value operator()(ast::Expression& expression) {
        auto val = std::visit(std::move(*this), std::move(expression.value));
        return val;
    }

    template <String op> Value operator()(ast::Binary<op>&& binary) {
        Value lhs = binary.lhs ? (*this)(*binary.lhs) : chainValue;
        Value rhs = (*this)(*binary.rhs);
        if (isNever(lhs) || isNever(rhs))
            return never;
        return (*this)(std::move(binary), lhs, rhs);
    }
    Value chainValue;

    Value operator()(ast::Binary<"<=">&& addition, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return {builder->CreateFCmpULE(lhs, rhs), {}, &type::boolean};
        }
        return {builder->CreateICmpSLE(lhs, rhs), {}, &type::boolean};
    }

    Value operator()(ast::Binary<">=">&& addition, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return {builder->CreateFCmpUGE(lhs, rhs), {}, &type::boolean};
        }
        return {builder->CreateICmpSGE(lhs, rhs), {}, &type::boolean};
    }

    Value operator()(ast::Binary<"<">&& addition, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return {builder->CreateFCmpULT(lhs, rhs), {}, &type::boolean};
        }
        return {builder->CreateICmpSLT(lhs, rhs), {}, &type::boolean};
    }

    Value operator()(ast::Binary<">">&& addition, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return {builder->CreateFCmpUGT(lhs, rhs), {}, &type::boolean};
        }
        return {builder->CreateICmpSGT(lhs, rhs), {}, &type::boolean};
    }

    Value operator()(ast::Binary<"/">&& addition, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return {builder->CreateFDiv(lhs, rhs), {}, &type::floating};
        }

        auto isOutOfBounds = builder->CreateICmpEQ(rhs, getInt(32, 0));
        auto outOfBounds = createBlock("outofbounds");
        auto inBounds = createBlock("inbounds");
        builder->CreateCondBr(isOutOfBounds, outOfBounds, inBounds);
        appendAndSetInsertTo(outOfBounds);
        std::stringstream error{};
        logs::SpannedMessage msg{currentFunction->location->source, addition.span, "", ""};
        msg.printBodyTo(error);
        auto errorMessage =
            builder->CreateGlobalStringPtr(error.str(), "zeroDivError");
        builder->CreateCall(rtZeroDivError, {errorMessage});
        builder->CreateBr(inBounds);
        appendAndSetInsertTo(inBounds);

        return {builder->CreateSDiv(lhs, rhs), {}, &type::integer};
    }

    Value operator()(ast::Prefix<"~">&& bsOperator) {
        auto value = (*this)(*bsOperator.rhs);
        value.owner = std::monostate{};
        return value;
    }

    Value operator()(ast::Prefix<"-">&& negation) {
        auto value = (*this)(*negation.rhs);
        if (value.type == &type::integer) {
            return {builder->CreateSub(getInt(32, 0), value), {}, &type::integer};
        }    
        return {builder->CreateFNeg(value), {}, &type::floating};
    }

    Value operator()(ast::Prefix<"!">&& negation) {
        return {builder->CreateSub(getInt(1, 1), (*this)(*negation.rhs)), {}, &type::boolean};
    }

    Value operator()(ast::Binary<"-">&& addition, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return {builder->CreateFSub(lhs, rhs), {}, &type::floating};
        }

        auto intType = lhs.asLLVM->getType();
        auto subWithOverflow = llvm::Intrinsic::getDeclaration(currentModule.get(), llvm::Intrinsic::ssub_with_overflow, intType);
        auto result = builder->CreateCall(subWithOverflow, {lhs, rhs});
        auto hasOverflown = builder->CreateExtractValue(result, {1});
        auto overflow = createBlock("overflow");
        auto noOverflow = createBlock("nooverflow");
        builder->CreateCondBr(hasOverflown, overflow, noOverflow);
        appendAndSetInsertTo(overflow);
        
        std::stringstream error{};
        logs::SpannedMessage msg{currentFunction->location->source, addition.span, "", ""};
        msg.printBodyTo(error);
        auto errorMessage =
            builder->CreateGlobalStringPtr(error.str(), "subUnderflowError");
        builder->CreateCall(rtSubUnderflowError, {errorMessage, lhs, rhs});

        builder->CreateBr(noOverflow);
        appendAndSetInsertTo(noOverflow);
        auto difference = builder->CreateExtractValue(result, {0});

        return {difference, {}, &type::integer};
    }

    Value operator()(ast::Binary<"&&">&& logicAnd) {
        Value lhs = (*this)(*logicAnd.lhs);
        auto currentBlock = builder->GetInsertBlock();
        auto shortCircuit = createBlock("short");
        auto evalRhs = createBlock("rhs");
        builder->CreateCondBr(lhs, evalRhs, shortCircuit);
        appendAndSetInsertTo(evalRhs);
        Value rhs = (*this)(*logicAnd.rhs);
        builder->CreateBr(shortCircuit);
        appendAndSetInsertTo(shortCircuit);
        auto phi = builder->CreatePHI(type::boolean.asLLVM, 2);
        phi->addIncoming(getInt(1, 0), currentBlock);
        phi->addIncoming(rhs, evalRhs);
        return {phi, {}, &type::boolean};
    }

    Value operator()(ast::Binary<"||">&& logicOr) {
        Value lhs = (*this)(*logicOr.lhs);
        auto currentBlock = builder->GetInsertBlock();
        auto shortCircuit = createBlock("short");
        auto evalRhs = createBlock("rhs");
        builder->CreateCondBr(lhs, shortCircuit, evalRhs);
        appendAndSetInsertTo(evalRhs);
        Value rhs = (*this)(*logicOr.rhs);
        builder->CreateBr(shortCircuit);
        appendAndSetInsertTo(shortCircuit);
        auto phi = builder->CreatePHI(type::boolean.asLLVM, 2);
        phi->addIncoming(getInt(1, 1), currentBlock);
        phi->addIncoming(rhs, evalRhs);
        return {phi, {}, &type::boolean};
    }

    Value operator()(ast::Binary<"+">&& addition, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return Value{builder->CreateFAdd(lhs, rhs), {}, &type::floating};
        }
        auto intType = lhs.asLLVM->getType();
        auto sumWithOverflow = llvm::Intrinsic::getDeclaration(currentModule.get(), llvm::Intrinsic::sadd_with_overflow, intType);
        auto result = builder->CreateCall(sumWithOverflow, {lhs, rhs});
        auto hasOverflown = builder->CreateExtractValue(result, {1});
        auto overflow = createBlock("overflow");
        auto noOverflow = createBlock("nooverflow");
        builder->CreateCondBr(hasOverflown, overflow, noOverflow);
        appendAndSetInsertTo(overflow);
        
        std::stringstream error{};
        logs::SpannedMessage msg{currentFunction->location->source, addition.span, "", ""};
        msg.printBodyTo(error);
        auto errorMessage =
            builder->CreateGlobalStringPtr(error.str(), "addOverflowError");
        builder->CreateCall(rtAddOverflowError, {errorMessage, lhs, rhs});

        builder->CreateBr(noOverflow);
        appendAndSetInsertTo(noOverflow);
        auto sum = builder->CreateExtractValue(result, {0});
        return Value{sum, {}, &type::integer};
    }

    Value operator()(ast::Binary<"==">&& equate, Value lhs, Value rhs) {
        chainValue = rhs;
        if (lhs.type == &type::null) return null();
        if (lhs.type == &type::floating) {
            return Value{builder->CreateFCmpUEQ(lhs, rhs), {}, &type::boolean};
        }
        return Value{builder->CreateICmpEQ(lhs, rhs), {}, &type::boolean};
    }

    Value operator()(ast::Binary<"!=">&& notEquals, Value lhs, Value rhs) {
        if (lhs.type == &type::null) return null();
        if (lhs.type == &type::floating) {
            return Value{builder->CreateFCmpUNE(lhs, rhs), {}, &type::boolean};
        }
        return Value{builder->CreateICmpNE(lhs, rhs), {}, &type::boolean};
    }

    Value operator()(ast::Binary<"*">&& multiplication, Value lhs, Value rhs) {
        if (lhs.type == &type::floating) {
            return Value{builder->CreateFMul(lhs, rhs), {}, &type::floating};
        }

        auto intType = lhs.asLLVM->getType();
        auto mulWithOverflow = llvm::Intrinsic::getDeclaration(currentModule.get(), llvm::Intrinsic::smul_with_overflow, intType);
        auto result = builder->CreateCall(mulWithOverflow, {lhs, rhs});
        auto hasOverflown = builder->CreateExtractValue(result, {1});
        auto overflow = createBlock("overflow");
        auto noOverflow = createBlock("nooverflow");
        builder->CreateCondBr(hasOverflown, overflow, noOverflow);
        appendAndSetInsertTo(overflow);
        
        std::stringstream error{};
        logs::SpannedMessage msg{currentFunction->location->source, multiplication.span, "", ""};
        msg.printBodyTo(error);
        auto errorMessage =
            builder->CreateGlobalStringPtr(error.str(), "mulOverflowError");
        builder->CreateCall(rtMulOverflowError, {errorMessage, lhs, rhs});

        builder->CreateBr(noOverflow);
        appendAndSetInsertTo(noOverflow);
        auto product = builder->CreateExtractValue(result, {0});

        return Value{product, {}, &type::integer};
    }

    Value operator()(ast::Condition&& condition) {

        auto& expr = std::get<ast::Expression>(condition.value);

        return (*this)(std::move(expr));
    }

    Value getVarPointer(auto&&, std::vector<llvm::Value*>& indices) {
        std::cout << "FATAL: invalid write" << std::endl;
        throw "FATAL: invalid write";
    }

    Value getVarPointer(
        ast::PropertyAccess&& access, std::vector<llvm::Value*>& indices
    ) {
        auto ptr = getVarPointer(std::move(*access.lhs), indices);
        for (size_t i = 0; i < access.namedDepth; ++i) {
            indices.push_back(getInt(32, 0));
        }
        indices.push_back(getInt(32, access.propertyIdx));
        return ptr;
    }

    Value getVarPointer(
        ast::IndexAccess&& access, std::vector<llvm::Value*>& indices
    ) {
        Value vec = (*this)(std::move(*access.lhs));
        return {
            indexVector(vec, (*this)(*access.index), access.span), vec.owner,
            access.elementType};
    }

    Value getVarPointer(
        ast::TupleFieldAccess&& access, std::vector<llvm::Value*>& indices
    ) {
        auto ptr = getVarPointer(*access.lhs, indices);
        for (size_t i = 0; i < access.namedDepth; ++i) {
            indices.push_back(getInt(32, 0));
        }
        indices.push_back(getInt(32, access.propertyIdx));
        return ptr;
    }

    Value
    getVarPointer(ast::Variable&& var, std::vector<llvm::Value*>& indices) {
        return {
            cleanupStack[var.binding].value, var.binding,
            cleanupStack[var.binding].type};
    }

    Value getVarPointer(
        ast::Expression&& expression, std::vector<llvm::Value*>& indices
    ) {
        return std::visit(
            [&](auto& expression) {
                return getVarPointer(std::move(expression), indices);
            },
            expression.value
        );
    }

    llvm::Value* gep(Value value, llvm::ArrayRef<llvm::Value*> indices) {
        return builder->CreateInBoundsGEP(
            asLLVM(value.type), value.asLLVM, indices
        );
    }

    Value operator()(ast::Binary<"=">&& assignment) {
        ast::Prefix<"~">* bsOp = std::get_if<ast::Prefix<"~">>(&assignment.lhs->value);
        ast::Expression& val = bsOp ? *bsOp->rhs : *assignment.lhs;

        auto uncopied = (*this)(*assignment.rhs);
        if (isNever(uncopied))
            return never;
        auto value = copy(uncopied);
        std::vector<llvm::Value*> indices{};

        indices.push_back(getInt(32, 0));
        auto varPtr = getVarPointer(std::move(val), indices);

        auto type = asLLVM(value.type);
        auto target = indices.size() > 1 ? gep(varPtr, indices) : varPtr;
        
        if (!bsOp) {
            drop({builder->CreateLoad(type, target), {}, value.type});
        }

        builder->CreateStore(value, target);

        return Value{value, varPtr.owner, assignment.type};
    }

    Value operator()(ast::Binary<"+=">&& assignment) {
        ast::Expression& val = *assignment.lhs;
        auto uncopied = (*this)(*assignment.rhs);
        if (isNever(uncopied))
            return never;
        auto value = copy(uncopied);
        std::vector<llvm::Value*> indices{};

        indices.push_back(getInt(32, 0));
        auto varPtr = getVarPointer(std::move(val), indices);

        auto type = asLLVM(value.type);
        auto target = indices.size() > 1 ? gep(varPtr, indices) : varPtr;
        
        auto rhs = builder->CreateLoad(uncopied->getType(), target);
        Value sum;
        if (uncopied.type == &type::floating) {
            sum = Value{builder->CreateFAdd(value, rhs), {}, &type::floating};
        } else {
            sum = Value{builder->CreateAdd(value, rhs), {}, &type::floating};
        }

        builder->CreateStore(sum, target);

        return Value{sum, {}, assignment.type};
    }

    Value getResolvedVarPointer(ast::Expression& expr, Type const& t) {
        std::vector<llvm::Value*> indices{};
        auto varPtr = getVarPointer(std::move(expr), indices);
        auto target = indices.size() > 1 ? gep(varPtr, indices) : varPtr;
        return {target, varPtr.owner, t};
    }

    Value operator()(ast::If&& ifExpr) {
        auto conditionValue = (*this)(std::move(*ifExpr.condition));
        if (isNever(conditionValue))
            return never;

        auto ifBlock = createBlock("if");
        auto elseBlock = createBlock("else");
        auto afterBlock = createBlock("afterif");

        builder->CreateCondBr(
            conditionValue, ifBlock, ifExpr.falseBranch ? elseBlock : afterBlock
        );

        appendAndSetInsertTo(ifBlock);

        size_t movesize = moves.size();
        auto trueVal = (*this)(*ifExpr.trueBranch);
        moves.resize(movesize);
        if (!ifExpr.hasSameTypeBranch) {
            if (trueVal.owner.index() == 0)
                drop(trueVal);
        }
        if (!isNever(trueVal))
            builder->CreateBr(afterBlock);
        ifBlock = builder->GetInsertBlock();

        bool resultIsOwned = true;

        Value falseVal;
        if (ifExpr.falseBranch) {

            appendAndSetInsertTo(elseBlock);
            size_t movesize = moves.size();
            falseVal = (*this)(*ifExpr.falseBranch);
            moves.resize(movesize);

            if (ifExpr.hasSameTypeBranch) {
                falseVal = copy(falseVal);
                trueVal = copy(trueVal);
            } else {
                if (falseVal.owner.index() == 0)
                    drop(falseVal);
            }

            if (!isNever(falseVal))
                builder->CreateBr(afterBlock);
            elseBlock = builder->GetInsertBlock();
        }

        appendAndSetInsertTo(afterBlock);

        if (ifExpr.hasSameTypeBranch) {
            if (isNever(trueVal)) {
                return falseVal;
            } else if (isNever(falseVal)) {
                return trueVal;
            } else {
                auto phi = builder->CreatePHI(trueVal->getType(), 2);
                phi->addIncoming(trueVal, ifBlock);
                phi->addIncoming(falseVal, elseBlock);
                return Value{phi, {}, ifExpr.type};
            }
        }

        return null();
    }

    Value operator()(ast::While&& loop) {
        auto function = builder->GetInsertBlock()->getParent();
        auto conditionBlock = createBlock("loopcondition");
        auto loopBlock = createBlock("loopbody");
        auto afterBlock = createBlock("afterwhile");

        {
            LoopContext loopContext{
                beginLifetime(),
                conditionBlock,
                afterBlock,
            };
            loopCtx = &loopContext;

            builder->CreateBr(conditionBlock);
            appendAndSetInsertTo(conditionBlock);
            if (std::visit(
                    match{
                        [&](ast::Expression& expression) {
                            Value condval = (*this)(std::move(expression));
                            if (isNever(condval))
                                return true;

                            builder->CreateCondBr(
                                condval, loopBlock, afterBlock
                            );
                            return false;
                        },
                        [&](ast::LetBinding& binding) {
                            auto initialValue = (*this)(binding.initalValue);
                            if (isNever(initialValue))
                                return true;
                            BindPattern{
                                *this, afterBlock, &loopContext.lifetime}(
                                binding.binding, (*this)(binding.initalValue)
                            );
                            builder->CreateBr(loopBlock);
                            return false;
                        },
                        [&](ast::VarBinding& binding) {
                            auto initialValue = (*this)(binding.initalValue);
                            if (isNever(initialValue))
                                return true;
                            BindPattern{
                                *this, afterBlock, &loopContext.lifetime}(
                                binding.binding, (*this)(binding.initalValue)
                            );
                            builder->CreateBr(loopBlock);
                            return false;
                        },
                    },
                    loop.condition->value
                )) {
                return never;
            }

            appendAndSetInsertTo(loopBlock);
            size_t movesize = moves.size();
            auto bodyValue = (*this)(*loop.body);
            moves.resize(movesize);
            if (bodyValue.owner.index() == 0)
                drop(bodyValue);
            loopBlock = builder->GetInsertBlock();
        }

        builder->CreateBr(conditionBlock);
        appendAndSetInsertTo(afterBlock);

        return null();
    }

    Value operator()(ast::TypeExpression&& typeExpression) {
        auto type = asLLVM(typeExpression.type);

        llvm::Value* size =
            getInt(32, currentModule->getDataLayout().getTypeAllocSize(type));
        auto literal = llvm::UndefValue::get(llvm::StructType::get(
            *context,
            llvm::ArrayRef{static_cast<llvm::Type*>(
                llvm::StructType::get(*context, llvm::ArrayRef{size->getType()})
            )}
        ));
        auto val = builder->CreateInsertValue(literal, size, {0});
        return {val, {}, typeExpression.type};
    }

    static void printllvm(llvm::StringRef str) {
        for (auto chr : str) {
            std::cout << chr;
        }
        std::cout << std::endl;
    }

    Value operator()(ast::Variable&& variable) {
        if (variable.binding <
            builder->GetInsertBlock()->getParent()->arg_size() && !(currentFunction->isMutation && variable.binding == 0)) {
            return {
                cleanupStack[variable.binding].value, variable.binding,
                cleanupStack[variable.binding].type};
        }

        return {
            builder->CreateLoad(
                asLLVM(cleanupStack[variable.binding].type),
                cleanupStack[variable.binding].value
            ),
            variable.binding, cleanupStack[variable.binding].type};
    }

    Value operator()(ast::Block&& block) {
        auto lifetime = beginLifetime();
        auto last = block.hasTrailingExpression ? --block.items.end()
                                                : block.items.end();
        for (auto item = block.items.begin(); item != last; ++item) {
            if (isNever((*this)(*item))) {
                lifetime.forget();
                return never;
            }
        }
        if (block.hasTrailingExpression) {
            auto value = (*this)(block.items.back(), false);
            if (lifetime.includes(value)) {
                value = copy(value);
            }
            
            return value;
        }
        return null();
    }

    Value operator()(ast::BlockItem& item, bool dropResult = true) {
        auto value = dropResult ? std::visit(*this, std::move(item.value))
                                : take(std::get<ast::Expression>(item.value));
        ast::Expression* expression = std::get_if<ast::Expression>(&item.value);
        if (expression && dropResult) {
            if (value.owner.index() == 0) {
                drop(value);
            }
        }
        return value;
    }

    Value operator()(lexer::IntegerLiteral&& integer) {
        return getInt(32, integer.value);
    }

    Value operator()(lexer::FloatLiteral&& floating) {
        return {llvm::ConstantFP::get(*context, llvm::APFloat(floating.value)), {}, builtInTypes["float"]};
    }

    Value operator()(ast::TupleLiteral&& tuple) {
        std::vector<Value> fieldValues{};
        for (auto& field : tuple.fields) {
            auto value = (*this)(field);
            if (isNever(value)) return never;
            fieldValues.push_back(suspend(value));
        }

        llvm::Value* literal = llvm::UndefValue::get(asLLVM(tuple.type));
        for (unsigned int i = 0; i < tuple.fields.size(); ++i) {
            literal = builder->CreateInsertValue(literal, copy(release(fieldValues[i])), {i});
        }

        return {literal, {}, tuple.type};
    }

    Value operator()(ast::StructLiteral&& structure) {
        std::vector<Value> fieldValues{};
        std::vector<std::pair<std::string*, llvm::Type*>> fieldTypesWithNames{};
        for (auto& [_, name, expression] : structure.properties) {
            auto value = (*this)(std::move(expression.value()));
            if (isNever(value)) return never;
            fieldTypesWithNames.push_back({&name, value->getType()});
            fieldValues.push_back(suspend(value));
        }
        std::ranges::sort(
            fieldTypesWithNames, {},
            [](std::pair<std::string*, llvm::Type*>& pair
            ) -> std::string const& { return *pair.first; }
        );
        std::vector<llvm::Type*> fieldTypes{};
        for (auto& [_, type] : fieldTypesWithNames) {
            fieldTypes.push_back(type);
        }

        llvm::Value* literal =
            llvm::UndefValue::get(llvm::StructType::get(*context, fieldTypes));
        for (unsigned int i = 0; i < structure.properties.size(); ++i) {
            auto fieldIdx = static_cast<unsigned int>(std::distance(
                fieldTypesWithNames.begin(),
                std::find_if(
                    fieldTypesWithNames.begin(), fieldTypesWithNames.end(),
                    [&](auto& elem) {
                        return *elem.first == structure.properties[i].name;
                    }
                )
            ));
            literal =
                builder->CreateInsertValue(literal, copy(release(fieldValues[i])), {fieldIdx});
        }

        if (!structure.namedType) {
            return {literal, {}, structure.type};
        }

        return {
            builder->CreateInsertValue(
                llvm::UndefValue::get(asLLVM(structure.type)), literal, {0}
            ),
            {},
            structure.type};
    }

    Value operator()(ast::VariantLiteral&& variant) {
        Value fieldValue = variant.value ? (*this)(*variant.value) : null();

        type::Variant type = variant.namedType 
            ? std::get<type::Variant>(variant.namedType->declaration->proto) 
            : type::Variant{{{variant.variant.value, fieldValue.type}}};

        llvm::AllocaInst* literal =
            createEntryBlockAlloca(asLLVM(type));
        Value variantValue = {literal, fieldValue.owner, type::Variant{type}};

        auto pos = ranges::find(type.variants, variant.variant.value, &std::pair<std::string, Type>::first);
        builder->CreateStore(getInt(64, std::distance(type.variants.begin(), pos)), gep(variantValue, {getInt(32, 0)}));
        builder->CreateStore(fieldValue, gep(variantValue, {getInt(32, 1)}));

        Value variantValueLoaded = {builder->CreateLoad(asLLVM(type), variantValue), fieldValue.owner, type::Variant{type}};

        if (!variant.namedType) {
            return variantValueLoaded;
        }

        return {
            builder->CreateInsertValue(
                llvm::UndefValue::get(asLLVM(variant.type)), variantValueLoaded, {0}
            ),
            variantValueLoaded.owner,
            variant.type};
    }

    FunctionMonomorph* createSignature(
        Function const& function, std::string_view name,
        std::optional<Type> thisType = {},
        std::vector<Type> const& blockTArgs = {}
    ) {
        std::vector<llvm::Type*> llvmParamTypes{};
        if (thisType) {
            llvmParamTypes.push_back(function.declaration->isMutation ? llvm::PointerType::get(*context, 0) : asLLVM(*thisType));
        }
        for (auto& arg : function.declaration->parameters) {
            llvmParamTypes.push_back(
                asLLVM(with(blockTArgs, function.typeArguments, arg.type))
            );
        }
        auto resolved = with(
            blockTArgs, function.typeArguments, function.declaration->returnType
        );
        auto returnTypeAsLLVM = asLLVM(resolved);
        auto funcType =
            llvm::FunctionType::get(returnTypeAsLLVM, llvmParamTypes, false);
        auto llvmFunc = llvm::Function::Create(
            funcType, llvm::Function::ExternalLinkage, name, currentModule.get()
        );
        size_t i = 0;
        for (auto& arg : llvmFunc->args()) {
            if (thisType) {
                if (i == 0) {
                    arg.setName("this");
                } else {
                    arg.setName(
                        function.declaration->parameters[i - 1].name.value
                    );
                }
            } else {
                arg.setName(function.declaration->parameters[i].name.value);
            }
            ++i;
        }
        auto resolvedThis = thisType;
        if (resolvedThis)
            resolvedThis = with(*blockTypeArguments, *this->typeArguments, *thisType);

        auto func = new FunctionMonomorph{
            llvmFunc,
            function.declaration,
            with(*blockTypeArguments, *typeArguments, function.typeArguments),
            with(*blockTypeArguments, *this->typeArguments, blockTArgs),
            resolvedThis,
        };
        return func;
    }

    llvm::Value* callTraitMethod(
        Value thisValue, size_t methodIdx,
        std::vector<Type> const& typeArguments, TraitImplRef const& impl,
        std::vector<Value>& args
    ) {
        ast::FunctionDeclaration& target =
            impl.declaration->implementations[methodIdx];

        auto name = nameOf(with(impl.typeArguments, {}, impl.declaration->type)
                    ) // type name
                    + "$" +
                    impl.declaration->trait.declaration->fullName // trait name
                    + nameOf(with(
                                 impl.typeArguments, {}, impl.declaration->trait
                      ) // trait args
                                 .typeArguments) +
                    "$" + target.name.value + nameOf(typeArguments);

        auto& func = functions[name];
        if (!func) {
            func = createSignature(
                Function{&target, typeArguments}, name, thisValue.type,
                impl.typeArguments
            );
            signatureStack.push_back(func);
        }

        args.insert(args.begin(), thisValue);

        return call(func, args);
    }

    llvm::Value* callMethod(
        Value thisValue, size_t methodIdx,
        std::vector<Type> const& typeArguments, ImplRef const& impl,
        std::vector<Value>& args
    ) {
        ast::FunctionDeclaration& target =
            impl.declaration->functions[methodIdx];

        auto name = nameOf(with(impl.typeArguments, {}, impl.declaration->type)
                    ) // type name
                    + "$" + target.name.value + nameOf(typeArguments);

        auto& func = functions[name];
        if (!func) {
            func = createSignature(
                Function{&target, typeArguments}, name, thisValue.type,
                impl.typeArguments
            );
            signatureStack.push_back(func);
        }
        args.insert(args.begin(), thisValue);
        return call(func, args);
    }

    llvm::Value* call(FunctionMonomorph* func, std::vector<Value> const& args) {
        std::vector<llvm::Value*> argVals{};
        for (auto& val : args) {
            argVals.push_back(val.asLLVM);
        }
        auto cl = builder->CreateCall(func->asLLVM, argVals);
        return cl;
    }

    std::optional<std::vector<Value>> getArgs(ast::Call& call) {
        std::vector<Value> args{};
        for (auto&& [i, arg] : call.argValues | views::enumerate) {
            Value argValue = (*this)(arg);
            if (isNever(argValue)) {
                return {};
            }
            args.push_back(argValue);
        }
        return args;
    }

    Value operator()(ast::Call&& call) {
        auto ret = std::visit(
            match{
                [&](Function const& function) -> Value {
                    auto maybeArgs = getArgs(call);
                    if (!maybeArgs) return never;
                    auto args = std::move(*maybeArgs);
                    auto monomorphName =
                        function.declaration->annotation
                            ? function.declaration->name.value
                            : function.declaration->fullName +
                                  nameOf(function.typeArguments);
                    auto& func = functions[monomorphName];
                    if (!func) {
                        func = createSignature(function, monomorphName);
                        signatureStack.push_back(func);
                    }
                    auto result = this->call(func, args);
                    for (size_t i = 0; i < args.size(); ++i) {
                        if (args[i].owner.index() == 0)
                            drop(args[i]);
                    }
                    return {result, {}, call.type};
                },
                [&](type::Named const& type) -> Value {
                    auto maybeArgs = getArgs(call);
                    if (!maybeArgs) return never;
                    auto args = std::move(*maybeArgs);
                    
                    auto literal = llvm::UndefValue::get(asLLVM(type));
                    if (!std::holds_alternative<type::Tuple>(type.declaration->proto)) {
                        auto res = builder->CreateInsertValue(literal, copy(args[0]), {0});
                        return {res, {}, call.type};
                    }
                    for (size_t i = 0; i < args.size(); ++i) {
                        builder->CreateInsertValue(
                            literal, copy(args[i]),
                            {0, static_cast<unsigned int>(i)}
                        );
                    }

                    return {literal, {}, call.type};
                },
                [&](TraitMethodRef const& method) -> Value {
                    ast::PropertyAccess& access =
                        std::get<ast::PropertyAccess>(call.lhs->value);
                    auto lhs = access.lhs ? (*this)(*access.lhs) : chainValue;
                    if (method.trait.declaration->signatures[access.propertyIdx].isMutation) {
                        lhs = getResolvedVarPointer(*access.lhs, lhs.type);
                    }

                    auto maybeArgs = getArgs(call);
                    if (!maybeArgs) return never;
                    auto args = std::move(*maybeArgs);


                    TraitImplRef impl = implScope.find(
                        with(*blockTypeArguments, *typeArguments, lhs.type),
                        with(*blockTypeArguments, *typeArguments, method.trait)
                    );

                    // impl.
                    if (args.size() > 0) chainValue = args.back();
                    std::vector<Value> args2{args};
                    auto result = callTraitMethod(
                        lhs, access.propertyIdx, method.typeArguments, impl,
                        args2
                    );
                    if (lhs.owner.index() == 0) drop(lhs);
                    for (size_t i = 0; i < args.size(); ++i) {
                        if (args[i].owner.index() == 0)
                            drop(args[i]);
                    }
                    return {result, {}, call.type};
                },
                [&](ImplRef const& method) -> Value {
                    ast::PropertyAccess& access =
                        std::get<ast::PropertyAccess>(call.lhs->value);
                       
                    auto lhs = (*this)(*access.lhs);
                    if (method.declaration->functions[access.propertyIdx].isMutation) {
                        lhs = getResolvedVarPointer(*access.lhs, lhs.type);
                    }
                    auto maybeArgs = getArgs(call);
                    if (!maybeArgs) return never;
                    auto args = std::move(*maybeArgs);

                    std::vector<Value> args2{args};
                    auto result = callMethod(
                        lhs, access.propertyIdx, method.funTypeArguments, method,
                        args2
                    );

                    for (size_t i = 0; i < args.size(); ++i) {
                        if (args[i].owner.index() == 0)
                            drop(args[i]);
                    }

                    return {result, {}, call.type};
                },
            },
            call.target
        );

        return {std::move(ret), {}, call.type};
    }

    Value operator()(ast::PropertyAccess&& access) {
        auto target = (*this)(*access.lhs);

        if (isNever(target)) {
            return never;
        }
        std::vector<unsigned int> indices{};
        for (size_t i = 0; i < access.namedDepth; ++i) {
            indices.push_back(0);
        }
        indices.push_back(static_cast<unsigned int>(access.propertyIdx));
        auto val = builder->CreateExtractValue(target, indices);
        return {val, target.owner, access.type};
    }

    Value operator()(ast::TupleFieldAccess&& access) {
        auto target = (*this)(*access.lhs);
        if (isNever(target)) {
            return never;
        }
        std::vector<unsigned int> indices{};
        for (size_t i = 0; i < access.namedDepth; ++i) {
            indices.push_back(0);
        }
        indices.push_back(static_cast<unsigned int>(access.propertyIdx));
        return {
            builder->CreateExtractValue(target, indices), target.owner,
            access.type};
    }

    Value extract(Value value, size_t index) {
        return extract(value, llvm::ArrayRef{static_cast<unsigned int>(index)});
    }

    Value extract(Value value, llvm::ArrayRef<unsigned int> indices) {
        Type type = value.type;
        for (auto index : indices) {
            type = type[index];
        }
        return {
            builder->CreateExtractValue(value.asLLVM, indices),
            value.owner,
            type,
        };
    }

    Value insert(Value value, size_t index, llvm::Value* field) {
        return insert(
            value, llvm::ArrayRef{static_cast<unsigned int>(index)}, field
        );
    }

    Value
    insert(Value value, llvm::ArrayRef<unsigned int> indices, llvm::Value* field) {
        return {
            builder->CreateInsertValue(value.asLLVM, field, indices),
            value.owner,
            value.type,
        };
    }

    Value suspend(Value value) {
        if (value.owner.index() != 0)
            return value;
        value.owner = Temp{temporaryCleanup.size()};
        temporaryCleanup.push_back(value);
        return value;
    }

    Value release(Value value) {
        if (value.owner.index() != 2)
            return value;
        if (temporaryCleanup[std::get<Temp>(value.owner).index].asLLVM ==
            value.asLLVM) {

            temporaryCleanup[std::get<Temp>(value.owner).index].asLLVM =
                nullptr;
            value.owner = std::monostate{};
        }
        return value;
    }

    Value load(size_t index) {
        ast::Binding& binding = cleanupStack[index];
        return {
            builder->CreateLoad(asLLVM(binding.type), binding.value),
            index,
            binding.type,
        };
    }

    Value operator()(ast::Cast&& cast) {
        auto value = (*this)(*cast.lhs);
        if (auto lhsUnion = std::get_if<type::Union>(&cast.type)) {
            Value unionValue = {createEntryBlockAlloca(asLLVM(*lhsUnion)), value.owner, Type{type::Union{*lhsUnion}}};
            if (auto rhsUnion = std::get_if<type::Union>(&value.type)) {
                auto discriminant = gep(unionValue, {getInt(32, 0)});
                auto body = gep(unionValue, {getInt(32, 1)});
                auto sourceDiscriminant = extract(value, 0);
                auto sourceBody = extract(value, 1);
                auto afterSwitch = createBlock("afterswitch");
                llvm::SwitchInst* _switch = builder->CreateSwitch(sourceDiscriminant, afterSwitch);
                for (auto&& [i, element] : rhsUnion->elements | views::enumerate) {
                    auto block = createBlock("elem");
                    _switch->addCase(llvm::ConstantInt::get(*context, llvm::APInt(64, i)), block);
                    appendAndSetInsertTo(block);
                    auto typePosition = ranges::find(lhsUnion->elements, element);
                    builder->CreateStore(getInt(64, std::distance(lhsUnion->elements.begin(), typePosition)), discriminant);
                    builder->CreateBr(afterSwitch);
                }
                appendAndSetInsertTo(afterSwitch);
                builder->CreateStore(sourceBody, body);
            } else {
                auto discriminant = gep(unionValue, {getInt(32, 0)});
                auto body = gep(unionValue, {getInt(32, 1)});
                auto typePosition = ranges::find(lhsUnion->elements, value.type);
                builder->CreateStore(getInt(64, std::distance(lhsUnion->elements.begin(), typePosition)), discriminant);
                builder->CreateStore(value, body);
            }
            unionValue.asLLVM = builder->CreateLoad(asLLVM(*lhsUnion), unionValue.asLLVM);
            return unionValue;
        }

        if (auto lhsVariant = std::get_if<type::Variant>(&cast.type)) {
            Value variantValue = {createEntryBlockAlloca(asLLVM(*lhsVariant)), value.owner, Type{type::Variant{*lhsVariant}}};
            type::Variant& rhsVariant = std::get<type::Variant>(value.type);
            auto discriminant = gep(variantValue, {getInt(32, 0)});
            auto body = gep(variantValue, {getInt(32, 1)});
            auto sourceDiscriminant = extract(value, 0);
            auto sourceBody = extract(value, 1);
            auto afterSwitch = createBlock("afterswitch");
            llvm::SwitchInst* _switch = builder->CreateSwitch(sourceDiscriminant, afterSwitch);
            for (auto&& [i, element] : rhsVariant.variants | views::enumerate) {
                auto block = createBlock("variant");
                _switch->addCase(llvm::ConstantInt::get(*context, llvm::APInt(64, i)), block);
                appendAndSetInsertTo(block);
                auto typePosition = ranges::find(lhsVariant->variants, element.first, &std::pair<std::string, Type>::first);
                builder->CreateStore(getInt(64, std::distance(lhsVariant->variants.begin(), typePosition)), discriminant);
                builder->CreateBr(afterSwitch);
            }
            appendAndSetInsertTo(afterSwitch);
            builder->CreateStore(sourceBody, body);
            variantValue.asLLVM = builder->CreateLoad(asLLVM(*lhsVariant), variantValue.asLLVM);
            return variantValue;
        }

        return value;
    }

    struct BindPattern {
        IRVisitor& ir;
        llvm::BasicBlock* noMatch;
        Lifetime* lifetime;

        void operator()(ast::Pattern& pattern, Value value) {
            (*this)(pattern.body, value);
            if (pattern.guard) {
                emitGuard(ir(*pattern.guard));
            }
        }

        void emitGuard(llvm::Value* guard) {
            auto function = ir.builder->GetInsertBlock()->getParent();
            auto afterGuard = ir.createBlock("afterguard");
            auto cleanup = ir.createBlock("noMatchCleanup");
            ir.builder->CreateCondBr(guard, afterGuard, cleanup);
            ir.appendAndSetInsertTo(cleanup);
            lifetime->end();
            ir.builder->CreateBr(noMatch);
            ir.appendAndSetInsertTo(afterGuard);
        }

        void assignBinding(Value value) {
            ir.cleanupStack.push_back({});
            auto& binding = ir.cleanupStack.back();
            llvm::Value* val = ir.copy(value);
            binding.value = ir.createEntryBlockAlloca(value->getType());
            binding.type = value.type;
            ir.builder->CreateStore(val, binding.value);
        }

        void operator()(ast::PatternBody& body, Value value) {
            std::visit(
                match{
                    [&](ast::Destructure& destructure) {
                        std::visit(
                            [&](auto& dest) { (*this)(dest, value); },
                            destructure.value
                        );
                    },
                    [&](ast::Expression& expression) {
                        std::optional<Lifetime> lf{};
                        if (body.anonymous) {
                            lf.emplace(ir.beginLifetime());
                        }
                        assignBinding(value);
                        if (!std::holds_alternative<ast::Variable>(
                                expression.value
                            )) {
                            emitGuard(ir(expression));
                        }
                    },
                },
                body.value
            );
        }

        void operator()(
            ast::PropertyPattern& property, std::vector<Value>& propertyValues
        ) {
            if (!property.pattern) {
                for (size_t index : property.propertyIndices) {
                    assignBinding(ir.release(propertyValues[index]));
                }
            }
            if (property.pattern) {
                (*this)(*property.pattern, ir.release(propertyValues[0]));
            } else if (!std::holds_alternative<ast::Variable>(
                           property.property.value
                       )) {
                emitGuard(ir(property.property));
            }
        }

        void operator()(ast::DestructureStruct& structure, Value value) {
            if (structure.name && ir.isDrop(value.type)) {
                value = ir.suspend(value);
            }
            Value rawStruct = structure.name ? ir.extract(value, 0) : value;

            size_t propCount =
                std::get<type::Struct>(value.type).properties.size();
            std::vector<Value> boundProps;
            for (size_t i = 0; i < propCount; ++i) {
                boundProps.push_back(ir.suspend(ir.extract(rawStruct, i)));
            }

            for (auto& property : structure.properties) {
                (*this)(property, boundProps);
            }

            if (structure.name && ir.isDrop(value.type)) {
                ir.drop(ir.release(value));
            }
        }

        void operator()(ast::DestructureVariant& variant, Value value) {
            Value rawValue = std::holds_alternative<type::Named>(value.type) ? ir.extract(value, 0) : value;

            auto& type = std::get<type::Variant>(rawValue.type);
            auto found = ranges::find(type.variants, variant.name.value, &std::pair<std::string, Type>::first);
            auto index = std::distance(type.variants.begin(), found);
            auto discriminant = ir.builder->CreateExtractValue(rawValue, 0);        
            Type valueType = found->second;

            rawValue = ir.suspend(rawValue);
            emitGuard(ir.builder->CreateICmpEQ(discriminant, ir.getInt(64, index))
            );
            rawValue = ir.release(rawValue);
            auto variantValue = ir.createEntryBlockAlloca(ir.asLLVM(type));
            ir.builder->CreateStore(rawValue, variantValue);
            auto cast = ir.builder->CreateLoad(ir.asLLVM(valueType), ir.gep({variantValue, {}, rawValue.type}, {ir.getInt(32, 0)}));
            if (variant.pattern) {
                (*this)(*variant.pattern, {cast, rawValue.owner, valueType});
            } else {
                assignBinding({cast, rawValue.owner, valueType});
            }
            
        }

        void operator()(ast::DestructureUnion& _union, Value value) {
            Value rawValue = std::holds_alternative<type::Named>(value.type) ? ir.extract(value, 0) : value;

            auto& type = std::get<type::Union>(rawValue.type);
            auto found = ranges::find(type.elements, _union.type);
            auto index = std::distance(type.elements.begin(), found);
            auto discriminant = ir.builder->CreateExtractValue(rawValue, 0);        

            rawValue = ir.suspend(rawValue);
            emitGuard(ir.builder->CreateICmpEQ(discriminant, ir.getInt(64, index))
            );
            rawValue = ir.release(rawValue);
            auto _unionValue = ir.createEntryBlockAlloca(ir.asLLVM(type));
            ir.builder->CreateStore(rawValue, _unionValue);
            auto cast = ir.builder->CreateLoad(ir.asLLVM(_union.type), ir.gep({_unionValue, {}, rawValue.type}, {ir.getInt(32, 0)}));
            (*this)(*_union.fields, {cast, rawValue.owner, _union.type});
        }

        void operator()(ast::DestructureVector& vector, Value value) {
            ast::RestElements* restBinding{};

            size_t minSize = std::ranges::count_if(
                vector.items,
                [](ast::ElementPattern const& elem) {
                    return std::holds_alternative<ast::Pattern>(elem.value);
                }
            );

            size_t restOffset = std::distance(
                vector.items.begin(),
                std::ranges::find_if(
                    vector.items,
                    [](ast::ElementPattern const& elem) {
                        return std::holds_alternative<ast::RestElements>(
                            elem.value
                        );
                    }
                )
            );

            llvm::Type* elementType = ir.asLLVM(vector.elementType);

            Value rawVec = ir.extract(value, 0);
            Value length = ir.extract(rawVec, 2);

            value = ir.suspend(value);
            emitGuard(ir.builder->CreateICmpSGE(length, ir.getInt(32, minSize))
            );
            value = ir.release(value);

            Value ptr = ir.extract(rawVec, 0);

            std::vector<Value> boundValues{};
            for (size_t i = 0; i < vector.items.size(); ++i) {
                if (ast::Pattern* pattern =
                        std::get_if<ast::Pattern>(&vector.items[i].value)) {
                    llvm::Value* index =
                        i >= restOffset
                            ? ir.builder->CreateSub(
                                  length, ir.getInt(32, vector.items.size() - i)
                              )
                            : ir.getInt(32, i);
                    llvm::Value* elemPtr = ir.builder->CreateInBoundsGEP(
                        elementType, ptr, {index}
                    );
                    boundValues.push_back(ir.suspend({
                        ir.builder->CreateLoad(elementType, elemPtr),
                        value.owner,
                        vector.elementType,
                    }));
                }
            }

            std::optional<Value> sliced{};
            if (restOffset != vector.items.size()) {
                Value newLen = {
                    ir.builder->CreateSub(length, ir.getInt(32, minSize)),
                    {},
                    ir.builtInTypes["int"]};

                auto size = ir.currentModule->getDataLayout().getTypeAllocSize(
                    elementType
                );
                sliced = ir.insert(ir.copy(value), {0, 2}, newLen);
                Value byteLen = {
                    ir.builder->CreateMul(newLen, ir.getInt(32, size)),
                    {},
                    ir.builtInTypes["int"]};
                auto offset = ir.getInt(32, restOffset * size);
                auto newPtr = ir.extract(*sliced, {0, 0});
                ir.call(ir.functions["rtSlice"], {newPtr, offset, byteLen});
                sliced = ir.suspend(*sliced);
            } else if (value.owner.index() == 0) {
                ir.call(ir.functions["rtFree"], {ptr});
            }

            auto item = boundValues.begin();
            for (size_t i = 0; i < vector.items.size(); ++i) {
                std::visit(
                    match{
                        [&](ast::RestElements& rest) {
                            assignBinding(ir.release(*sliced));
                        },
                        [&](ast::Pattern& pattern) {
                            (*this)(pattern, ir.release(*item));
                            ++item;
                        },
                    },
                    vector.items[i].value
                );
            }
        }

        void operator()(ast::DestructureTuple& tuple, Value value) {
            if (tuple.name && ir.isDrop(value.type)) {
                value = ir.suspend(value);
            }
            Value rawTuple = tuple.name ? ir.extract(value, 0) : value;

            std::vector<Value> fields{};
            for (size_t i = 0; i < tuple.fields.size(); ++i) {
                fields.push_back(ir.suspend(ir.extract(rawTuple, i)));
            }

            for (size_t i = 0; i < tuple.fields.size(); ++i) {
                (*this)(tuple.fields[i], ir.release(fields[i]));
            }

            if (tuple.name && ir.isDrop(value.type)) {
                ir.drop(ir.release(value));
            }
        }
    };

    Value operator()(ast::LetBinding&& let) {
        auto value = (*this)(let.initalValue);
        if (isNever(value)) {
            return never;
        }
        BindPattern{*this, nullptr}(let.binding, value);
        return null();
    }

    llvm::AllocaInst* createEntryBlockAlloca(llvm::Type* type) {
        auto& entryBlock =
            builder->GetInsertBlock()->getParent()->getEntryBlock();
        llvm::IRBuilder<> tempBuilder(&entryBlock, entryBlock.begin());
        return tempBuilder.CreateAlloca(type);
    }

    Value operator()(ast::VarBinding&& var) {
        auto init = (*this)(var.initalValue);
        if (isNever(init)) {
            return never;
        }
        BindPattern{*this, nullptr}(var.binding, init);
        return null();
    }

    void operator()(FunctionMonomorph* function) {
        if (function->definition->annotation) {
            return;
        }

        auto entry = createBlock("entry");
        function->asLLVM->insert(function->asLLVM->end(), entry);
        builder->SetInsertPoint(entry);

        typeArguments = &function->typeArguments;
        blockTypeArguments = &function->blockTypeArguments;
        auto arg = function->asLLVM->args().begin();
        ast::Binding temp;
        if (function->thisType) {
            cleanupStack.push_back({&*arg, *function->thisType});
            ++arg;
        }
        size_t i = 0;
        while (arg != function->asLLVM->args().end()) {
            cleanupStack.push_back(
                {&*arg, with(
                            *blockTypeArguments, *typeArguments,
                            function->definition->parameters[i].type
                        )}
            );
            ++arg;
            ++i;
        }

        auto currentStackSize = builder->CreateLoad(type::integer.asLLVM, stackSize);
        auto stackOverflow = createBlock("stackoverflow");
        auto noStackOverflow = createBlock("nostackoverflow");
        auto stackWillOverflow = builder->CreateICmpSGE(currentStackSize, getInt(32, 1000));
        builder->CreateCondBr(stackWillOverflow, stackOverflow, noStackOverflow);
        appendAndSetInsertTo(stackOverflow);
        std::stringstream error{};
        logs::SpannedMessage msg{currentFunction->location->source, static_cast<ast::Signature&>(*currentFunction).span, "", ""};
        msg.printBodyTo(error);
        auto errorMessage =
            builder->CreateGlobalStringPtr(error.str(), "stackoverflowError");
        builder->CreateCall(rtStackOverflowError, {errorMessage});
        builder->CreateBr(noStackOverflow);
        appendAndSetInsertTo(noStackOverflow);
        auto newStackSize = builder->CreateAdd(currentStackSize, getInt(32, 1));
        builder->CreateStore(newStackSize, stackSize);

        auto ret = (*this)(function->definition->body);
        if (!isNever(ret)) {
            ret.type = with(
                *blockTypeArguments, *typeArguments,
                function->definition->returnType
            );
            auto currentStackSize = builder->CreateLoad(type::integer.asLLVM, stackSize);
            auto newStackSize = builder->CreateSub(currentStackSize, getInt(32, 1));
            builder->CreateStore(newStackSize, stackSize);
            builder->CreateRet(copy(ret));
        }
        cleanupStack.clear();
        if (llvm::verifyFunction(*function->asLLVM, &llvm::outs())) {
            std::cout << "error found" << std::endl;
            throw "error found";
        }
    }

    void operator()(TypeMonomorph* type) {}

    void emitMain(ast::FunctionDeclaration* mainFunction) {
        auto global = currentModule->getOrInsertGlobal("stacksize", type::integer.asLLVM);
        stackSize = currentModule->getNamedGlobal("stacksize");
        stackSize->setInitializer(llvm::ConstantInt::get(*context, llvm::APInt(32, 0)));
        stackSize->setLinkage(llvm::GlobalValue::PrivateLinkage);
        currentFunction = mainFunction;
        functions["rtAlloc"] = createSignature(allocFunc, "rtAlloc");
        functions["rtMove"] = createSignature(moveFunc, "rtMove");
        functions["rtSlice"] = createSignature(sliceFunc, "rtSlice");
        functions["rtFree"] = createSignature(freeFunc, "rtFree");

        rtStackOverflowError = llvm::Function::Create(
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(*context),
                {
                    llvm::PointerType::get(*context, 0),
                },
                false
            ),
            llvm::Function::ExternalLinkage, "rtStackOverflowError", currentModule.get()
        );
        rtOobError = llvm::Function::Create(
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(*context),
                {
                    llvm::PointerType::get(*context, 0),
                    llvm::Type::getInt32Ty(*context),
                    llvm::Type::getInt32Ty(*context),
                },
                false
            ),
            llvm::Function::ExternalLinkage, "rtOobError", currentModule.get()
        );
                rtSubUnderflowError = llvm::Function::Create(
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(*context),
                {
                    llvm::PointerType::get(*context, 0),
                    llvm::Type::getInt32Ty(*context),
                    llvm::Type::getInt32Ty(*context),
                },
                false
            ),
            llvm::Function::ExternalLinkage, "rtSubUnderflowError", currentModule.get()
        );
                rtAddOverflowError = llvm::Function::Create(
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(*context),
                {
                    llvm::PointerType::get(*context, 0),
                    llvm::Type::getInt32Ty(*context),
                    llvm::Type::getInt32Ty(*context),
                },
                false
            ),
            llvm::Function::ExternalLinkage, "rtAddOverflowError", currentModule.get()
        );
                rtMulOverflowError = llvm::Function::Create(
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(*context),
                {
                    llvm::PointerType::get(*context, 0),
                    llvm::Type::getInt32Ty(*context),
                    llvm::Type::getInt32Ty(*context),
                },
                false
            ),
            llvm::Function::ExternalLinkage, "rtMulOverflowError", currentModule.get()
        );
        rtZeroDivError = llvm::Function::Create(
            llvm::FunctionType::get(
                llvm::Type::getVoidTy(*context),
                {
                    llvm::PointerType::get(*context, 0),
                },
                false
            ),
            llvm::Function::ExternalLinkage, "rtZeroDivError", currentModule.get()
        );


        auto llvmFunc = llvm::Function::Create(
            llvm::FunctionType::get(llvm::Type::getInt32Ty(*context), false),
            llvm::Function::ExternalLinkage, "main", currentModule.get()
        );
        auto mainBlock = createBlock("entry");
        llvmFunc->insert(llvmFunc->end(), mainBlock);
        builder->SetInsertPoint(mainBlock);
        auto result = (*this)(mainFunction->body);
        if (!isNever(result)) {
            builder->CreateRet(result);
        }
        if (llvm::verifyFunction(*llvmFunc, &llvm::outs())) {
            std::cout << "error found" << std::endl;
            throw "error found";
        }
        while (signatureStack.size() > 0) {
            auto func = signatureStack.back();
            signatureStack.pop_back();
            currentFunction = func->definition;
            (*this)(func);
        }
    }

    std::string nameOf(Type const& type) {
        return std::visit(
            [this](auto const& type) { return nameOf(type); }, type
        );
    }

    std::string nameOf(std::vector<Type> const& named) {
        if (named.size() == 0) {
            return "";
        }

        std::string name{"<"};
        for (auto& type : named) {
            name.append(nameOf(type));
            name.append(",");
        }
        name.append(">");
        return std::move(name);
    }

    std::string nameOf(type::Named const& named) {
        std::string mono{named.declaration->fullName};
        mono.append(nameOf(named.typeArguments));
        return std::move(mono);
    }

    std::string nameOf(type::Struct const& structure) {
        std::string name{"{"};
        for (auto& [property, type] : structure.properties) {
            name.append(property);
            name.append(":");
            name.append(nameOf(type));
            name.append(",");
        }
        name.append("}");
        return std::move(name);
    }

    std::string nameOf(type::Tuple const& tuple) {
        std::string name{"("};
        for (auto& type : tuple.fields) {
            name.append(nameOf(type));
            name.append(",");
        }
        name.append(")");
        return std::move(name);
    }

    std::string nameOf(type::Variant const& structure) {
        std::string name{"{"};
        for (auto& [property, type] : structure.variants) {
            name.append(property);
            name.append(":");
            name.append(nameOf(type));
            name.append("|");
        }
        name.append("}");
        return std::move(name);
    }

    std::string nameOf(type::Union const& tuple) {
        std::string name{"("};
        for (auto& type : tuple.elements) {
            name.append(nameOf(type));
            name.append("|");
        }
        name.append(")");
        return std::move(name);
    }

    std::string nameOf(type::BuiltIn* const& builtIn) {
        return std::string{builtIn->name};
    }

    std::string nameOf(type::Parameter const& parameter) {
        return nameOf(
            parameter.isBlockParameter ? (*blockTypeArguments)[parameter.index]
                                       : (*typeArguments)[parameter.index]
        );
    }
    
    llvm::Type* asLLVM(type::Named const& type) {
        auto name = nameOf(type);
        auto& monomorph = namedTypes[name];
        if (!monomorph) {
            auto resT = with(type.typeArguments, {}, type.declaration->proto);
            auto fieldT = asLLVM(std::move(resT));
            auto asLLVMStruct = llvm::StructType::create(
                *context, {fieldT}, name, false
            );
            monomorph = new TypeMonomorph{asLLVMStruct};
        }
        return monomorph->asLLVM;
    }

    llvm::Type* asLLVM(type::BuiltIn* const& type) {
        return type->asLLVM;
    }

    llvm::Type* asLLVM(type::Tuple const& type) {
        std::vector<llvm::Type*> fields;
        for (auto& field : type.fields) {
            auto fieldType = asLLVM(field);
            // fieldType.asLLVM
            fields.push_back(fieldType);
        }
        return llvm::StructType::get(*context, fields, false);
    }

    llvm::Type* asLLVM(type::Struct const& type) {
        std::vector<llvm::Type*> fields;
        for (auto& [_, property] : type.properties) {
            fields.push_back(asLLVM(property));
        }
        return llvm::StructType::get(*context, fields, false);
    }

    llvm::Type* asLLVM(type::Union const& type) {
        size_t maxSize = 0;
        for (auto& field : type.elements) {
            auto size = currentModule->getDataLayout().getTypeAllocSize(asLLVM(field));
            maxSize = size > maxSize ? size : maxSize;
        }
        size_t blocks = maxSize / 8;
        if (maxSize % 8 != 0) ++blocks;
        std::vector<llvm::Type*> fields{llvm::Type::getInt64Ty(*context), llvm::ArrayType::get(llvm::Type::getInt64Ty(*context), blocks)};
        return llvm::StructType::get(*context, fields, false);
    }

    llvm::Type* asLLVM(type::Variant const& type) {
        size_t maxSize = 0;
        for (auto& [_, property] : type.variants) {
            auto size = currentModule->getDataLayout().getTypeAllocSize(asLLVM(property));
            maxSize = size > maxSize ? size : maxSize;
        }
        size_t blocks = maxSize / 8;
        if (maxSize % 8 != 0) ++blocks;
        std::vector<llvm::Type*> fields{llvm::Type::getInt64Ty(*context), llvm::ArrayType::get(llvm::Type::getInt64Ty(*context), blocks)};
        return llvm::StructType::get(*context, fields, false);
    }

    llvm::Type* asLLVM(type::Parameter const& type) {
        return asLLVM(
            type.isBlockParameter ? (*blockTypeArguments)[type.index]
                                  : (*typeArguments)[type.index]
        );
    }

    llvm::Type* asLLVM(Type const& type) {
        return std::visit([&](auto const& type) { return asLLVM(type); }, type);
    }

    void copyInner(
        Value* value, Type const& type, std::vector<unsigned int>& indices
    ) {
        std::visit(
            match{
                [&](type::Named const& named) {
                    indices.push_back(0);
                    copy(
                        value,
                        with(named.typeArguments, {}, named.declaration->proto),
                        indices
                    );
                    indices.pop_back();
                },
                [&](type::Struct const& structure) {
                    indices.push_back(0);
                    for (size_t i = 0; i < structure.properties.size(); ++i) {
                        indices.back() = i;
                        copy(value, structure.properties[i].second, indices);
                    }
                    indices.pop_back();
                },
                [&](type::Tuple const& tuple) {
                    indices.push_back(0);
                    for (size_t i = 0; i < tuple.fields.size(); ++i) {
                        indices.back() = i;
                        copy(value, tuple.fields[i], indices);
                    }
                    indices.pop_back();
                },
                [](type::Parameter&) {
                    fmt::println("this is rly bad cpy");
                    throw "";
                },
                [](auto const&) {},
            },
            type
        );
    }

    void dropInner(
        llvm::Value* value, Type const& type, std::vector<unsigned int>& indices
    ) {
        std::visit(
            match{
                [&](type::Named const& named) {
                    indices.push_back(0);
                    drop(
                        value,
                        with(named.typeArguments, {}, named.declaration->proto),
                        indices
                    );
                    indices.pop_back();
                },
                [&](type::Struct const& structure) {
                    indices.push_back(0);
                    for (size_t i = 0; i < structure.properties.size(); ++i) {
                        indices.back() = i;
                        drop(value, structure.properties[i].second, indices);
                    }
                    indices.pop_back();
                },
                [&](type::Tuple const& tuple) {
                    indices.push_back(0);
                    for (size_t i = 0; i < tuple.fields.size(); ++i) {
                        indices.back() = i;
                        drop(value, tuple.fields[i], indices);
                    }
                    indices.pop_back();
                },
                [&](type::Variant const& variant) {
                    indices.push_back(0);
                    auto discriminant = builder->CreateExtractValue(value, indices);
                    indices.back() = 1;
                    auto body = builder->CreateExtractValue(value, indices);
                    indices.pop_back();
                    auto afterSwitch = createBlock("afterswitch");
                    llvm::SwitchInst * _switch = builder->CreateSwitch(discriminant, afterSwitch);
                    for (auto&& [i, element] : variant.variants | views::enumerate) {
                        auto block = createBlock("elem");
                        _switch->addCase(llvm::ConstantInt::get(*context, llvm::APInt(64, i)), block);
                        appendAndSetInsertTo(block);
                        auto elemType = asLLVM(element.second);
                        auto alloc = createEntryBlockAlloca(body->getType());
                        builder->CreateStore(body, alloc);
                        auto elementValue = builder->CreateLoad(elemType, alloc);
                        drop({elementValue, {}, element.second});
                        builder->CreateBr(afterSwitch);
                    }
                    appendAndSetInsertTo(afterSwitch);
                },
                [&](type::Union const& _union) {
                    indices.push_back(0);
                    auto discriminant = builder->CreateExtractValue(value, indices);
                    indices.back() = 1;
                    auto body = builder->CreateExtractValue(value, indices);
                    indices.pop_back();
                    auto afterSwitch = createBlock("afterswitch");
                    llvm::SwitchInst * _switch = builder->CreateSwitch(discriminant, afterSwitch);
                    for (auto&& [i, element] : _union.elements | views::enumerate) {
                        auto block = createBlock("elem");
                        _switch->addCase(llvm::ConstantInt::get(*context, llvm::APInt(64, i)), block);
                        appendAndSetInsertTo(block);
                        auto elemType = asLLVM(element);
                        auto alloc = createEntryBlockAlloca(body->getType());
                        builder->CreateStore(body, alloc);
                        auto elementValue = builder->CreateLoad(elemType, alloc);
                        drop({elementValue, {}, element});
                        builder->CreateBr(afterSwitch);
                    }
                    appendAndSetInsertTo(afterSwitch);
                },
                [](type::Parameter&) {
                    fmt::println("this is rly bad");
                    throw "";
                },
                [](auto const&) {},
            },
            type
        );
    }

    void
    copy(Value* value, Type const& type, std::vector<unsigned int>& indices) {
        if (auto copyImpl =
                implScope.tryFind(type, Trait{copyDeclaration, {}})) {
            std::vector<Value> args;

            value->asLLVM = builder->CreateInsertValue(
                *value,
                callTraitMethod(
                    {builder->CreateExtractValue(*value, indices), value->owner,
                     type},
                    0, {}, copyImpl.value(), args
                ),
                indices
            );
            return;
        }
        copyInner(value, type, indices);
    }

    Value copy(Value value) {
        auto resolvedType = 
            with(*blockTypeArguments, *typeArguments, value.type);
        if (value.owner.index() == 0) {
            return value;
        }

        if (auto copyImpl =
                implScope.tryFind(resolvedType, Trait{copyDeclaration, {}})) {
            std::vector<Value> args;

            return {
                callTraitMethod(value, 0, {}, copyImpl.value(), args),
                {},
                resolvedType,
            };
        }

        std::vector<unsigned int> indices{};
        copyInner(&value, value.type, indices);
        return {value.asLLVM, {}, value.type};
    }

    void drop(
        llvm::Value* value, Type const& type, std::vector<unsigned int>& indices
    ) {
        if (auto dropImpl =
                implScope.tryFind(type, Trait{dropDeclaration, {}})) {
            std::vector<Value> args;
            auto alloca = createEntryBlockAlloca(asLLVM(type));
            builder->CreateStore(builder->CreateExtractValue(value, indices), alloca);
            callTraitMethod(
                {alloca, 0, type}, 0, {},
                dropImpl.value(), args
            );
            return;
        }
        dropInner(value, type, indices);
    }

    bool isDrop(Type const& type) {
        return implScope.tryFind(type, Trait{dropDeclaration, {}}).has_value();
    }

    void drop(Value value) {
        if (!value.asLLVM)
            return;
        auto resolvedType = 
            with(*blockTypeArguments, *typeArguments, value.type);
        if (auto dropImpl =
                implScope.tryFind(resolvedType, Trait{dropDeclaration, {}})) {
            std::vector<Value> args;
            auto alloca = createEntryBlockAlloca(asLLVM(resolvedType));
            builder->CreateStore(value, alloca);
            value.asLLVM = alloca;
            callTraitMethod(
                {value, 0, resolvedType}, 0, {}, dropImpl.value(), args
            );
        }

        std::vector<unsigned int> indices{};
        dropInner(value, resolvedType, indices);
    }

    Value take(ast::Expression& expression) {
        return std::visit(
            match{
                [this](auto& value) { return (*this)(std::move(value)); },
                [this](ast::Variable& var) {
                    auto value = (*this)(std::move(var));
                    if (value.owner.index() == 1 && var.mayMove) {
                        moves.push_back(std::get<size_t>(value.owner));
                        value.owner = std::monostate{};
                    }
                    return value;
                },
            },
            expression.value
        );
    }

    Lifetime beginLifetime() {
        return Lifetime{*this};
    }
};

class Compiler {
    IRVisitor ir;
    // TypeCheck
};

std::optional<std::tuple<std::unique_ptr<llvm::LLVMContext>, std::unique_ptr<llvm::Module>, llvm::TargetMachine*>> compile(std::vector<Module*>& modules) {
    auto ctx = std::make_unique<llvm::LLVMContext>();
    auto mod = std::make_unique<llvm::Module>("my cool jit", *ctx);
    auto builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

    type::integer.asLLVM = llvm::Type::getInt32Ty(*ctx);
    type::character.asLLVM = llvm::Type::getInt8Ty(*ctx);
    type::null.asLLVM = llvm::StructType::create(*ctx, {}, "null");
    type::boolean.asLLVM = llvm::Type::getInt1Ty(*ctx);
    type::ptr.asLLVM = llvm::PointerType::get(*ctx, 0);
    type::floating.asLLVM = llvm::Type::getDoubleTy(*ctx);

    std::unordered_map<std::string, type::BuiltIn*> builtInTypes{
        {"int", &type::integer},
        {"null", &type::null},
        {"bool", &type::boolean},
        {"ptr", &type::ptr},
        {"char", &type::character},
        {"float", &type::floating},
    };
    
    TypeChecker tc{
        builtInTypes,
    };

    tc.check(modules);

    if (!tc.log.errorsAreEmpty()) {
        tc.log.printDiagnosticsTo(std::cerr);
        return {};
    }

    tc.implementationScope.currentBounds.block = nullptr;
    tc.implementationScope.currentBounds.local = nullptr;
    IRVisitor ir{
        std::move(ctx), std::move(builder),
        std::move(mod), builtInTypes,   std::move(tc.implementationScope)};

    ir.vecDeclaration = tc.typeScope.at("std::Vector");
    ir.stringDeclaration = tc.typeScope.at("std::String");
    ir.allocFunc = Function{tc.funcScope.at("std::rtAlloc"), {}};
    ir.sliceFunc = Function{tc.funcScope.at("std::rtSlice"), {}};
    ir.freeFunc = Function{tc.funcScope.at("std::rtFree"), {}};
    ir.moveFunc = Function{tc.funcScope.at("std::rtMove"), {}};
    ir.copyDeclaration = tc.traitScope.at("std::Copy");
    ir.dropDeclaration = tc.traitScope.at("std::Drop");
    ir.modules = &modules;

    auto targetTriple = llvm::sys::getDefaultTargetTriple();

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string targetErr;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, targetErr);
    if (!target) {
        std::cerr << targetErr;
        return {};
    }

    auto targetMachine =
        target->createTargetMachine(targetTriple, "generic", "", {}, {});

    ir.currentModule->setDataLayout(targetMachine->createDataLayout());
    ir.currentModule->setTargetTriple(targetTriple);

    auto mainBody =
        tc.funcScope.find(modules[1]->moduleId + "::main");
    if (mainBody == tc.funcScope.end()) {
        std::cout << "main undefined" << std::endl;
        throw "";
    }
    ir.emitMain(std::move(mainBody->second));

    return {{std::move(ir.context), std::move(ir.currentModule), targetMachine}};
}
