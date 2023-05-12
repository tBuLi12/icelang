#include "../semanticAnalyzer/typeChecker.cpp"

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

struct ImplScope : std::vector<ast::TraitImplementation*> {
    std::optional<ImplRef> tryFind(Type const& type, Trait const& trait) {
        for (auto implementation : *this) {
            // std::cout << "trying codegen "
            //           << implementation->trait.declaration->name.value
            //           << std::endl;
            // std::cout << "trying codegen " << implementation->type <<
            // std::endl; std::cout << "trying codegen " <<
            // trait.declaration->name.value
            //           << std::endl;
            // std::cout << "trying codegen " << type << std::endl;

            if (auto found = matches(*implementation, trait, type)) {
                std::cout << "impl found" << type << std::endl;
                return {{implementation, std::move(found.value())}};
            }
        }
        return {};
    }

    ImplRef find(Type const& type, Trait const& trait) {
        auto impl = tryFind(type, trait);
        if (impl) {
            return std::move(impl.value());
        }

        std::cout << "FATAL: impl not found" << std::endl;
        throw "impl not found";
    }

    bool areSatisfied(std::vector<std::pair<Type, Trait>> const& traitBounds) {
        for (auto& [type, trait] : traitBounds) {
            bool found = false;
            for (auto implementation : *this) {
                if ((found =
                         matches(*implementation, trait, type).has_value())) {
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }

    std::optional<std::vector<Type>> matches(
        ast::TraitImplementation const& implemetation, Trait const& trait,
        Type const& type
    ) {
        std::cout << "impl found1" << type << " "
                  << trait.declaration->name.value << std::endl;
        if (implemetation.trait.declaration != trait.declaration) {
            return {};
        }

        std::vector<Type> inferredTypeArguments{};
        inferredTypeArguments.resize(implemetation.typeParameterNames.size());

        std::cout << "impl found2" << type << " cnt "
                  << implemetation.trait.typeArguments.size() << std::endl;
        for (size_t i = 0; i < implemetation.trait.typeArguments.size(); ++i) {
            if (!tryMatch(
                    inferredTypeArguments, implemetation.trait.typeArguments[i],
                    trait.typeArguments[i]
                )) {
                return {};
            }
        }

        std::cout << "impl found3" << type << std::endl;
        if (!tryMatch(inferredTypeArguments, implemetation.type, type)) {
            return {};
        }

        std::cout << "impl found4" << type << std::endl;
        return areSatisfied(
                   with(inferredTypeArguments, {}, implemetation.traitBounds)
               )
                   ? std::optional<std::vector<Type>>{std::move(
                         inferredTypeArguments
                     )}
                   : std::optional<std::vector<Type>>{};
    }
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
    Source source;

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::unique_ptr<llvm::Module> currentModule;
    std::unordered_map<std::string, type::BuiltIn*>& builtInTypes;
    ImplScope implScope;

    std::vector<Type> const* typeArguments;
    std::vector<Type> const* blockTypeArguments;

    std::vector<FunctionMonomorph*> signatureStack{};

    std::unordered_map<std::string, TypeMonomorph*> namedTypes{};
    std::unordered_map<std::string, FunctionMonomorph*> functions{};

    Function allocFunc;
    Function freeFunc;
    Function sliceFunc;

    ast::TypeDeclaration* vecDeclaration;
    ast::TraitDeclaration* copyDeclaration;
    ast::TraitDeclaration* dropDeclaration;
    llvm::Function* rtOobError;

    std::vector<Value> temporaryCleanup{};
    std::vector<ast::Binding> cleanupStack{};

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
            std::cout << "LIFETIME START" << std::endl;
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
            fmt::println(
                "MANUAL END {} {}", ir.temporaryCleanup.size(), temporarySize
            );
            for (auto& t : ir.temporaryCleanup) {
                fmt::println(
                    "with t {} {}", t.type, reinterpret_cast<void*>(t.asLLVM)
                );
            }
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
                if (drop.value) {
                    auto value = ir.builder->CreateLoad(
                        ir.asLLVM(drop.type), drop.value
                    );
                    ir.drop({value, {}, drop.type});
                }
            }
        }

        ~Lifetime() {
            while (ir.temporaryCleanup.size() > temporarySize) {
                auto value = std::move(ir.temporaryCleanup.back());
                ir.temporaryCleanup.pop_back();
                value.owner = std::monostate{};
                if (value.asLLVM) {
                    ir.drop(value);
                }
            }

            while (ir.cleanupStack.size() > cleanupSize) {
                auto drop = std::move(ir.cleanupStack.back());
                ir.cleanupStack.pop_back();
                if (drop.value) {
                    auto value = ir.builder->CreateLoad(
                        ir.asLLVM(drop.type), drop.value
                    );
                    ir.drop({value, {}, drop.type});
                }
            }
        }
    };

    bool isNever(Value value) {
        return !value.asLLVM;
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
                std::cout << "match" << std::endl;
                auto lifetime = beginLifetime();
                BindPattern{
                    *this, noMatch, &lifetime}(matchCase.pattern, scrutinee);
                result = std::visit(*this, std::move(matchCase.value.value));
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

    Value operator()(ast::Break&&) {
        fmt::println("unsupported");
        throw "";
    }

    Value operator()(ast::Return&&) {
        fmt::println("unsupported");
        throw "";
    }

    Value operator()(ast::Continue&&) {
        fmt::println("unsupported");
        throw "";
    }

    Value operator()(lexer::StringLiteral&&) {
        fmt::println("unsupported");
        throw "";
    }

    Value operator()(ast::IndexAccess&& access) {
        fmt::println("into index access");
        auto lhs = (*this)(*access.lhs);
        auto index = (*this)(*access.index);
        if (isNever(lhs) || isNever(index))
            return never;

        llvm::Type* elementType = asLLVM(access.elementType);

        llvm::Value* vecLen = builder->CreateExtractValue(lhs, {0, 2});
        auto isOutOfBounds = builder->CreateICmpSGE(index, vecLen);

        auto outOfBounds = createBlock("outofbounds");
        auto inBounds = createBlock("inbounds");

        builder->CreateCondBr(isOutOfBounds, outOfBounds, inBounds);
        appendAndSetInsertTo(outOfBounds);
        std::stringstream error{};
        logs::SpannedMessage msg{source, access.span, "", ""};
        std::cout << msg << std::endl;
        msg.printBodyTo(error);
        auto errorMessage =
            builder->CreateGlobalStringPtr(error.str(), "oobError");
        builder->CreateCall(rtOobError, {errorMessage, index, vecLen});
        builder->CreateBr(inBounds);

        appendAndSetInsertTo(inBounds);
        llvm::Value* bufPtr = builder->CreateExtractValue(lhs, {0, 0});
        llvm::Value* elementPtr =
            builder->CreateInBoundsGEP(elementType, bufPtr, {index});

        llvm::Value* value = builder->CreateLoad(elementType, elementPtr);

        if (lhs.owner.index() == 0) {
            value = copy(Value{value, 0, access.elementType});
            drop(lhs);
        }

        return Value{value, lhs.owner, access.type};
    }

    Value operator()(ast::VectorLiteral&& vector) {
        auto elements = std::get_if<ast::VectorElements>(&vector.content);
        size_t capacity = elements ? elements->elements.size() : 0;
        llvm::Type* elemType = asLLVM(vector.elementType);
        llvm::Value* cap = getInt(
            32,
            capacity * currentModule->getDataLayout().getTypeAllocSize(elemType)
        );
        llvm::Value* ptr =
            builder->CreateCall(functions["rtAlloc"]->asLLVM, {cap});

        for (size_t i = 0; i < capacity; ++i) {
            llvm::Value* element =
                builder->CreateInBoundsGEP(elemType, ptr, {getInt(32, i)});
            auto value = (*this)(std::move(elements->elements[i]));
            builder->CreateStore(copy(value), element);
        }
        llvm::Value* literal = llvm::UndefValue::get(asLLVM(type::Named{
            vecDeclaration, {Type{vector.elementType}}}));

        cap = getInt(32, static_cast<unsigned int>(capacity));

        literal = builder->CreateInsertValue(literal, ptr, {0, 0});
        literal = builder->CreateInsertValue(literal, cap, {0, 1});
        literal = builder->CreateInsertValue(literal, cap, {0, 2});
        fmt::println("will print type");
        std::cout << vector.type << std::endl;
        auto val = Value{literal, {}, vector.type};
        fmt::println("in literal {}", val.owner.index());
        return val;
    }

    Value operator()(ast::VectorElement&& element) {
        return std::visit(std::move(*this), std::move(element.value));
    }

    Value operator()(ast::Spread&& spread) {
        fmt::println("unsupported");
        throw "no";
    }

    Value operator()(ast::Expression&& expression) {
        return std::visit(std::move(*this), std::move(expression.value));
    }

    Value operator()(ast::Expression& expression) {
        fmt::println("visiting it");
        auto val = std::visit(std::move(*this), std::move(expression.value));
        fmt::println("after visit");
        return val;
    }

    template <String op> Value operator()(ast::Binary<op>&& binary) {
        Value lhs = (*this)(*binary.lhs);
        Value rhs = (*this)(*binary.rhs);
        if (isNever(lhs) || isNever(rhs))
            return never;
        return (*this)(std::move(binary), lhs, rhs);
    }

    Value operator()(ast::Binary<"+">&& binary) {
        fmt::println("mkaing addition");
        Value lhs = (*this)(*binary.lhs);
        fmt::println("lhs done");
        Value rhs = (*this)(*binary.rhs);
        if (isNever(lhs) || isNever(rhs))
            return never;
        return (*this)(std::move(binary), lhs, rhs);
    }

    template <String op>
    Value operator()(ast::Binary<op>&& addition, Value lhs, Value rhs) {
        fmt::println("unsupported");
        throw "";
    }
    Value operator()(ast::Binary<"+">&& addition, Value lhs, Value rhs) {
        if (lhs->getType()->isIntegerTy() && lhs->getType()->isIntegerTy()) {
            return Value{builder->CreateAdd(lhs, rhs), {}, addition.type};
        }
        return Value{builder->CreateFAdd(lhs, rhs), {}, addition.type};
    }

    Value operator()(ast::Binary<"==">&& equate, Value lhs, Value rhs) {
        fmt::println("time for eq");
        return Value{builder->CreateICmpEQ(lhs, rhs), {}, equate.type};
    }

    Value operator()(ast::Binary<"!=">&& notEquals, Value lhs, Value rhs) {
        return Value{builder->CreateICmpNE(lhs, rhs), {}, notEquals.type};
    }

    Value operator()(ast::Binary<"*">&& multiplication, Value lhs, Value rhs) {
        return Value{builder->CreateMul(lhs, rhs), {}, multiplication.type};
    }

    Value operator()(ast::Condition&& condition) {
        fmt::println("cond gen");

        auto& expr = std::get<ast::Expression>(condition.value);
        fmt::println("gotexpr");

        return (*this)(std::move(expr));
    }

    Value getVarPointer(auto&&, std::vector<llvm::Value*>& indices) {
        std::cout << "FATAL: invalid write" << std::endl;
        throw "FATAL: invalid write";
    }

    Value getVarPointer(
        ast::PropertyAccess&& access, std::vector<llvm::Value*>& indices
    ) {
        std::cout << "IN HERE" << std::endl;
        auto ptr = getVarPointer(std::move(*access.lhs), indices);
        while (access.namedDepth) {
            --access.namedDepth;
            indices.push_back(getInt(32, 0));
        }
        indices.push_back(getInt(32, access.propertyIdx));
        return ptr;
    }

    Value getVarPointer(
        ast::IndexAccess&& access, std::vector<llvm::Value*>& indices
    ) {
        Value vec = (*this)(std::move(*access.lhs));
        llvm::Type* elemType = asLLVM(access.elementType);
        vec.asLLVM = builder->CreateExtractValue(vec, {0, 0});
        vec.asLLVM =
            builder->CreateInBoundsGEP(elemType, vec, {(*this)(*access.index)});
        return vec;
    }

    Value getVarPointer(
        ast::TupleFieldAccess&& access, std::vector<llvm::Value*>& indices
    ) {
        auto ptr = getVarPointer(*access.lhs, indices);
        while (access.namedDepth) {
            --access.namedDepth;
            indices.push_back(getInt(32, 0));
        }
        indices.push_back(getInt(32, access.propertyIdx));
        return ptr;
    }

    Value
    getVarPointer(ast::Variable&& var, std::vector<llvm::Value*>& indices) {
        // if (!cleanupStack[var.binding].isMutable) {
        //     std::cout << "assignment to immutable" << std::endl;
        //     throw "";
        // }

        return {
            cleanupStack[var.binding].value, var.binding,
            cleanupStack[var.binding].type};
    }

    Value getVarPointer(
        ast::Expression&& expression, std::vector<llvm::Value*>& indices
    ) {
        std::cout << "calling getVarPointer on ";
        fmt::println("{}", expression);
        std::cout << std::endl;

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
        auto uncopied = (*this)(*assignment.rhs);
        if (isNever(uncopied))
            return never;
        auto value = copy(uncopied);
        std::vector<llvm::Value*> indices{};
        std::cout << "Processing assignment" << std::endl;

        indices.push_back(getInt(32, 0));
        auto varPtr = getVarPointer(std::move(*assignment.lhs), indices);

        auto type = asLLVM(value.type);
        auto target = indices.size() > 1 ? gep(varPtr, indices) : varPtr;
        drop({builder->CreateLoad(type, target), {}, value.type});
        builder->CreateStore(value, target);
        return Value{value, varPtr.owner, assignment.type};
    }

    Value operator()(ast::If&& ifExpr) {
        fmt::println("into if");
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

        auto trueVal = (*this)(*ifExpr.trueBranch);
        if (!ifExpr.hasSameTypeBranch) {
            if (trueVal.owner.index() == 0)
                drop(trueVal);
        }
        builder->CreateBr(afterBlock);
        ifBlock = builder->GetInsertBlock();
        fmt::println("into if 2");

        bool resultIsOwned = true;

        Value falseVal;
        if (ifExpr.falseBranch) {
            fmt::println("into if 3");

            appendAndSetInsertTo(elseBlock);
            falseVal = (*this)(**ifExpr.falseBranch);

            if (ifExpr.hasSameTypeBranch) {
                falseVal = copy(falseVal);
                trueVal = copy(trueVal);
            } else {
                if (falseVal.owner.index() == 0)
                    drop(falseVal);
            }

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
                fmt::println("into if 7");
                auto phi = builder->CreatePHI(trueVal->getType(), 2);
                phi->addIncoming(trueVal, ifBlock);
                phi->addIncoming(falseVal, elseBlock);
                return Value{phi, {}, ifExpr.type};
            }
        }

        fmt::println("after if");

        return null();
    }

    Value operator()(ast::While&& loop) {
        fmt::println("into while");

        auto function = builder->GetInsertBlock()->getParent();
        auto conditionBlock = createBlock("loopcondition");
        auto loopBlock = createBlock("loopbody");
        auto afterBlock = createBlock("afterwhile");

        builder->CreateBr(conditionBlock);
        appendAndSetInsertTo(conditionBlock);
        Value condval = (*this)(std::move(*loop.condition));
        if (isNever(condval))
            return never;

        fmt::println("after valgen");

        builder->CreateCondBr(condval, loopBlock, afterBlock);
        fmt::println("after cond");

        appendAndSetInsertTo(loopBlock);
        auto bodyValue = (*this)(*loop.body);
        if (bodyValue.owner.index() == 0)
            drop(bodyValue);
        loopBlock = builder->GetInsertBlock();
        builder->CreateBr(conditionBlock);

        appendAndSetInsertTo(afterBlock);

        fmt::println("while done");
        return null();
    }

    Value operator()(ast::TypeExpression&& typeExpression) {
        auto type = asLLVM(typeExpression.theType);
        fmt::println("b4 int");

        fmt::println("b4 get");
        llvm::Value* size =
            getInt(32, currentModule->getDataLayout().getTypeAllocSize(type));
        fmt::println("b4 undef");
        auto literal = llvm::UndefValue::get(llvm::StructType::get(
            *context,
            llvm::ArrayRef{static_cast<llvm::Type*>(
                llvm::StructType::get(*context, llvm::ArrayRef{size->getType()})
            )}
        ));
        fmt::println("b4 create");
        auto val = builder->CreateInsertValue(literal, size, {0});
        fmt::println("got {}", reinterpret_cast<void*>(val));
        return {val, {}, typeExpression.type};
    }

    Value operator()(ast::Variable&& variable) {
        if (variable.binding <
            builder->GetInsertBlock()->getParent()->arg_size()) {
            return {
                cleanupStack[variable.binding].value, variable.binding,
                cleanupStack[variable.binding].type};
        }

        std::cout << cleanupStack.size() << " generating value from var "
                  << variable.binding << " " << variable.name << " "
                  << cleanupStack[variable.binding].type << std::endl;
        return {
            builder->CreateLoad(
                asLLVM(cleanupStack[variable.binding].type),
                cleanupStack[variable.binding].value
            ),
            variable.binding, cleanupStack[variable.binding].type};
    }

    Value operator()(ast::Block&& block) {
        std::cout << "block" << std::endl;
        auto lifetime = beginLifetime();
        auto last = block.hasTrailingExpression ? --block.items.end()
                                                : block.items.end();
        for (auto item = block.items.begin(); item != last; ++item) {
            if (isNever((*this)(*item))) {
                return never;
            }
        }
        if (block.hasTrailingExpression) {
            auto value = (*this)(block.items.back(), false);
            std::cout << "includes?" << std::endl;
            if (lifetime.includes(value)) {
                std::cout << "yep" << std::endl;
                value = copy(value);
            }
            std::cout << "no" << std::endl;
            return value;
        }
        return null();
    }

    Value operator()(ast::BlockItem& item, bool dropResult = true) {
        auto value = dropResult ? std::visit(*this, std::move(item.value))
                                : take(std::get<ast::Expression>(item.value));
        ast::Expression* expression = std::get_if<ast::Expression>(&item.value);
        if (expression && dropResult) {
            if (value.owner.index() == 0)
                drop(value);
        }
        return value;
    }

    Value operator()(lexer::IntegerLiteral&& integer) {
        std::cout << "returning integer" << std::endl;
        return {getInt(32, integer.value), {}, builtInTypes["int"]};
    }

    Value operator()(ast::TupleLiteral&& tuple) {
        std::vector<llvm::Value*> fieldValues{};
        for (auto& field : tuple.fields) {
            auto value = (*this)(field);
            fieldValues.push_back(copy(value));
        }

        llvm::Value* literal = llvm::UndefValue::get(asLLVM(tuple.type));
        for (unsigned int i = 0; i < tuple.fields.size(); ++i) {
            literal = builder->CreateInsertValue(literal, fieldValues[i], {i});
        }

        return {literal, {}, tuple.type};
    }

    Value operator()(ast::StructLiteral&& structure) {
        std::vector<llvm::Value*> fieldValues{};
        std::vector<std::pair<std::string*, llvm::Type*>> fieldTypesWithNames{};
        for (auto& [_, name, expression] : structure.properties) {
            auto value = (*this)(std::move(expression.value()));
            fieldTypesWithNames.push_back({&name, value->getType()});
            fieldValues.push_back(copy(value));
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
                builder->CreateInsertValue(literal, fieldValues[i], {fieldIdx});
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

    FunctionMonomorph* createSignature(Function const& function) {
        auto monomorphName = nameOf(function.typeArguments);
        std::vector<llvm::Type*> llvmParamTypes{};
        for (auto& arg : function.declaration->parameters) {
            llvmParamTypes.push_back(
                asLLVM(with({}, function.typeArguments, arg.type))
            );
        }
        auto resolved =
            with({}, function.typeArguments, function.declaration->returnType);
        auto returnTypeAsLLVM = asLLVM(resolved);
        auto funcType =
            llvm::FunctionType::get(returnTypeAsLLVM, llvmParamTypes, false);
        auto llvmFunc = llvm::Function::Create(
            funcType, llvm::Function::ExternalLinkage,
            function.declaration->name.value + monomorphName,
            currentModule.get()
        );
        size_t i = 0;
        for (auto& arg : llvmFunc->args()) {
            arg.setName(function.declaration->parameters[i].name.value);
            ++i;
        }

        auto func = new FunctionMonomorph{
            llvmFunc,
            function.declaration,
            with(*blockTypeArguments, *typeArguments, function.typeArguments),
        };
        return func;
    }

    llvm::Value* callTraitMethod(
        Value thisValue, size_t methodIdx, TraitMethodRef const& method,
        ImplRef const& impl, std::vector<Value>& args
    ) {
        ast::FunctionDeclaration& target =
            impl.declaration->implementations[methodIdx];

        auto name =
            nameOf(with(impl.typeArguments, {}, impl.declaration->type)) + "$" +
            impl.declaration->trait.declaration->name.value +
            nameOf(with(impl.typeArguments, {}, impl.declaration->trait)
                       .typeArguments) +
            "$" + target.name.value + nameOf(method.typeArguments);

        auto& func = functions[name];
        if (!func) {
            std::vector<llvm::Type*> llvmParamTypes{};
            llvmParamTypes.push_back(asLLVM(with(
                impl.typeArguments, method.typeArguments, impl.declaration->type
            )));

            for (auto& arg : target.parameters) {
                llvmParamTypes.push_back(asLLVM(
                    with(impl.typeArguments, method.typeArguments, arg.type)
                ));
            }
            auto retTypeIs = with(
                impl.typeArguments, method.typeArguments, target.returnType
            );
            auto returnTypeAsLLVM = asLLVM(std::move(retTypeIs));
            auto funcType = llvm::FunctionType::get(
                returnTypeAsLLVM, llvmParamTypes, false
            );
            auto llvmFunc = llvm::Function::Create(
                funcType, llvm::Function::ExternalLinkage, name,
                currentModule.get()
            );
            size_t i = 0;
            for (auto& arg : llvmFunc->args()) {
                if (i == 0) {
                    arg.setName("this");
                } else {
                    arg.setName(target.parameters[i - 1].name.value);
                }
                ++i;
            }
            func = new FunctionMonomorph{
                llvmFunc,
                &target,
                with(*blockTypeArguments, *typeArguments, method.typeArguments),
                with(*blockTypeArguments, *typeArguments, impl.typeArguments),
                thisValue.type,
            };
            signatureStack.push_back(func);
        }

        args.insert(args.begin(), thisValue);

        return call(func, args);
    }

    llvm::Value* call(FunctionMonomorph* func, std::vector<Value> const& args) {
        std::vector<llvm::Value*> argVals{};
        fmt::println("EMIT call");
        for (auto& val : args) {
            argVals.push_back(val.asLLVM);
        }
        return builder->CreateCall(func->asLLVM, argVals);
    }

    Value operator()(ast::Call&& call) {
        fmt::println("EMIT call2");
        std::vector<Value> args{};
        std::vector<std::variant<std::monostate, size_t, Temp>> owners{};
        for (auto& arg : call.argValues) {
            Value argValue = (*this)(arg);
            if (isNever(argValue)) {
                fmt::println("never arg! {}", arg);
                return never;
            }
            args.push_back(argValue);
            owners.push_back(argValue.owner);
        }

        auto ret = std::visit(
            match{
                [&](Function const& function) -> llvm::Value* {
                    fmt::println("EMIT call3");
                    auto monomorphName = nameOf(function.typeArguments);
                    auto& func = functions
                        [function.declaration->name.value + monomorphName];
                    if (!func) {
                        func = createSignature(function);
                        signatureStack.push_back(func);
                    }
                    std::vector<llvm::Value*> argVals{};
                    auto result = this->call(func, args);
                    for (size_t i = 0; i < args.size(); ++i) {
                        if (owners[i].index() == 0)
                            drop(args[i]);
                    }
                    return result;
                },
                [&](type::Named const& type) -> llvm::Value* {
                    std::cout << "now, for type " << std::endl;
                    std::cout << "now, for type "
                              << type.declaration->name.value << std::endl;

                    auto named = asLLVM(type);

                    auto literal = llvm::UndefValue::get(named);
                    for (size_t i = 0; i < args.size(); ++i) {
                        builder->CreateInsertValue(
                            literal, copy(args[i]),
                            {0, static_cast<unsigned int>(i)}
                        );
                    }

                    return literal;
                },
                [&](TraitMethodRef const& method) -> llvm::Value* {
                    fmt::println("EMIT call4");
                    ImplRef impl = implScope.find(
                        with(
                            *blockTypeArguments, *typeArguments, method.thisType
                        ),
                        with(*blockTypeArguments, *typeArguments, method.trait)
                    );

                    ast::PropertyAccess& access =
                        std::get<ast::PropertyAccess>(call.lhs->value);
                    auto result = callTraitMethod(
                        (*this)(*access.lhs), access.propertyIdx, method, impl,
                        args
                    );
                    for (size_t i = 0; i < args.size(); ++i) {
                        if (owners[i].index() == 0)
                            drop(args[i]);
                    }
                    return result;
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
        fmt::println("asdasdasd {}, {}", access.namedDepth, access.propertyIdx);
        while (access.namedDepth) {
            --access.namedDepth;
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
        while (access.namedDepth) {
            --access.namedDepth;
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

    Value insert(Value value, size_t index, Value field) {
        return insert(
            value, llvm::ArrayRef{static_cast<unsigned int>(index)}, field
        );
    }

    Value
    insert(Value value, llvm::ArrayRef<unsigned int> indices, Value field) {
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
            value.owner = {};
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
            std::cout << ":bind var/let" << std::endl;
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
                            std::cout << "anon" << std::endl;
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
            for (size_t index : property.propertyIndices) {
                assignBinding(ir.release(propertyValues[index]));
            }
            if (property.pattern) {
                (*this)(*property.pattern, ir.load(ir.cleanupStack.size() - 1));
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
            std::cout << "REST OFFSET: " << restOffset << std::endl;
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
                std::cout << "devec 1" << std::endl;
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
        fmt::println("THE owner {}", value.owner.index());
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
        std::cout << "CHECK FUNC: " << function->definition->name.value
                  << std::endl;

        auto entry = createBlock("entry");
        function->asLLVM->insert(function->asLLVM->end(), entry);
        builder->SetInsertPoint(entry);

        auto arg = function->asLLVM->args().begin();
        ast::Binding temp;
        if (function->thisType) {
            std::cout << ":bind this " << cleanupStack.size() << std::endl;
            cleanupStack.push_back({&*arg});
            ++arg;
        }
        size_t i = 0;
        while (arg != function->asLLVM->args().end()) {
            std::cout << ":bind arg" << std::endl;
            cleanupStack.push_back({&*arg});
            ++arg;
            ++i;
        }
        typeArguments = &function->typeArguments;
        blockTypeArguments = &function->blockTypeArguments;

        std::cout << "looking for trait bounds" << std::endl;

        std::cout << "got both " << function->definition->name.value
                  << std::endl;
        fmt::println("b {}", function->definition->body);

        auto ret = (*this)(function->definition->body);
        if (!isNever(ret)) {
            ret.type = with(
                *blockTypeArguments, *typeArguments,
                function->definition->returnType
            );
            builder->CreateRet(copy(ret));
        }
        cleanupStack.clear();
        if (llvm::verifyFunction(*function->asLLVM, &llvm::outs())) {
            std::cout << "error found" << std::endl;
            throw "error found";
        }
    }

    void operator()(TypeMonomorph* type) {}

    void emitMain(ast::Expression&& mainBody) {
        functions["rtAlloc"] = createSignature(allocFunc);
        functions["rtSlice"] = createSignature(sliceFunc);
        functions["rtFree"] = createSignature(freeFunc);
        std::cout << "sigs done" << std::endl;

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

        auto llvmFunc = llvm::Function::Create(
            llvm::FunctionType::get(llvm::Type::getInt32Ty(*context), false),
            llvm::Function::ExternalLinkage, "main", currentModule.get()
        );
        auto mainBlock = createBlock("entry");
        std::cout << "generating body..." << std::endl;
        llvmFunc->insert(llvmFunc->end(), mainBlock);
        builder->SetInsertPoint(mainBlock);
        std::cout << "generating body..." << std::endl;
        auto result = (*this)(mainBody);
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

        fmt::println("name list");
        std::string name{"["};
        for (auto& type : named) {
            fmt::println("in loop {}", type.index());
            name.append(nameOf(type));
            name.append("$");
        }
        name.append("]");
        return std::move(name);
    }

    std::string nameOf(type::Named const& named) {
        std::string mono{named.declaration->name.value};
        fmt::println("OF {}", named.declaration->name.value);
        mono.append(nameOf(named.typeArguments));
        return std::move(mono);
    }

    std::string nameOf(type::Struct const& structure) {
        std::string name{"{"};
        for (auto& [property, type] : structure.properties) {
            name.append(property);
            name.append(":");
            name.append(nameOf(type));
            name.append("$");
        }
        name.append("}");
        return std::move(name);
    }

    std::string nameOf(type::Tuple const& tuple) {
        std::string name{"("};
        for (auto& type : tuple.fields) {
            name.append(nameOf(type));
            name.append("$");
        }
        name.append(")");
        return std::move(name);
    }

    std::string nameOf(type::BuiltIn* const& builtIn) {
        return builtIn->name;
    }

    std::string nameOf(type::Parameter const& parameter) {
        return nameOf(
            parameter.isBlockParameter ? (*blockTypeArguments)[parameter.index]
                                       : (*typeArguments)[parameter.index]
        );
    }

    llvm::Type* asLLVM(type::Named const& type) {
        std::cout << "get named" << std::endl;
        auto name = nameOf(type);
        auto& monomorph = namedTypes[name];
        if (!monomorph) {
            std::cout << "no mono " << type.typeArguments.size() << std::endl;
            std::cout << type.declaration->proto << std::endl;
            auto resT = with(type.typeArguments, {}, type.declaration->proto);
            std::cout << "no mono2" << std::endl;
            auto asLLVMStruct = llvm::StructType::create(
                *context, {asLLVM(std::move(resT))}, name, false
            );
            std::cout << "asllvm created" << std::endl;
            monomorph = new TypeMonomorph{asLLVMStruct};
        }
        std::cout << "got named" << std::endl;
        return monomorph->asLLVM;
    }

    llvm::Type* asLLVM(type::BuiltIn* const& type) {
        std::cout << "so, it is here indeed - " << type->name << type->asLLVM
                  << std::endl;
        return type->asLLVM;
    }

    llvm::Type* asLLVM(type::Tuple const& type) {
        std::vector<llvm::Type*> fields;
        for (auto& field : type.fields) {
            fields.push_back(asLLVM(field));
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
                    0, TraitMethodRef{type, Trait{copyDeclaration, {}}, {}},
                    copyImpl.value(), args
                ),
                indices
            );
            return;
        }
        copyInner(value, type, indices);
    }

    Value copy(Value value) {
        std::cout << "COPY? " << value.type << std::endl;
        if (value.owner.index() == 0) {
            fmt::println("No, it's owned");
            return value;
        }

        std::cout << value.owner.index() << " copying value of type "
                  << value.type << std::endl;
        if (auto copyImpl =
                implScope.tryFind(value.type, Trait{copyDeclaration, {}})) {
            std::vector<Value> args;

            return {
                callTraitMethod(
                    value, 0,
                    TraitMethodRef{value.type, Trait{copyDeclaration, {}}, {}},
                    copyImpl.value(), args
                ),
                {},
                value.type,
            };
        }

        std::vector<unsigned int> indices{};
        copyInner(&value, value.type, indices);
        return {value.asLLVM, {}, value.type};
    }

    void drop(
        llvm::Value* value, Type const& type, std::vector<unsigned int>& indices
    ) {
        std::cout << "letsa drop" << std::endl;
        if (auto dropImpl =
                implScope.tryFind(type, Trait{dropDeclaration, {}})) {
            std::vector<Value> args;
            callTraitMethod(
                {builder->CreateExtractValue(value, indices), 0, type}, 0,
                TraitMethodRef{type, Trait{dropDeclaration, {}}, {}},
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
        if (auto dropImpl =
                implScope.tryFind(value.type, Trait{dropDeclaration, {}})) {
            std::vector<Value> args;
            callTraitMethod(
                {value, 0, value.type}, 0,
                TraitMethodRef{value.type, Trait{dropDeclaration, {}}, {}},
                dropImpl.value(), args
            );
            return;
        }

        std::vector<unsigned int> indices{};
        dropInner(value, value.type, indices);
    }

    Value take(ast::Expression& expression) {
        std::cout << "take attempted" << std::endl;

        return std::visit(
            match{
                [this](auto& value) { return (*this)(std::move(value)); },
                [this](ast::Variable& var) {
                    auto value = (*this)(std::move(var));
                    if (value.owner.index() == 1 && var.mayMove) {
                        cleanupStack[std::get<size_t>(value.owner)].value =
                            nullptr;
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
