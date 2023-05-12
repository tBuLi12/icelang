#include "../parser/ast.h"
#include "./types.h"

#include <algorithm>
#include <tuple>
#include <unordered_map>
#include <unordered_set>

std::unordered_map<std::string, ast::FunctionDeclaration*>
collectFunctions(ast::AST& program) {
    std::unordered_map<std::string, ast::FunctionDeclaration*> scope{};
    for (auto& function : program.functions) {
        fmt::println("received {}", function.name.value);
        scope[function.name.value] = &function;
    }
    return std::move(scope);
}

std::unordered_map<std::string, ast::TypeDeclaration*>
collectNamedTypes(ast::AST& program) {
    std::unordered_map<std::string, ast::TypeDeclaration*> scope{};
    for (auto& typeDeclaration : program.typeDeclarations) {
        scope[typeDeclaration.name.value] = &typeDeclaration;
    }
    return std::move(scope);
}

std::unordered_map<std::string, ast::TraitDeclaration*>
collectTraitDeclarations(ast::AST& program) {
    std::unordered_map<std::string, ast::TraitDeclaration*> scope{};
    for (auto& traitDeclaration : program.traitDeclarations) {
        scope[traitDeclaration.name.value] = &traitDeclaration;
    }
    return std::move(scope);
}

struct FieldAccessor {
    ast::PropertyAccess& access;

    Type operator()(type::Struct const& structure) {
        auto prop = std::find_if(
            structure.properties.begin(), structure.properties.end(),
            [&](auto& field) { return field.first == access.property.value; }
        );
        if (prop == structure.properties.end()) {
            std::cout << "invalid property name" << std::endl;
            throw "invalid property name";
        }

        size_t index = std::distance(structure.properties.begin(), prop);
        access.propertyIdx = index;
        return Type{structure.properties[index].second};
    }
    Type operator()(type::Named const& named) {
        access.namedDepth += 1;
        return std::visit(
            *this, with(named.typeArguments, {}, named.declaration->proto)
        );
    }

    Type operator()(type::Never const& never) {
        return never;
    }

    Type operator()(auto const&) {
        std::cout << "property access on non-struct ";
        fmt::println("{}", *access.lhs);
        throw "property access on non-struct";
    }
};

struct TraitBoundScope {
    std::vector<std::pair<Type, Trait>>* local;
    std::vector<std::pair<Type, Trait>>* block;

    std::optional<std::tuple<TraitBoundRef, Trait*, size_t, ast::Signature*>>
    findMethod(Type const& target, std::string const& name) const {
        if (block) {
            size_t boundIndex = 0;
            for (auto& [type, trait] : *block) {
                if (type == target) {
                    size_t methodIndex = 0;
                    for (auto& signature : trait.declaration->signatures) {
                        if (signature.name.value == name) {
                            return {
                                {TraitBoundRef{true, boundIndex}, &trait,
                                 methodIndex, &signature}};
                        }
                        ++methodIndex;
                    }
                }
                ++boundIndex;
            }
        }
        size_t boundIndex = 0;
        for (auto& [type, trait] : *local) {
            if (type == target) {
                size_t methodIndex = 0;
                for (auto& signature : trait.declaration->signatures) {
                    if (signature.name.value == name) {
                        return {
                            {TraitBoundRef{false, boundIndex}, &trait,
                             methodIndex, &signature}};
                    }
                    ++methodIndex;
                }
            }
            ++boundIndex;
        }
        return {};
    }

    bool includes(Trait const& checkedTrait, Type const& checkedType) const {
        for (auto& [type, trait] : *local) {
            if (checkedTrait == trait && checkedType == type) {
                return true;
            }
        }
        if (block) {
            for (auto& [type, trait] : *block) {
                if (checkedTrait == trait && checkedType == type) {
                    return true;
                }
            }
        }
        return false;
    }
};

bool tryMatch(
    std::vector<Type>& inferredTypeArguments, Type const& first,
    Type const& second
) {
    std::cout << "matching " << first << second << std::endl;
    return std::visit(
        match{
            [](auto const&, auto const&) { return false; },
            [&](type::Parameter const& parameter, auto const&) {
                inferredTypeArguments[parameter.index] = Type{second};
                return true;
            },
            [](type::BuiltIn* const& first, type::BuiltIn* const& second) {
                return first == second;
            },
            [&](type::Named const& first, type::Named const& second) {
                if (first.declaration != second.declaration) {
                    return false;
                }
                for (size_t i = 0; i < first.typeArguments.size(); ++i) {
                    if (!tryMatch(
                            inferredTypeArguments, first.typeArguments[i],
                            second.typeArguments[i]
                        )) {
                        return false;
                    }
                }
                return true;
            },
            [&](type::Struct const& first, type::Struct const& second) {
                if (first.properties.size() != second.properties.size()) {
                    return false;
                }
                for (size_t i = 0; i < first.properties.size(); ++i) {
                    if (first.properties[i].first !=
                            second.properties[i].first ||
                        !tryMatch(
                            inferredTypeArguments, first.properties[i].second,
                            second.properties[i].second
                        )) {
                        return false;
                    }
                }
                return true;
            },
            [&](type::Tuple const& first, type::Tuple const& second) {
                if (first.fields.size() != second.fields.size()) {
                    return false;
                }
                for (size_t i = 0; i < first.fields.size(); ++i) {
                    if (!tryMatch(
                            inferredTypeArguments, first.fields[i],
                            second.fields[i]
                        )) {
                        return false;
                    }
                }
                return true;
            },
        },
        first, second
    );
}

struct ImplementationScope : std::vector<ast::TraitImplementation*> {
    TraitBoundScope currentBounds;

    bool areSatisfied(std::vector<std::pair<Type, Trait>> const& traitBounds) {
        for (auto& [type, trait] : traitBounds) {
            std::cout << "looking for " << type << " as " << trait.declaration
                      << trait.declaration->name.value << std::endl;

            if (currentBounds.includes(trait, type)) {
                continue;
            }
            std::cout << "fuck3 " << this->size() << std::endl;
            bool found = false;
            for (auto implementation : *this) {
                std::cout << "trying  "
                          << implementation->trait.declaration->name.value
                          << std::endl;
                std::cout << "trying  " << implementation->type << std::endl;
                std::cout << "trying  " << trait.declaration->name.value
                          << std::endl;
                std::cout << "trying  " << type << std::endl;
                if ((found = matches(*implementation, trait, type))) {
                    std::cout << "yeah it does" << std::endl;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }

    bool matches(
        ast::TraitImplementation const& implemetation, Trait const& trait,
        Type const& type
    ) {
        if (implemetation.trait.declaration != trait.declaration) {
            std::cout << "no 1" << std::endl;
            return false;
        }

        std::vector<Type> inferredTypeArguments{};
        inferredTypeArguments.resize(implemetation.typeParameterNames.size());

        for (size_t i = 0; i < implemetation.trait.typeArguments.size(); ++i) {
            if (!tryMatch(
                    inferredTypeArguments, implemetation.trait.typeArguments[i],
                    trait.typeArguments[i]
                )) {
                std::cout << "no 2 " << implemetation.trait.typeArguments[i]
                          << trait.typeArguments[i] << std::endl;
                return false;
            }
        }

        if (!tryMatch(inferredTypeArguments, implemetation.type, type)) {
            std::cout << "no 3" << std::endl;
            return false;
        }

        std::cout << "no 4..." << std::endl;
        return areSatisfied(
            with(inferredTypeArguments, {}, implemetation.traitBounds)
        );
    }
};

constexpr std::array<std::pair<std::string_view, std::string_view>, 12>
    operatorTraitNames{{
        {"+", "Add"},
        {"*", "Multiply"},
        {"==", "Equate"},
        {"!=", "Equate"},
        {"&&", "And"},
        {"||", "Or"},
        {">=", "Compare"},
        {"<=", "Compare"},
        {"<", "Compare"},
        {">", "Compare"},
        {"/", "Divide"},
        {"-", "Substract"},
    }};

struct VariableScope : std::vector<std::unordered_map<std::string, size_t>> {

    std::vector<ast::Binding> bindings;

    std::vector<size_t> matchedStructPropertyIndices;
    bool matchMutable = false;
    std::vector<std::pair<std::string, Type>> const* matchedStructProperties;
    std::optional<Type> matchedField;
    bool matchedFieldWasAnonymous;

    void add(std::string const& name, Type type, bool isMutable = false) {
        back()[name] = bindings.size();
        bindings.push_back({{}, type, isMutable});
    }

    ast::Binding& operator[](size_t index) {
        return bindings[index];
    }

    std::optional<size_t> operator[](std::string const& name) {
        for (auto& scope : *this) {
            auto binding = scope.find(name);
            if (binding != scope.end()) {
                return binding->second;
            }
        }

        if (matchedField) {
            if (name == "_") {
                matchedFieldWasAnonymous = true;
                enter();
            }
            add(name, *matchedField, matchMutable);
            matchedField.reset();
            return bindings.size() - 1;
        }

        if (matchedStructProperties) {
            auto binding = std::ranges::find(
                *matchedStructProperties, name,
                &std::pair<std::string, Type>::first
            );
            if (binding != matchedStructProperties->end()) {
                matchedStructPropertyIndices.push_back(
                    std::distance(matchedStructProperties->begin(), binding)
                );
                add(name, binding->second, matchMutable);
                return bindings.size() - 1;
            }
        }
        return {};
    }

    void enter() {
        push_back({});
    }

    void exit(size_t controlFlowDepth) {
        for (auto [_, index] : back()) {
            if (bindings[index].lastUse.controlFlowDepth == controlFlowDepth &&
                bindings[index].lastUse.usee) {
                bindings[index].lastUse.usee->mayMove = true;
            }
        }
        bindings.resize(bindings.size() - back().size());
        pop_back();
    }
};

template <class T>
concept TypedNode = requires(T node) {
    { node.type } -> std::same_as<Type&>;
};

struct TypeChecker {
    std::unordered_map<std::string, type::BuiltIn*>& builtInTypes;
    std::unordered_map<std::string, ast::TypeDeclaration*> typeScope;
    std::unordered_map<std::string, ast::FunctionDeclaration*> funcScope;
    std::unordered_map<std::string, ast::TraitDeclaration*> traitScope;
    VariableScope scope{};
    ImplementationScope implementationScope;
    std::unordered_map<std::string, size_t> typeParameters{};
    std::unordered_map<std::string, size_t> blockTypeParameters{};
    std::optional<Type> selfType{};

    size_t controlFlowDepth = 0;
    std::vector<Type>* breakTypes{};

    Trait resolve(ast::TraitName& traitName) {
        auto declaration = traitScope.find(traitName.name.value);
        if (declaration == traitScope.end()) {
            std::cout << "undeclared trait: " << traitName.name.value
                      << std::endl;
            throw "undeclared trait";
        }
        std::vector<Type> typeParams;
        for (auto& typeName : traitName.typeArgumentNames) {
            typeParams.push_back(resolve(typeName));
        }
        return Trait{
            declaration->second,
            std::move(typeParams),
        };
    }

    Type resolve(ast::NamedType& named) {
        if (named.annotation) {
            return Type{builtInTypes["ptr"]};
        }

        if (named.name.value == "This") {
            if (!selfType) {
                std::cout << "self cannot be used in this context" << std::endl;
                throw "asd";
            }
            return Type{selfType.value()};
        }

        auto typeParameter = typeParameters.find(named.name.value);
        if (typeParameter != typeParameters.end() &&
            named.typeArgumentNames.size() == 0) {
            return Type{type::Parameter{false, typeParameter->second}};
        }
        auto blockTypeParameter = blockTypeParameters.find(named.name.value);
        if (blockTypeParameter != blockTypeParameters.end() &&
            named.typeArgumentNames.size() == 0) {
            return Type{type::Parameter{true, blockTypeParameter->second}};
        }
        auto type = typeScope.find(named.name.value);
        if (type == typeScope.end()) {
            auto builtin = builtInTypes.find(named.name.value);
            if (builtin == builtInTypes.end() ||
                named.typeArgumentNames.size() != 0) {
                std::cout << "undeclared type: " << named.name.value
                          << std::endl;
                throw "undeclared type";
            }
            return std::move(builtin->second);
        }
        std::vector<Type> typeArgs{};
        for (auto& type : named.typeArgumentNames) {
            typeArgs.push_back(resolve(type));
        }
        return Type{type->second, std::move(typeArgs)};
    }

    type::Tuple resolve(ast::TupleType& tuple) {
        std::vector<Type> fieldTypes{};
        for (auto& fieldType : tuple.fields) {
            fieldTypes.push_back(this->resolve(fieldType));
        }
        return type::Tuple{std::move(fieldTypes)};
    }

    type::Struct resolve(ast::StructType& structure) {
        std::vector<std::pair<std::string, Type>> fieldTypes{};
        for (auto& property : structure.properties) {
            fieldTypes.push_back(
                {std::move(property.name), this->resolve(property.typeName)}
            );
        }
        std::ranges::sort(fieldTypes, {}, &std::pair<std::string, Type>::first);
        return type::Struct{std::move(fieldTypes)};
    }

    Type resolve(ast::VectorType& vector) {
        return type::Named{typeScope["Vector"], {resolve(*vector.elementType)}};
    }

    Type resolve(ast::TypeName& typeName) {
        return std::visit(
            [this](auto& name) -> Type { return resolve(name); }, typeName.value
        );
    }

    void save(ast::TraitImplementation& traitImplementation) {
        selfType = Type{traitImplementation.type};

        size_t i = 0;
        for (auto& [_, parameter, bounds] :
             traitImplementation.typeParameterNames) {
            blockTypeParameters[parameter.value] = i;
            for (auto& trait : bounds) {
                traitImplementation.traitBounds.push_back(
                    {Type{type::Parameter{true, i}}, resolve(trait)}
                );
            }
            ++i;
        }

        traitImplementation.trait = resolve(traitImplementation.traitName);
        traitImplementation.type = resolve(traitImplementation.typeName);

        std::cout << traitImplementation.type << std::endl;

        implementationScope.push_back(&traitImplementation);

        blockTypeParameters.clear();

        selfType.reset();
    }

    void operator()(ast::TraitImplementation& traitImplementation) {
        selfType = Type{traitImplementation.type};
        size_t i = 0;
        for (auto& [_, parameter, __] :
             traitImplementation.typeParameterNames) {
            blockTypeParameters[parameter.value] = i++;
        }

        std::ranges::sort(
            traitImplementation.implementations, {},
            [](auto const& decl) { return decl.name.value; }
        );

        scope.enter();
        scope.add("this", *selfType, false);

        std::cout << "hmm 1" << std::endl;
        std::ranges::for_each(
            traitImplementation.implementations,
            [&](ast::FunctionDeclaration& func) {
                return (*this)(static_cast<ast::Signature&>(func));
            }
        );
        std::ranges::for_each(traitImplementation.implementations, *this);
        std::cout << "hmm 2" << std::endl;
        scope.exit(controlFlowDepth);

        if (traitImplementation.implementations.size() !=
            traitImplementation.trait.declaration->signatures.size()) {
            throw "invalid number of functions in impl";
            std::cout << "invalid number of functions in impl" << std::endl;
        }

        for (size_t i = 0; i < traitImplementation.implementations.size();
             ++i) {
            auto& fromTrait =
                traitImplementation.trait.declaration->signatures.at(i);
            auto& provided = traitImplementation.implementations.at(i);

            std::cout << "so we begin checking" << std::endl;

            if (fromTrait.name.value != provided.name.value) {
                fmt::println(
                    "mismatched name: {} {}", fromTrait.name, provided.name
                );
                throw "mismatched name";
            }
            std::cout << "so we 1" << std::endl;

            if (fromTrait.parameters.size() != provided.parameters.size()) {
                std::cout << "mismatched param count" << std::endl;
                throw "mismatched param count";
            }
            std::cout << "so we 2 " << fromTrait.parameters.size() << std::endl;

            if (fromTrait.typeParameterNames.size() !=
                provided.typeParameterNames.size()) {
                throw "mismatched type param count";
                std::cout << "mismatched type param count" << std::endl;
            }
            std::cout << "so we 3" << fromTrait.typeParameterNames.size()
                      << std::endl;

            std::vector<Type> typeArguments{};
            for (size_t j = 0; j < provided.typeParameterNames.size(); ++j) {
                typeArguments.push_back(type::Parameter{
                    false,
                    j,
                });
            }
            auto traitTypeArgs = traitImplementation.trait.typeArguments;
            traitTypeArgs.push_back(Type{traitImplementation.type});
            for (size_t j = 0; j < fromTrait.parameters.size(); ++j) {
                auto withe = with(
                    traitTypeArgs, typeArguments, fromTrait.parameters[j].type
                );
                std::cout << "so we checking cd" << j << std::endl;
                if (withe != provided.parameters[j].type) {
                    std::cout << "mismatched arg type" << std::endl;
                    throw "mismatched arg type";
                }
            }
            std::cout << "so we 4" << std::endl;

            if (with(traitTypeArgs, typeArguments, fromTrait.returnType) !=
                provided.returnType) {
                std::cout << "mismatched return type" << std::endl;
                throw "mismatched return type";
            }
            std::cout << "so we done checking" << std::endl;
        }
        selfType.reset();
        blockTypeParameters.clear();
    }

    void operator()(ast::TraitDeclaration& traitDeclaration) {
        size_t i = 0;
        for (auto& [_, parameter, bounds] :
             traitDeclaration.typeParameterNames) {
            blockTypeParameters[parameter.value] = i;
            for (auto& trait : bounds) {
                traitDeclaration.traitBounds.push_back(
                    {Type{type::Parameter{true, i}}, resolve(trait)}
                );
            }
            ++i;
        }

        selfType = Type{type::Parameter{true, i}};

        std::ranges::sort(
            traitDeclaration.signatures, {},
            [](auto const& decl) { return decl.name.value; }
        );

        for (auto& signature : traitDeclaration.signatures) {
            (*this)(signature);
        }

        selfType.reset();
        blockTypeParameters.clear();
    }

    void operator()(ast::TypeDeclaration& typeDeclaration) {
        size_t i = 0;
        for (auto& parameter : typeDeclaration.typeParameterNames) {
            blockTypeParameters[parameter.name.value] = i;
            ++i;
        }

        typeDeclaration.proto = resolve(typeDeclaration.protoName);

        blockTypeParameters.clear();
    }

    void operator()(ast::Signature& function) {
        size_t i = 0;
        for (auto& [_, parameter, bounds] : function.typeParameterNames) {
            typeParameters[parameter.value] = i;
            for (auto& trait : bounds) {
                function.traitBounds.push_back(
                    {Type{type::Parameter{false, i}}, resolve(trait)}
                );
            }
            ++i;
        }

        function.returnType = function.returnTypeName
                                  ? resolve(function.returnTypeName.value())
                                  : Type{builtInTypes["null"]};

        for (auto& parameter : function.parameters) {
            parameter.type = resolve(parameter.typeName);
        }

        typeParameters.clear();
    }

    static bool isNever(Type const& type) {
        return std::holds_alternative<type::Never>(type);
    }

    void operator()(ast::FunctionDeclaration& function) {
        if (function.annotation) {
            return;
        }

        std::cout << "will check " << function.name.value << std::endl;
        size_t i = 0;
        for (auto& [_, parameter, __] : function.typeParameterNames) {
            typeParameters[parameter.value] = i;
            ++i;
        }
        scope.enter();
        for (auto& parameter : function.parameters) {
            std::cout << "adding to scope" << std::endl;
            scope.add(parameter.name.value, parameter.type, false);
            std::cout << "added to scope" << std::endl;
        }
        implementationScope.currentBounds.local = &function.traitBounds;

        std::cout << "tc body... " << function.name.value << std::endl;
        auto returnType = (*this)(function.body);
        std::cout << "tc body done " << function.name.value << std::endl;

        scope.exit(controlFlowDepth);

        if (function.returnType != returnType && !isNever(returnType)) {
            std::cout << "mismtch in " << function.name.value << ": expected "
                      << function.returnType << " got " << returnType
                      << std::endl;
            fmt::println("{}", function.body);
            throw "type error - mismatched return type";
        }

        scope.clear();
        typeParameters.clear();
        std::cout << "returning..." << function.name.value << std::endl;
    }

    template <String op>
    ast::Expression subsituteOverloadedOperator(ast::Binary<op>&& binary) {
        return subsituteOverloadedOperator(
            std::move(binary), (*this)(*binary.lhs), (*this)(*binary.rhs)
        );
    }

    template <String op>
    ast::Expression subsituteOverloadedOperator(
        ast::Binary<op>&& binary, Type&& lhsType, Type&& rhsType
    ) {
        if (isNever(lhsType) || isNever(rhsType)) {
            return binary;
        }

        static constexpr std::string_view traitName =
            std::ranges::find(
                operatorTraitNames, std::string_view{op.characters},
                &std::pair<std::string_view, std::string_view>::first
            )
                ->second;

        fmt::println("ONE {}", op.characters);
        std::cout << rhsType << std::endl;
        fmt::println("ONE {}", op.characters);
        if (!implementationScope.areSatisfied(
                {{Type{lhsType},
                  Trait{traitScope[std::string{traitName}], {Type{rhsType}}}}}
            )) {
            fmt::println("No.");
            throw "";
        }

        if (lhsType == builtInTypes["int"] && rhsType == builtInTypes["int"]) {
            fmt::println("built in op");
            return binary;
        }

        std::vector<ast::Expression> args{};
        args.push_back(std::move(*binary.rhs));
        return ast::Call{
            Span{},
            ast::Expression{ast::PropertyAccess{
                Span{},
                std::move(binary.lhs),
                {},
                0,
                0,
            }},
            {},
            {std::move(args)},
            {TraitMethodRef{
                Type{lhsType},
                Trait{traitScope[std::string{traitName}], {Type{rhsType}}}}},
        };
    }

    ast::Expression subsituteOverloadedOperator(ast::Binary<"=">&& expression) {
        return expression;
    }
    ast::Expression subsituteOverloadedOperator(auto&& expression) {
        return expression;
    }

    Type operator()(ast::Expression& expression) {
        expression = std::visit(
            [this](auto& expr) {
                return subsituteOverloadedOperator(std::move(expr));
            },
            expression.value
        );
        expression = std::visit(
            [this](auto& expr) {
                return resolveVariableOrTypeExpression(std::move(expr));
            },
            expression.value
        );
        static_assert(TypedNode<ast::VectorLiteral>);
        auto type = std::visit(
            match{
                [this](auto& expr) { return (*this)(expr); },
                [this](TypedNode auto& expr) {
                    auto type = (*this)(expr);
                    expr.type = type;
                    return type;
                },
            },
            expression.value
        );
        return type;
    }

    template <String op> Type operator()(ast::Binary<op>& binary) {
        auto lhs = (*this)(*binary.lhs);
        auto rhs = (*this)(*binary.rhs);
        if (isNever(lhs) || isNever(rhs)) {
            return type::Never{};
        }
        return check(binary);
    }

    Type check(ast::Binary<"+">& addition) {

        return Type{builtInTypes["int"]};
    }

    Type check(ast::Binary<"==">& equate) {
        // auto lhsType = (*this)(*equate.lhs);
        // auto rhsType = (*this)(*equate.rhs);
        //     fmt::println("THREE");

        return Type{builtInTypes["bool"]};
    }

    Type check(ast::Binary<"&&">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type check(ast::Binary<("||")>& op) {
        fmt::println("not supported");
        throw "";
    }

    Type check(ast::Binary<"<=">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type check(ast::Binary<">=">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type check(ast::Binary<">">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type check(ast::Binary<"-">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type check(ast::Binary<"<">& op) {
        return builtInTypes["bool"];
    }

    Type check(ast::Binary<"/">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type operator()(ast::Cast& cast) {
        fmt::println("not supported");
        throw "";
    }

    void
    mergeBranchTypes(std::optional<Type>& accumulator, Type const& branch) {
        if (accumulator) {
            if (isNever(*accumulator)) {
                accumulator = std::move(branch);
            } else if (*accumulator != branch) {
                accumulator.reset();
            }
        }
    }

    Type operator()(ast::Match& match) {
        auto type = (*this)(*match.scrutinee);
        if (match.body.size() == 0) {
            fmt::println("match body cannot be empty");
            throw "";
        }

        std::optional<Type> result = type::Never{};
        for (auto& matchCase : match.body) {
            scope.enter();
            CheckPattern{*this, &matchCase != &match.body.back(), false}(
                matchCase.pattern, type
            );

            auto arm = std::visit(*this, matchCase.value.value);
            mergeBranchTypes(result, arm);
            scope.exit(controlFlowDepth);
        }
        match.sameTypeResult = result.has_value();
        return result ? *result : builtInTypes["null"];
    }

    Type operator()(lexer::FloatLiteral& cast) {
        fmt::println("not supported");
        throw "";
    }

    Type operator()(lexer::StringLiteral& cast) {
        fmt::println("not supported");
        throw "";
    }

    Type operator()(ast::IndexAccess& access) {
        auto lhs = (*this)(*access.lhs);
        auto index = (*this)(*access.index);
        type::Named* type = std::get_if<type::Named>(&lhs);
        if (!isNever(lhs) &&
            (!type || type->declaration != typeScope["Vector"])) {
            std::cout << "canot index non vec, got " << lhs.index();
            throw "";
        }
        if (!isNever(index) && index != builtInTypes["int"]) {
            fmt::println("canot index with non int");
            throw "";
        }
        if (isNever(index) || isNever(lhs)) {
            return type::Never{};
        }
        access.elementType = Type{type->typeArguments[0]};
        return std::move(type->typeArguments[0]);
    }

    Type operator()(ast::Return& cast) {
        fmt::println("not supported");
        throw "";
    }

    Type operator()(ast::Break& _break) {
        if (!breakTypes) {
            fmt::println("invalid ctx");
            throw "";
        }
        breakTypes->push_back(
            _break.value ? (*this)(*_break.value) : builtInTypes["null"]
        );
        return type::Never{};
    }

    Type operator()(ast::Continue& _continue) {
        if (!breakTypes) {
            fmt::println("invalid ctx");
            throw "";
        }
        breakTypes->push_back(
            _continue.value ? (*this)(*_continue.value) : builtInTypes["null"]
        );
        return type::Never{};
    }

    Type operator()(ast::Prefix<"-">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type operator()(ast::Prefix<"!">& op) {
        fmt::println("not supported");
        throw "";
    }

    Type operator()(ast::VectorElement& element) {
        return std::visit(*this, element.value);
    }

    Type operator()(ast::Spread& spread) {
        Type spreadeeType = (*this)(spread.value);
        if (isNever(spreadeeType))
            return type::Never{};

        type::Named* spreadee = std::get_if<type::Named>(&spreadeeType);
        if (!spreadee || spreadee->declaration != typeScope["Vector"]) {
            fmt::println("spread must target a vector");
            throw "";
        }
        return std::move(spreadee->typeArguments[0]);
    }

    Type operator()(ast::VectorLiteral& op) {
        Type elementType = std::visit(
            match{
                [this](ast::VectorElementType& type) {
                    return resolve(type.typeName);
                },
                [this](ast::VectorElements& elements) {
                    if (elements.elements.size() == 0) {
                        fmt::println("vec cannot be empty");
                        throw "";
                    }
                    Type firstElementType = (*this)(elements.elements[0]);
                    for (auto& element :
                         elements.elements | std::views::drop(1)) {
                        auto type = (*this)(element);
                        if (!isNever(firstElementType) && !isNever(type) &&
                            type != firstElementType) {
                            fmt::println("invalid elem type");
                            throw "";
                        }
                    }
                    return firstElementType;
                },
            },
            op.content
        );
        op.elementType = Type{elementType};
        std::cout << "LIT type " << elementType << std::endl;
        return type::Named{typeScope["Vector"], {std::move(elementType)}};
    }

    Type check(ast::Binary<"!=">& notEquals) {
        // auto lhsType = (*this)(*notEquals.lhs);
        // auto rhsType = (*this)(*notEquals.rhs);
        // fmt::println("FOUR");
        // if (!implementationScope.areSatisfied(
        //         {{Type{lhsType}, Trait{traitScope["Neq"], {Type{rhsType}}}}}
        //     )) {
        //     throw "";
        // }
        // notEquals.methodRef = TraitMethodRef{
        //     Type{lhsType}, Trait{traitScope["Neq"], {Type{rhsType}}}};
        return Type{builtInTypes["bool"]};
    }

    Type check(ast::Binary<"*">& multiplication) {
        return builtInTypes["int"];
    }

    Type tryWriteTo(auto&) {
        std::cout << "invalid assignment target - non vec " << std::endl;
        throw "";
    }

    Type tryWriteTo(ast::IndexAccess& access) {
        Type lhsType = tryWriteTo(*access.lhs);
        Type idxType = tryWriteTo(*access.index);
        type::Named* type = std::get_if<type::Named>(&lhsType);
        type::BuiltIn** idx = std::get_if<type::BuiltIn*>(&idxType);
        if (!isNever(lhsType) &&
            (!type || type->declaration != typeScope["Vector"])) {
            std::cout << "invalid assignment target - non vec " << std::endl;
            throw "";
        }
        if (!isNever(idxType) && (!idx || *idx != builtInTypes["int"])) {
            std::cout << "invalid index type for vec " << std::endl;
            throw "";
        }
        if (isNever(idxType) || isNever(lhsType)) {
            return type::Never{};
        }
        access.elementType = type->typeArguments[0];
        return type->typeArguments[0];
    }

    Type tryWriteTo(ast::PropertyAccess& access) {
        Type lhsType = tryWriteTo(*access.lhs);
        access.namedDepth = 0;
        return std::visit(FieldAccessor{access}, lhsType);
    }

    Type tryWriteTo(ast::TupleFieldAccess& access) {
        Type lhsType = tryWriteTo(*access.lhs);
        return std::visit(
            match{
                [&](type::Tuple const& tuple) -> Type {
                    return Type{tuple.fields[access.propertyIdx]};
                },
                [&](type::Never const&) -> Type { return type::Never{}; },
                [](auto const&) -> Type {
                    std::cout << "field write on non-tuple" << std::endl;
                    throw "field write on non-tuple";
                },
            },
            lhsType
        );
    }

    Type tryWriteTo(ast::Variable& variable) {
        auto index = scope[variable.name];
        if (!index) {
            std::cout << "undeclared variable";
            throw "";
        }
        variable.binding = index.value();
        if (scope[variable.binding].isMutable) {
            return scope[variable.binding].type;
        }
        std::cout << "assignment to immutable";
        throw "";
    }

    Type tryWriteTo(ast::Expression& expression) {
        auto type = std::visit(
            match{
                [this](auto& expression) { return tryWriteTo(expression); },
                [this](TypedNode auto& expression) {
                    auto type = tryWriteTo(expression);
                    expression.type = type;
                    return type;
                },
            },
            expression.value
        );
        return type;
    }

    Type operator()(ast::Binary<"=">& assignment) {
        Type rhsType = (*this)(*assignment.rhs);
        Type lhsType = (*this)(*assignment.lhs);
        tryWriteTo(*assignment.lhs);
        if (lhsType != rhsType && !isNever(lhsType) && !isNever(rhsType)) {
            std::cout << "invalid assignment - mismatched types";
            throw "";
        }
        return std::move(lhsType);
    }

    ast::Expression resolveVariableOrTypeExpression(auto&& expr) {
        return expr;
    }

    ast::Expression resolveVariableOrTypeExpression(ast::Variable&& variable) {
        if (variable.binding) {
            return variable;
        }

        if (variable.name != "this" && !scope[variable.name]) {
            ast::NamedType typeName{Span{}, {}, variable.name, {}};
            return ast::TypeExpression{
                Span{},
                ast::TypeName{std::move(typeName)},
                {},
            };
        }
        return variable;
    }

    Type operator()(ast::TypeExpression& typeExpression) {
        typeExpression.theType = resolve(typeExpression.value);
        return type::Named{typeScope["Type"], {}};
    }

    Type operator()(ast::Variable& variable) {
        std::cout << "ENTER var" << std::endl;
        if (variable.name == "") {
            std::cout << "'SVAR " << variable.binding << " "
                      << scope.bindings.size() << std::endl;
            Type t = scope[variable.binding].type;
            std::cout << "sss" << std::endl;
            return t;
        }

        if (variable.name == "this") {
            if (selfType) {
                return Type{selfType.value()};
            }
            std::cout << "'this' cannot be used in this context" << std::endl;
            throw "undefined variable";
        }

        auto binding = scope[variable.name];
        if (!binding) {
            std::cout << "undeclared variable " << variable.name << std::endl;
            throw "undefined variable";
        }
        variable.binding = *binding;
        fmt::println("foundee {} {}", variable.binding, scope.bindings.size());
        auto& bind = scope[variable.binding];
        fmt::println("found {}", variable.name);
        std::cout << bind.type << std::endl;
        bind.lastUse = {&variable, controlFlowDepth};
        return bind.type;
    };

    Type operator()(ast::Block& block) {
        scope.enter();
        Type type = Type{builtInTypes["null"]};
        for (auto& blockItem : block.items) {
            auto itemType = std::visit(*this, blockItem.value);
            if (!isNever(type)) {
                type = itemType;
            }
        }
        scope.exit(controlFlowDepth);
        if (isNever(type))
            return type::Never{};
        if (block.hasTrailingExpression) {
            return Type{type};
        }
        return Type{builtInTypes["null"]};
    }

    Type operator()(lexer::IntegerLiteral& integer) {
        return Type{builtInTypes["int"]};
    }

    Type operator()(ast::TupleLiteral& tuple) {
        std::vector<Type> fields{};
        for (auto& field : tuple.fields) {
            fields.push_back((*this)(field));
        }
        return Type{type::Tuple{std::move(fields)}};
    }

    Type operator()(ast::StructLiteral& structure) {
        std::vector<std::pair<std::string, Type>> fields{};
        for (auto& property : structure.properties) {
            if (!property.value) {
                property.value = ast::Expression{
                    ast::Variable{property.span, property.name}};
            }
            fields.push_back(
                {property.name, Type{(*this)(property.value.value())}}
            );
        }
        std::ranges::sort(fields, {}, &std::pair<std::string, Type>::first);
        Type literalType{type::Struct{std::move(fields)}};

        if (!structure.name) {
            return literalType;
        }
        type::Named type =
            std::get<type::Named>(resolve(structure.name.value()));

        if (type.declaration->proto != literalType) {
            std::cout << "invalid proto" << std::endl;
            throw "";
        }
        structure.namedType = type;
        return type;
    }

    Type protoOf(type::Named const& named) {
        return with(named.typeArguments, {}, named.declaration->proto);
    }

    struct CheckPattern {
        TypeChecker& check;
        bool allowGuards;
        bool isMutable;

        void operator()(ast::Pattern& pattern, Type const& type) {
            check.scope.matchMutable = isMutable;
            (*this)(pattern.body, type);
            if (pattern.guard) {
                if (!allowGuards) {
                    std::cout << "this pattern cannot fail" << std::endl;
                    throw "";
                }

                if (check(*pattern.guard) != check.builtInTypes["bool"]) {
                    std::cout << "guards must evaluate to a boolean"
                              << std::endl;
                    throw "";
                }
            }
        }

        void operator()(ast::PatternBody& body, Type const& type) {
            std::visit(
                match{
                    [&](ast::Destructure& destructure) {
                        std::visit(
                            [&](auto& destructure) {
                                (*this)(destructure, type);
                            },
                            destructure.value
                        );
                    },
                    [&](ast::Expression& expression) {
                        std::cout << "GSETEEE " << type << std::endl;
                        check.scope.matchedField = type;
                        check.scope.matchedFieldWasAnonymous = false;
                        Type expressionType = check(expression);
                        std::cout << "LETSAGO" << type << std::endl;
                        if (!check.scope.matchedField) {
                            std::cout << "GSET " << type << std::endl;
                            check.scope.bindings.back().type = type;
                            check.scope.bindings.back().isMutable = isMutable;
                            body.anonymous =
                                check.scope.matchedFieldWasAnonymous;
                            if (check.scope.matchedFieldWasAnonymous) {
                                check.scope.exit(check.controlFlowDepth);
                            }
                            if (!std::holds_alternative<ast::Variable>(
                                    expression.value
                                ) &&
                                expressionType != check.builtInTypes["bool"]) {
                                std::cout << "guards must evaluate to a boolean"
                                          << std::endl;
                                throw "";
                            }
                        } else {
                            body.anonymous = true;
                            check.scope.enter();
                            std::cout << "G " << std::endl;
                            check.scope.add("", type, false);
                            std::cout << "G" << std::endl;
                            check.scope.matchedField.reset();
                            expression = ast::Binary<"==">{
                                Span{}, std::move(expression),
                                ast::Expression{ast::Variable{
                                    Span{}, "",
                                    check.scope.bindings.size() - 1}}};
                            std::cout << "G" << std::endl;
                            check(expression);
                            std::cout << "G" << std::endl;
                            check.scope.exit(check.controlFlowDepth);
                        }
                    },
                },
                body.value
            );
        }

        void operator()(ast::DestructureTuple& tuple, Type const& tupleType) {
            Type target = tupleType;
            if (tuple.name) {
                auto type = check.resolve(*tuple.name);
                if (type != tupleType && !isNever(tupleType)) {
                    std::cout << "mismatched type" << std::endl;
                    throw "";
                }
                target = check.protoOf(std::get<type::Named>(type));
            }

            type::Tuple const* type = std::get_if<type::Tuple>(&target);
            if (!type && !isNever(tupleType)) {
                std::cout << "not a tuple type" << std::endl;
                throw "";
            }

            if (!isNever(tupleType) &&
                type->fields.size() != tuple.fields.size()) {
                std::cout << "invalid tuple size" << std::endl;
                throw "";
            }

            for (size_t i = 0; i < tuple.fields.size(); ++i) {
                Type never{type::Never{}};
                (*this)(
                    tuple.fields[i].body,
                    isNever(tupleType) ? never : type->fields[i]
                );
            }

            tuple.type = tupleType;
        }

        void
        operator()(ast::DestructureVector& vector, Type const& vectorType) {
            if (!allowGuards) {
                std::cout << "this pattern cannot fail" << std::endl;
                throw "";
            }
            type::Named const* type = std::get_if<type::Named>(&vectorType);
            if (!isNever(vectorType) &&
                (!type || type->declaration != check.typeScope["Vector"])) {
                std::cout << "not a vector type" << std::endl;
                throw "";
            }
            vector.elementType = isNever(vectorType) ? Type{type::Never{}}
                                                     : type->typeArguments[0];

            bool restPresent = false;
            for (auto& pattern : vector.items) {
                std::visit(
                    match{
                        [&](ast::Pattern& pattern) {
                            std::cout << "PTYPE " << vector.elementType
                                      << std::endl;
                            (*this)(pattern, vector.elementType);
                        },
                        [&](ast::RestElements& rest) {
                            if (std::exchange(restPresent, true)) {
                                std::cout << "only one rest pattern is allowed"
                                          << std::endl;
                                throw "";
                            }
                            if (rest.binding) {
                                check.scope.add(
                                    rest.binding->value, vectorType, isMutable
                                );
                            }
                        },
                    },
                    pattern.value
                );
            }
        }

        void operator()(
            ast::DestructureStruct& structure, Type const& structureType
        ) {
            Type target = structureType;
            if (structure.name) {
                auto type = check.resolve(*structure.name);
                if (type != structureType && !isNever(structureType)) {
                    std::cout << "mismatched type" << std::endl;
                    throw "";
                }
                target = check.protoOf(std::get<type::Named>(type));
            }
            type::Struct const* type = std::get_if<type::Struct>(&target);

            if (isNever(structureType)) {
                structure.type = structureType;
                return;
            }

            if (!type) {
                std::cout << "not a struct type" << std::endl;
                throw "";
            }

            for (auto& property : structure.properties) {
                (*this)(property, *type);
            }
            structure.type = structureType;
        }

        void
        operator()(ast::PropertyPattern& property, type::Struct const& type) {
            if (property.pattern) {
                auto propertyName =
                    std::get_if<ast::Variable>(&property.property.value);
                if (!propertyName) {
                    std::cout << "destructured properties may not be guarded"
                              << std::endl;
                    throw "";
                }
                auto prop = std::ranges::find(
                    type.properties, propertyName->name,
                    &std::pair<std::string, Type>::first
                );
                if (prop == type.properties.end()) {
                    std::cout << "invalid property name" << std::endl;
                    throw "";
                }
                property.propertyIndices.push_back(
                    std::distance(type.properties.begin(), prop)
                );
                (*this)(*property.pattern, prop->second);
            } else {
                size_t size = check.scope.size();
                check.scope.matchedStructProperties = &type.properties;
                Type type = check(property.property);
                check.scope.matchedStructProperties = nullptr;
                property.propertyIndices =
                    std::move(check.scope.matchedStructPropertyIndices);
                if (!std::holds_alternative<ast::Variable>(
                        property.property.value
                    )) {
                    if (!allowGuards) {
                        std::cout << "this pattern cannot fail" << std::endl;
                        throw "";
                    }
                    if (type != check.builtInTypes["bool"]) {
                        std::cout << "guards must evaluate to a boolean"
                                  << std::endl;
                        throw "";
                    }
                }
            }
        }
    };

    Type operator()(ast::LetBinding& let) {
        Type type = (*this)(let.initalValue);
        if (let.typeName &&
            !type.isAssignableTo(resolve(let.typeName.value()))) {
            std::cout << "cant assign this to" << std::endl;
            throw "";
        }
        CheckPattern{*this, false, false}(let.binding, type);
        return Type{builtInTypes["null"]};
    }

    Type operator()(ast::VarBinding& var) {
        std::cout << "VAR BEGIN" << std::endl;
        Type type = (*this)(var.initalValue);
        if (var.typeName &&
            !type.isAssignableTo(resolve(var.typeName.value()))) {
            std::cout << "cant assign this to" << std::endl;
            throw "";
        }
        CheckPattern{*this, false, true}(var.binding, type);
        std::cout << "VAR END" << std::endl;
        return Type{builtInTypes["null"]};
    }

    Type operator()(ast::Condition& condition) {
        return std::visit(
            match{
                [](auto&) -> Type { throw ""; },
                [this](ast::Expression& expression) {
                    return (*this)(expression);
                },
            },
            condition.value
        );
    }

    void expect(Type const& type, auto& node) {
        Type actual = (*this)(node);
        if (!actual.isAssignableTo(type)) {
            std::cout << "exprected different type" << std::endl;
            throw "";
        }
    }

    Type operator()(ast::If& ifExpr) {
        expect(Type{builtInTypes["bool"]}, *ifExpr.condition);

        std::optional<Type> result{type::Never{}};

        ++controlFlowDepth;
        auto trueType = (*this)(*ifExpr.trueBranch);
        if (ifExpr.falseBranch) {
            mergeBranchTypes(result, (*this)(**ifExpr.falseBranch));
        }
        ifExpr.hasSameTypeBranch = result.has_value();
        --controlFlowDepth;

        return ifExpr.hasSameTypeBranch ? std::move(*result)
                                        : Type{builtInTypes["null"]};
    }

    Type operator()(ast::While& loop) {
        std::visit(
            match{
                [&](ast::Expression& expression) {
                    expect(Type{builtInTypes["bool"]}, expression);
                    ++controlFlowDepth;
                    (*this)(*loop.body);
                    --controlFlowDepth;
                    return Type{builtInTypes["null"]};
                },
                [&](ast::LetBinding& binding) {
                    std::vector<Type> breaks;
                    auto previous = std::exchange(breakTypes, &breaks);
                    scope.enter();
                    auto type = (*this)(binding.initalValue);
                    CheckPattern{*this, true, false}(binding.binding, type);
                    ++controlFlowDepth;
                    auto body = (*this)(*loop.body);
                    --controlFlowDepth;
                    scope.exit(controlFlowDepth);
                    breakTypes = previous;
                    if (!body.isAssignableTo(type) &&
                        body != Type{builtInTypes["null"]}) {
                        std::cout << "exprected different type" << std::endl;
                        throw "";
                    }
                    for (auto& breakType : breaks) {
                        if (!breakType.isAssignableTo(body)) {
                            std::cout << "exprected different type"
                                      << std::endl;
                            throw "";
                        }
                    }
                    loop.useBody = body != Type{builtInTypes["null"]};
                },
                [&](ast::VarBinding&) {},
            },
            loop.condition->value
        );
    }

    Type checkCall(
        ast::Signature const& function, ast::Call& call,
        std::vector<Type> const& blockTypeArguments,
        std::vector<Type> const& typeArguments
    ) {
        std::cout << "checking..." << std::endl;

        if (call.argValues.size() != function.parameters.size()) {
            std::cout << "invalid number of arguments" << std::endl;
            throw "invalid number of arguments";
        }
        std::cout << "1" << std::endl;

        if (call.typeArgumentNames.size() !=
            function.typeParameterNames.size()) {
            std::cout << "invalid number of type arguments" << std::endl;
            throw "invalid number of type arguments";
        }
        std::cout << "2" << std::endl;

        auto withres =
            with(blockTypeArguments, typeArguments, function.traitBounds);
        // CHECK TRAIT BOUNDS
        std::cout << "2.5" << std::endl;
        if (!implementationScope.areSatisfied(std::move(withres))) {
            std::cout << "trait bounds not satisfied" << std::endl;
            throw "trait bounds not satisfied";
        }
        std::cout << "3" << std::endl;

        for (size_t i = 0; i < call.argValues.size(); ++i) {
            std::cout << "arg " << i << " of ";
            fmt::println("{}", *call.lhs);
            std::cout << std::endl;
            fmt::println("{}", call.argValues[i]);
            auto argT = (*this)(call.argValues[i]);
            std::cout << "gotval" << std::endl;
            auto otherArgT = with(
                blockTypeArguments, typeArguments, function.parameters[i].type
            );
            if (!argT.isAssignableTo(otherArgT)) {
                // std::cout << logs::SpannedMessage {  };
                std::cout << "invalid arg type in " << otherArgT << std::endl
                          << std::endl
                          << argT << std::endl;
                std::cout << "ye" << std::endl;
                throw "invalid arg type";
            }
        }
        std::cout << "4" << std::endl;

        return Type{
            with(blockTypeArguments, typeArguments, function.returnType)};
    }

    Type operator()(ast::Call& call) {
        std::vector<Type> typeArguments{};
        for (auto& typeArg : call.typeArgumentNames) {
            typeArguments.push_back(resolve(typeArg));
        }

        return std::visit(
            match{
                [&](ast::Variable const& variable) -> Type {
                    std::cout << "processing function call... " << std::endl;
                    auto func = funcScope.find(variable.name);
                    if (func == funcScope.end()) {
                        if (call.argValues.size() != 1) {
                            std::cout << "oh, undefined function" << std::endl;
                            throw "undefined function";
                        }
                        auto namedType = typeScope.find(variable.name);
                        if (namedType == typeScope.end()) {
                            std::cout << "oh, undefined type" << std::endl;
                            throw "undefined type";
                        }
                        if (namedType->second->typeParameterNames.size() !=
                            typeArguments.size()) {
                            std::cout << "oh, wrong tparam count" << std::endl;
                            throw "undefined type";
                        }
                        type::Tuple* tuple =
                            std::get_if<type::Tuple>(&namedType->second->proto);
                        if (!tuple) {
                            std::cout << "oh, not a tuple" << std::endl;
                            throw "undefined type";
                        }
                        if (tuple->fields.size() != call.argValues.size()) {
                            std::cout << "wrong param size" << std::endl;
                            throw "undefined type";
                        }
                        for (size_t i = 0; i < call.argValues.size(); ++i) {
                            if ((*this)(call.argValues[i])
                                    .isAssignableTo(with(
                                        typeArguments, {}, tuple->fields[i]
                                    ))) {
                                std::cout << "invalid named type" << std::endl;
                                throw "invalid named type";
                            }
                        }
                        auto constructedType = type::Named{
                            namedType->second,
                            std::move(typeArguments),
                        };
                        call.target = type::Named{constructedType};
                        return std::move(Type{std::move(constructedType)});
                    }
                    std::cout << "found func" << std::endl;

                    auto resolvedReturnType =
                        checkCall(*func->second, call, {}, typeArguments);
                    call.target = Function{
                        func->second,
                        std::move(typeArguments),
                    };
                    return std::move(resolvedReturnType);
                },
                [&](ast::PropertyAccess& method) -> Type {
                    std::cout << "processing method call... " << std::endl;
                    Type lhsType = (*this)(*method.lhs);
                    if (isNever(lhsType)) {
                        std::ranges::for_each(call.argValues, *this);
                        return type::Never{};
                    }
                    std::cout << "got lhs " << std::endl;
                    auto target = implementationScope.currentBounds.findMethod(
                        lhsType, method.property.value
                    );
                    if (!target) {
                        std::cout << "no such method" << std::endl;
                        throw "method not found";
                    }
                    auto [traitRef, trait, index, signature] =
                        std::move(target.value());
                    method.propertyIdx = index;

                    // blockTypeParameters
                    auto blockTArgs = trait->typeArguments;
                    blockTArgs.push_back(Type{lhsType});
                    auto resolvedReturnType =
                        checkCall(*signature, call, blockTArgs, typeArguments);
                    call.target = TraitMethodRef{
                        std::move(lhsType),
                        Trait{*trait},
                        std::move(typeArguments),
                    };
                    return std::move(resolvedReturnType);
                },
                [](auto const&) -> Type {
                    std::cout << "only functions or methods can be called"
                              << std::endl;
                    throw "only functions or methods can be called";
                },
            },
            call.lhs->value
        );
    }

    Type operator()(ast::PropertyAccess& access) {
        auto type = (*this)(*access.lhs);
        access.namedDepth = 0;
        std::cout << "it is " << type << std::endl;
        return std::visit(FieldAccessor{access}, type);
    }

    Type operator()(ast::TupleFieldAccess& access) {
        auto type = (*this)(*access.lhs);
        return std::visit(
            match{
                [&](type::Tuple const& tuple) -> Type {
                    return Type{tuple.fields[access.propertyIdx]};
                },
                [&](type::Never const&) -> Type { return type::Never{}; },
                [](auto const&) -> Type {
                    std::cout << "field access on non-tuple" << std::endl;
                    throw "field access on non-tuple";
                },
            },
            type
        );
    }

    void check(ast::AST& program) {
        typeScope["Type"] = new ast::TypeDeclaration{
            Span::null(),
            lexer::Identifier{"Type", Span::null()},
            {},
            ast::TypeName{},
            type::Struct{{{"size", Type{builtInTypes["int"]}}}},
        };

        for (auto& type : program.typeDeclarations) {
            (*this)(type);
        }
        std::cout << "type decls" << std::endl;
        for (auto& trait : program.traitDeclarations) {
            std::cout << "got " << trait.name.value << std::endl;
            (*this)(trait);
        }
        std::cout << "trait decls" << std::endl;
        for (auto& trait : program.traitImplementations) {
            save(trait);
        }
        std::cout << "trait impl headers" << std::endl;
        for (ast::Signature& function : program.functions) {
            (*this)(function);
        }
        std::cout << "function headers" << std::endl;
        for (auto& function : program.functions) {
            (*this)(function);
        }
        std::cout << "functions" << std::endl;
        for (auto& trait : program.traitImplementations) {
            (*this)(trait);
            std::cout << "one checked" << std::endl;
        }
        std::cout << "trait impls" << std::endl;
    }

    // void take(ast::Expression& expression) {
    //     ast::Variable* var = std::get_if<ast::Variable>(&expression.value);
    //     if (!var) {
    //         return;
    //     }
    //     ast::Parameter** param = std::get_if<ast::Parameter*>(&var->binding);
    //     if (!param) {
    //         return;
    //     }

    //     (**param).willMove = true;
    // }
};
