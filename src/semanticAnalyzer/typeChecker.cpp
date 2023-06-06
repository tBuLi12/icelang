#include "../parser/ast.h"
#include "./types.h"

#include <algorithm>
#include <filesystem>
#include <range/v3/all.hpp>
#include <tuple>
#include <unordered_map>
#include <unordered_set>

namespace views = ranges::views;

template <class... Ts> Span spanOf(std::variant<Ts...> const& variant);
Span spanOf(auto const& thing);

Span spanOf(auto const& variant)
    requires requires {
        variant.value;
        requires !requires { variant.span; };
    }
{
    return spanOf(variant.value);
}

template <class... Ts> Span spanOf(std::variant<Ts...> const& variant) {
    return std::visit([](auto const& item) { return spanOf(item); }, variant);
}

Span spanOf(auto const& thing)
    requires requires { thing.span; }
{
    return thing.span;
}

std::vector<ast::FunctionDeclaration*> collectFunctions(
    std::unordered_map<std::string, ast::FunctionDeclaration*>& scope,
    ast::Program& program, std::string const& modName
) {
    std::vector<ast::FunctionDeclaration*> redefinitions{};
    for (auto& function : program.functions) {
        function.fullName = fmt::format("{}::{}", modName, function.name.value);
        if (scope.find(function.fullName) != scope.end()) {
            redefinitions.push_back(&function);
        }
        scope[function.fullName] = &function;
    }
    return redefinitions;
}

std::vector<ast::TypeDeclaration*> collectNamedTypes(
    std::unordered_map<std::string, ast::TypeDeclaration*>& scope,
    ast::Program& program, std::string const& modName
) {
    std::vector<ast::TypeDeclaration*> redefinitions{};
    for (auto& typeDeclaration : program.typeDeclarations) {
        typeDeclaration.fullName =
            fmt::format("{}::{}", modName, typeDeclaration.name.value);
        if (scope.find(typeDeclaration.fullName) != scope.end()) {
            redefinitions.push_back(&typeDeclaration);
        }
        scope[typeDeclaration.fullName] = &typeDeclaration;
    }
    return redefinitions;
}

std::vector<ast::TraitDeclaration*> collectTraitDeclarations(
    std::unordered_map<std::string, ast::TraitDeclaration*>& scope,
    ast::Program& program, std::string const& modName
) {
    std::vector<ast::TraitDeclaration*> redefinitions{};
    for (auto& traitDeclaration : program.traitDeclarations) {
        traitDeclaration.fullName =
            fmt::format("{}::{}", modName, traitDeclaration.name.value);
        if (scope.find(traitDeclaration.fullName) != scope.end()) {
            redefinitions.push_back(&traitDeclaration);
        }
        scope[traitDeclaration.fullName] = &traitDeclaration;
    }
    return redefinitions;
}

struct TraitBoundScope {
    std::vector<std::pair<Type, Trait>>* local;
    std::vector<std::pair<Type, Trait>>* block;

    std::optional<std::tuple<Trait*, size_t>>
    findMethod(Type const& target, std::string const& name) const {
        if (block) {
            size_t boundIndex = 0;
            for (auto& [type, trait] : *block) {
                if (type == target) {
                    size_t methodIndex = 0;
                    for (auto& signature : trait.declaration->signatures) {
                        if (signature.name.value == name) {
                            return {{&trait, methodIndex}};
                        }
                        ++methodIndex;
                    }
                }
                ++boundIndex;
            }
        }
        size_t boundIndex = 0;
        if (local) {
            for (auto& [type, trait] : *local) {
                if (type == target) {
                    size_t methodIndex = 0;
                    for (auto& signature : trait.declaration->signatures) {
                        if (signature.name.value == name) {
                            return {{&trait, methodIndex}};
                        }
                        ++methodIndex;
                    }
                }
                ++boundIndex;
            }
        }
        return {};
    }

    bool includes(Trait const& checkedTrait, Type const& checkedType) const {
        if (local) {
            for (auto& [type, trait] : *local) {
                fmt::println("SEARCHING LOCAL BOUNDS {} {} {} {}", checkedTrait ,trait ,checkedType ,type);
                if (checkedTrait == trait && checkedType == type) {
                    return true;
                }
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
    std::vector<std::optional<Type>>& blockArgs,
    std::vector<std::optional<Type>>& args, Type const& first,
    Type const& second
) {
    std::cout << "matching " << first << second << std::endl;
    return std::visit(
        match{
            [](auto const&, auto const&) { return false; },
            [&](type::Parameter const& parameter, auto const&) {
                std::optional<Type>& toInfer = parameter.isBlockParameter
                                                   ? blockArgs[parameter.index]
                                                   : args[parameter.index];
                if (toInfer) {
                    return *toInfer == second;
                }
                toInfer = second;
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
                            blockArgs, args, first.typeArguments[i],
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
                            blockArgs, args, first.properties[i].second,
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
                            blockArgs, args, first.fields[i], second.fields[i]
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

bool containsType(Type const& first, Type const& second) {
    return first == second || std::visit(
        match{
            [](auto const&) {
                return false;
            },
            [&](type::Named const& named) {
                return ranges::any_of(named.typeArguments, [&](Type const& inner){return containsType(inner, second);});
            },
            [&](type::Struct const& structure) {
                return ranges::any_of(structure.properties, [&](Type const& inner){return containsType(inner, second);}, &std::pair<std::string, Type>::second);
            },
            [&](type::Tuple const& tuple) {
                return ranges::any_of(tuple.fields, [&](Type const& inner){return containsType(inner, second);});
            },
        },
        first
    );
}

bool containsType(Trait const& first, Type const& second) {
    return ranges::any_of(first.typeArguments, [&](Type const& inner){return containsType(inner, second);});
}

struct Intersect {
    std::vector<std::optional<Type>> parameters;
    bool ok = true;
    size_t noft;
    size_t noftother;

    Intersect(size_t one, size_t two) : noft(one), noftother(two) {
        parameters.resize(one + two);
    }

    struct Reindex {
        size_t offset;

        Trait operator()(Trait const& trait) {
            return Trait{
                trait.declaration, trait.typeArguments |
                                       views::transform(*this) |
                                       ranges::to<std::vector>()};
        }

        Type operator()(Type const& type) {
            return std::visit(*this, type);
        }

        Type operator()(type::Named const& type) {
            return type::Named{
                type.declaration, type.typeArguments | views::transform(*this) |
                                      ranges::to<std::vector>()};
        }

        Type operator()(type::Tuple const& type) {
            return type::Tuple{
                type.fields | views::transform(*this) |
                ranges::to<std::vector>()};
        }

        Type operator()(type::Struct const& type) {
            return type::Struct{
                type.properties |
                views::transform(
                    [this](auto const& prop) -> std::pair<std::string, Type> {
                        return {prop.first, (*this)(prop.second)};
                    }
                ) |
                ranges::to<std::vector>()};
        }

        Type operator()(type::Parameter const& type) {
            return type::Parameter{true, type.index + offset};
        }

        Type operator()(auto const& type) {
            return type;
        }
    };

    struct Substitute {
        std::vector<std::optional<Type>>& params;

        Trait operator()(Trait const& trait) {
            return Trait{
                trait.declaration, trait.typeArguments |
                                       views::transform(*this) |
                                       ranges::to<std::vector>()};
        }

        Type operator()(Type const& type) {
            return std::visit(*this, type);
        }

        Type operator()(type::Named const& type) {
            return type::Named{
                type.declaration, type.typeArguments | views::transform(*this) |
                                      ranges::to<std::vector>()};
        }

        Type operator()(type::Tuple const& type) {
            return type::Tuple{
                type.fields | views::transform(*this) |
                ranges::to<std::vector>()};
        }

        Type operator()(type::Struct const& type) {
            return type::Struct{
                type.properties |
                views::transform(
                    [this](auto const& prop) -> std::pair<std::string, Type> {
                        return {prop.first, (*this)(prop.second)};
                    }
                ) |
                ranges::to<std::vector>()};
        }

        Type operator()(type::Parameter const& type) {
            if (!params[type.index])
                return type;
            return *params[type.index];
        }

        Type operator()(auto const& type) {
            return type;
        }
    };

    void intersect(Type const& one, Type const& other) {
        (*this)(one, Reindex{noft}(other));
    }

    void intersect(Trait const& tone, Trait const& tother) {
        if (tone.declaration != tother.declaration) {
            ok = false;
            return;
        }

        for (auto&& [t1, t2] :
             views::zip(tone.typeArguments, tother.typeArguments)) {
            (*this)(t1, Reindex{noft}(t2));
        }
    }

    std::pair<
        std::vector<std::pair<Type, Trait>>,
        std::vector<std::pair<Type, Trait>>>
    mergeBounds(
        std::vector<std::pair<Type, Trait>> const& left,
        std::vector<std::pair<Type, Trait>> const& right
    ) {
        std::vector<std::pair<Type, Trait>> merged{};
        for (auto const& [type, trait] : left) {
            merged.push_back(
                {Substitute{parameters}(type), Substitute{parameters}(trait)}
            );
        }
        std::vector<std::pair<Type, Trait>> merged2{};
        for (auto const& [type, trait] : right) {
            merged2.push_back(
                {Substitute{parameters}(Reindex{noft}(type)),
                 Substitute{parameters}(Reindex{noft}(trait))}
            );
        }
        return {merged, merged2};
    }

    bool leftIsMoreSpecialized() {
        std::vector<type::Parameter*> referenced{};
        for (auto&& param : parameters | views::take(noft)) {
            if (!param) {
                continue;
            }
            type::Parameter* tparam = std::get_if<type::Parameter>(&*param);
            if (!tparam)
                return false;
            if (ranges::find(referenced, tparam) != referenced.end())
                return false;
            if (tparam->index < noft)
                return false;
            referenced.push_back(tparam);
        }
        return true;
    }

    bool rightIsMoreSpecialized() {
        std::vector<type::Parameter*> referenced{};
        for (auto&& param :
             parameters | views::slice(noft, parameters.size())) {
            if (!param) {
                continue;
            }
            type::Parameter* tparam = std::get_if<type::Parameter>(&*param);
            if (!tparam)
                return false;
            if (ranges::find(referenced, tparam) != referenced.end())
                return false;
            if (tparam->index >= noft)
                return false;
            referenced.push_back(tparam);
        }
        return true;
    }

    void operator()(Type const& left, Type const& right) {
        std::visit(*this, left, right);
    }

    void operator()(type::Named const& left, type::Named const& right) {
        if (left.declaration != right.declaration) {
            ok = false;
        }
        for (auto&& [t1, t2] :
             views::zip(left.typeArguments, right.typeArguments)) {
            (*this)(t1, t2);
        }
    }

    void operator()(type::BuiltIn* left, type::BuiltIn* right) {
        if (left != right) {
            ok = false;
        }
    }

    void operator()(type::Tuple const& left, type::Tuple const& right) {
        if (left.fields.size() != right.fields.size()) {
            ok = false;
        }
        for (auto&& [t1, t2] : views::zip(left.fields, right.fields)) {
            (*this)(t1, t2);
        }
    }

    void operator()(type::Struct const& left, type::Struct const& right) {
        if (left.properties.size() != right.properties.size()) {
            ok = false;
        }
        for (auto&& [t1, t2] : views::zip(left.properties, right.properties)) {
            if (t1.first != t2.first) {
                ok = false;
            }
            (*this)(t1.second, t2.second);
        }
    }

    void assertEq(type::Parameter const& left, Type const& right) {
        if (parameters[left.index]) {
            (*this)(*parameters[left.index], Substitute{parameters}(right));
        } else {
            std::vector<std::optional<Type>> dummy{};
            dummy.resize(parameters.size());
            dummy[left.index] = Substitute{parameters}(right);
            for (auto& rule : parameters) {
                if (rule) {
                    rule = Substitute{dummy}(*rule);
                }
            }
            parameters[left.index] = std::move(*dummy[left.index]);
        }
    }

    template <class T>
    void operator()(type::Parameter const& left, T const& right) {
        assertEq(left, Type{T{right}});
    }

    template <class T>
    void operator()(T const& left, type::Parameter const& right) {
        assertEq(right, Type{T{left}});
    }

    void operator()(type::Parameter const& left, type::Parameter const& right) {
        assertEq(left, Type{type::Parameter{right}});
    }

    void operator()(auto const&, auto const&) {
        ok = false;
    }
};

struct ImplementationNode {
    ast::TraitImplementation* impl;
    std::vector<ImplementationNode> children;
};

struct ImplementationScope {
    TraitBoundScope currentBounds;
    ImplementationNode root;

    bool areSatisfied(std::vector<std::pair<Type, Trait>> const& traitBounds) {
        for (auto& [type, trait] : traitBounds) {
            if (currentBounds.includes(trait, type)) {
                continue;
            }
            bool found = false;
            for (auto implementation : root.children) {
                if ((found =
                         matches(*implementation.impl, trait, type).has_value()
                    )) {
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }

    std::vector<size_t>
    areSatisfiedAll(std::vector<std::pair<Type, Trait>> const& traitBounds) {
        std::vector<size_t> notSatisfied{};
        for (auto&& [i, traitBound] : traitBounds | views::enumerate) {
            auto&& [type, trait] = traitBound;
            if (currentBounds.includes(trait, type)) {
                continue;
            }
            bool found = false;
            for (auto implementation : root.children) {
                if ((found =
                         matches(*implementation.impl, trait, type).has_value()
                    )) {
                    break;
                }
            }
            if (!found) {
                notSatisfied.push_back(i);
            }
        }
        return notSatisfied;
    }

    std::optional<std::vector<Type>> matches(
        ast::TraitImplementation const& implemetation, Trait const& trait,
        Type const& type
    ) {
        if (implemetation.trait.declaration != trait.declaration) {
            return {};
        }

        std::vector<std::optional<Type>> inferredTypeArguments{};
        std::vector<std::optional<Type>> dummy{};
        inferredTypeArguments.resize(implemetation.typeParameterNames.size());

        for (size_t i = 0; i < implemetation.trait.typeArguments.size(); ++i) {
            if (!tryMatch(
                    inferredTypeArguments, dummy,
                    implemetation.trait.typeArguments[i], trait.typeArguments[i]
                )) {
                return {};
            }
        }

        if (!tryMatch(inferredTypeArguments, dummy, implemetation.type, type)) {
            return {};
        }

        std::vector<Type> typeArguments{};
        for (auto& type : inferredTypeArguments) {
            typeArguments.push_back(*type);
        }

        return areSatisfied(with(typeArguments, {}, implemetation.traitBounds))
                   ? std::optional<std::vector<Type>>{std::move(typeArguments)}
                   : std::optional<std::vector<Type>>{};
    }

    std::optional<TraitImplRef> tryFindIn(
        ImplementationNode const& node, Type const& type, Trait const& trait
    ) {
        if (auto found = matches(*node.impl, trait, type)) {
            std::cout << "impl found" << type << std::endl;
            for (auto child : node.children) {
                if (auto moreSpecialized = tryFindIn(child, type, trait)) {
                    return moreSpecialized;
                }
            }
            return {{node.impl, std::move(found.value())}};
        }
        return {};
    }

    std::optional<TraitImplRef> tryFind(Type const& type, Trait const& trait) {
        for (auto implementation : root.children) {
            if (auto found = tryFindIn(implementation, type, trait)) {
                return found;
            }
        }
        return {};
    }

    TraitImplRef find(Type const& type, Trait const& trait) {
        auto impl = tryFind(type, trait);
        if (impl) {
            return std::move(impl.value());
        }

        std::cout << "FATAL: impl not found" << std::endl;
        throw "impl not found";
    }

    enum class ImplSpan {
        Overlapping,
        Greater,
        Lesser,
        Equal,
        Disjoint,
    };

    ImplSpan compare(
        std::vector<std::pair<Type, Trait>> const& left,
        std::vector<std::pair<Type, Trait>> const& right, size_t s
    ) {
        auto isSatisfiableBy = [&](ast::TraitImplementation* impl,
                                   Type const& type,
                                   Trait const& trait) -> bool {
            if (impl->trait.declaration != trait.declaration) {
                std::cout << "no cuz name" << std::endl;
                return false;
            }

            std::vector<std::optional<Type>> inferredTypeArguments{};
            std::vector<std::optional<Type>> dummy{};
            inferredTypeArguments.resize(s);

            for (size_t i = 0; i < impl->trait.typeArguments.size(); ++i) {
                if (!tryMatch(
                        inferredTypeArguments, dummy, trait.typeArguments[i],
                        impl->trait.typeArguments[i]
                    )) {
                    std::cout << "no cuz trait args" << std::endl;
                    return false;
                }
            }

            if (!tryMatch(inferredTypeArguments, dummy, type, impl->type)) {
                std::cout << "no cuz type" << std::endl;
                return false;
            }
            return true;
        };
        auto isNotSatisfiable = [&](std::pair<Type, Trait> const& bound
                                ) -> bool {
            Trait const& trait = bound.second;
            Type const& type = bound.first;
            for (auto implementation : root.children) {
                if (isSatisfiableBy(implementation.impl, type, trait)) {
                    return false;
                }
            };

            std::cout << "not satisfiable: " << bound.first << " is "
                      << bound.second.declaration->name.value << std::endl;
            return true;
        };
        if (ranges::any_of(left, isNotSatisfiable) ||
            ranges::any_of(right, isNotSatisfiable)) {
            std::vector<size_t> ttt{};
            std::cout << "not satisfiable " << left.size() << ' '
                      << right.size() << std::endl;
            std::cout << "said bounds "
                      << ranges::any_of(ttt, [](size_t i) { return true; })
                      << std::endl;
            return ImplSpan::Disjoint;
        }
        std::cout << "satisfiable " << left.size() << ' ' << right.size()
                  << std::endl;
        auto lInR =
            ranges::all_of(left, [&](std::pair<Type, Trait> const& bound) {
                std::cout << bound.first << " is "
                          << bound.second.declaration->name.value << std::endl;
                return ranges::find(right, bound) != right.end();
            });
        auto rInL =
            ranges::all_of(right, [&](std::pair<Type, Trait> const& bound) {
                std::cout << bound.first << " is "
                          << bound.second.declaration->name.value << std::endl;
                return ranges::find(left, bound) != left.end();
            });
        if (lInR && rInL)
            return ImplSpan::Equal;
        if (lInR)
            return ImplSpan::Greater;
        if (rInL)
            return ImplSpan::Lesser;
        std::cout << "bounds overlapping" << std::endl;
        return ImplSpan::Overlapping;
    }

    ImplSpan
    compare(ast::TraitImplementation* impl, ast::TraitImplementation* other) {
        Intersect is{
            impl->typeParameterNames.size(), other->typeParameterNames.size()};
        is.intersect(impl->type, other->type);
        is.intersect(impl->trait, other->trait);
        if (!is.ok) {
            std::cout << "said structure" << std::endl;
            return ImplSpan::Disjoint;
        }

        auto [lBounds, rBounds] =
            is.mergeBounds(impl->traitBounds, other->traitBounds);
        auto boundCmp = compare(
            lBounds, rBounds,
            impl->typeParameterNames.size() + other->typeParameterNames.size()
        );
        if (boundCmp == ImplSpan::Disjoint)
            return ImplSpan::Disjoint;

        bool lMore = is.leftIsMoreSpecialized();
        bool rMore = is.rightIsMoreSpecialized();

        std::cout << "structural compare " << rMore << lMore << std::endl;
        if (lMore && rMore)
            return boundCmp;
        
        if (impl->typeParameterNames.size() == 0)
            return ImplSpan::Lesser;
        if (other->typeParameterNames.size() == 0)
            return ImplSpan::Greater;

        if (lMore) {
            if (boundCmp == ImplSpan::Equal || boundCmp == ImplSpan::Lesser) {
                return ImplSpan::Lesser;
            }
        }
        if (rMore) {
            if (boundCmp == ImplSpan::Equal || boundCmp == ImplSpan::Greater) {
                return ImplSpan::Greater;
            }
        };
        return ImplSpan::Overlapping;
    }

    std::vector<logs::SpannedMessage> appendTo(
        std::vector<ImplementationNode>& nodes, ast::TraitImplementation* impl
    ) {
        std::vector<logs::SpannedMessage> errors{};
        std::cout << "append to..." << std::endl;
        ImplementationNode node{impl, {}};
        auto current = nodes.begin();
        auto end = nodes.end();
        while (current != end) {
            switch (compare(impl, current->impl)) {
            case ImplSpan::Overlapping:
                errors.push_back(logs::SpannedMessage{
                    impl->location->source,
                    impl->span.first().to(impl->traitName.span),
                    "invalid specialization", "does not specialize an existing implementation"});
                errors.push_back(logs::SpannedMessage{
                    current->impl->location->source,
                    current->impl->span.first().to(current->impl->traitName.span),
                    "note", "this implementation is equally specific", logs::Level::Info});
                ++current;
                break;
            case ImplSpan::Greater:
                node.children.push_back(std::move(*current));
                nodes.pop_back();
                --end;
                *current = *end;
                break;
            case ImplSpan::Lesser:
                for (auto& error : appendTo(current->children, impl)) {
                    errors.push_back(std::move(error));
                }
                return errors;
            case ImplSpan::Equal:
                errors.push_back(logs::SpannedMessage{
                    impl->location->source,
                    impl->span.first().to(impl->traitName.span),
                    "invalid specialization", "does not specialize an existing implementation"});
                errors.push_back(logs::SpannedMessage{
                    current->impl->location->source,
                    current->impl->span.first().to(current->impl->traitName.span),
                    "note", "this implementation is equally specific", logs::Level::Info});
                ++current;
                break;
            case ImplSpan::Disjoint:
                ++current;
                break;
            }
        }
        nodes.push_back(std::move(node));
        return errors;
    }

    std::vector<logs::SpannedMessage> removeDuplicates() {
        ImplementationNode newRoot{nullptr, {}};
        std::vector<logs::SpannedMessage> errors{};
        for (auto implementation : root.children) {
            if (!implementation.impl->invalidBounds) {
                for (auto& error :
                     appendTo(newRoot.children, implementation.impl)) {
                    errors.push_back(std::move(error));
                }
            }
        }
        root = std::move(newRoot);
        return errors;
    }

    struct TreePrinter {
        std::vector<size_t> indices{0};
        void operator()(ImplementationNode const& node) {
            std::cout << logs::SpannedMessage{
                node.impl->location->source, node.impl->span, "level",
                fmt::format("{}", fmt::join(indices, ", "))};
            indices.push_back(0);
            ranges::for_each(node.children, *this);
            indices.pop_back();
            ++indices.back();
        }
    };

    void debugTree() {
        TreePrinter printer{};
        ranges::for_each(root.children, printer);
    }

    // std::optional<std::tuple<Trait*, size_t>>
    // findMethod(Type const& target, std::string const& name) const {
    //     if (auto method = currentBounds.findMethod(target, name)) {
    //         return *method;
    //     }

    //     for (auto implementation : *this) {
    //         auto func = std::ranges::find_if(
    //             implementation->implementations,
    //             [&](auto& decl) { return name == decl.name.value; }
    //         );
    //         if ((found = matches(*implementation, trait, type).has_value()))
    //         {
    //             break;
    //         }
    //     }
    //     return {};
    // }
};

constexpr std::array<std::pair<std::string_view, std::string_view>, 3>
    operatorTraitNames{{
        {"+", "std::Add"},
        {"*", "std::Multiply"},
        {"==", "std::Equate"},
        // {"!=", "std::Equate"},
        // {"&&", "std::And"},
        // {"||", "std::Or"},
        // {">=", "std::Compare"},
        // {"<=", "std::Compare"},
        // {"<", "std::Compare"},
        // {">", "std::Compare"},
        // {"/", "std::Divide"},
        // {"-", "std::Substract"},
    }};

struct VariableScope : std::vector<std::pair<size_t, std::unordered_map<std::string, size_t>>> {

    std::vector<ast::Binding> bindings;

    std::vector<size_t> matchedStructPropertyIndices;
    bool matchMutable = false;
    std::vector<std::pair<std::string, Type>> const* matchedStructProperties;
    std::optional<Type> matchedField;
    bool matchedFieldWasAnonymous;

    void add(std::string const& name, Type type, bool isMutable = false) {
        if (name.length() > 0) {
            back().second[name] = bindings.size();
        } else {
            ++back().first;
        }
        bindings.push_back({{}, type, isMutable});
    }

    ast::Binding& operator[](size_t index) {
        return bindings[index];
    }

    std::optional<size_t> operator[](std::string const& name) {
        for (auto& scope : *this) {
            auto binding = scope.second.find(name);
            if (binding != scope.second.end()) {
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
        push_back({0, {}});
    }

    void exit(size_t controlFlowDepth) {
        std::cout << "hmm - " << size() << std::endl;
        for (auto [_, index] : back().second) {
            if (bindings[index].lastUse.controlFlowDepth == controlFlowDepth &&
                bindings[index].lastUse.usee) {
                bindings[index].lastUse.usee->mayMove = true;
            }
        }
        bindings.resize(bindings.size() - back().second.size() - back().first);
        pop_back();
    }
};

template <class T>
concept TypedNode = requires(T node) {
    { node.type } -> std::same_as<Type&>;
};

std::string_view packageName(std::string_view name) {
    auto slash = std::ranges::find(name, '/');
    return {name.begin(), slash};
}

std::string_view moduleName(std::string_view name) {
    auto slash = std::ranges::find(name, ':');
    return {name.begin(), slash};
}

// std::string findOwner(std::vector<Module*>& modules,
// ast::FunctionDeclaration* declaration) {
//     for (auto& mod : modules) {
//         auto ptr = mod->program.functions.data();
//         if (ptr <= declaration &&
//             declaration < (ptr + mod->program.functions.size())) {
//             return mod->moduleId;
//         }
//     }
//     std::cout << "no owner" << std::endl;
//     throw "";
// }

// std::string findOwner(ast::TypeDeclaration* declaration) {
//     for (auto& mod : modules) {
//         auto ptr = mod->program.typeDeclarations.data();
//         if (ptr <= declaration &&
//             declaration < (ptr + mod->program.typeDeclarations.size())) {
//             return mod->moduleId;
//         }
//     }
//     std::cout << "no owner" << std::endl;
//     throw "";
// }

Type operator||(std::optional<Type> type, Type other) {
    if (type)
        return *type;
    return other;
}

struct TypeChecker {
    std::unordered_map<std::string, type::BuiltIn*>& builtInTypes;

    std::unordered_map<std::string, ast::TypeDeclaration*> typeScope{};
    std::unordered_map<std::string, ast::FunctionDeclaration*> funcScope{};
    std::unordered_map<std::string, ast::TraitDeclaration*> traitScope{};
    std::vector<ast::Implementation*> implScope{};
    VariableScope scope{};
    ImplementationScope implementationScope;
    std::unordered_map<std::string, size_t> typeParameters{};
    std::unordered_map<std::string, size_t> blockTypeParameters{};

    std::optional<Type> selfType{};

    Type currentReturnType{};

    size_t controlFlowDepth = 0;
    std::vector<Type>* breakTypes{};

    Module* currentModule;
    std::vector<Module*>* modules;

    logs::MessageLog log{};

    template <class... Ts>
    void logError(
        Span span, std::string_view type, fmt::format_string<Ts...> format,
        Ts&&... args
    ) {
        log.diagnostics.push_back(logs::SpannedMessage{
            currentModule->source,
            span,
            type,
            fmt::format(format, std::forward<Ts>(args)...),
        });
    }

    template <class... Ts>
    void logInfo(
        Span span, std::string_view type, fmt::format_string<Ts...> format,
        Ts&&... args
    ) {
        log.diagnostics.push_back(logs::SpannedMessage{
            currentModule->source,
            span,
            type,
            fmt::format(format, std::forward<Ts>(args)...),
            logs::Level::Info,
        });
    }

    struct FieldAccessor {
        TypeChecker& tc;
        ast::PropertyAccess& access;

        Type operator()(type::Struct const& structure) {
            auto prop = std::find_if(
                structure.properties.begin(), structure.properties.end(),
                [&](auto& field) {
                    return field.first == access.property.value;
                }
            );
            if (prop == structure.properties.end()) {
                tc.logError(
                    access.property.span, "error",
                    "property {} does not exist on type {}",
                    access.property.value, Type{type::Struct{structure}}
                );
                return type::Never{};
            }

            size_t index = std::distance(structure.properties.begin(), prop);
            access.propertyIdx = index;
            return Type{structure.properties[index].second};
        }
        Type operator()(type::Named const& named) {
            if (!tc.isVisible(
                    named.declaration, named.declaration->protoVisibility
                )) {
                tc.logError(
                    spanOf(*access.lhs), "error",
                    "prototype of {} is not accessible",
                    Type{type::Named{named}}
                );
                return type::Never{};
            }
            std::cout << "PROTOVis " << (named.declaration->protoVisibility.level == ast::Visibility::Level::Public) << " of " <<  named.declaration->name.value << std::endl;
            if (access.property.value == "proto") {
                access.propertyIdx = 0;
                return with(named.typeArguments, {}, named.declaration->proto);
            }
            access.namedDepth += 1;
            return std::visit(
                *this, with(named.typeArguments, {}, named.declaration->proto)
            );
        }

        Type operator()(type::Never const& never) {
            return type::Never{};
        }

        Type operator()(auto const&) {
            tc.logError(spanOf(*access.lhs), "error", "not a struct type");
            return type::Never{};
        }
    };

    std::optional<std::string> prepend(ast::Path const& path) {
        auto mod = currentModule;
        std::cout << "looking in " << mod->moduleId << mod->imports.size()
                  << std::endl;
        auto last = --path.segments.end();
        auto first = path.segments.begin();
        while (first != last) {
            auto import = mod->imports.find(first->value);
            if (import == mod->imports.end()) {
                std::cout << "looking failed in " << mod->moduleId
                          << mod->imports.size() << std::endl;
                for (auto md : *modules) {
                    for (auto& [name, _] : md->imports) {
                        std::cout << "available " << name << std::endl;
                    }
                }
                logError(
                    first->span, "error", "module {} not found", first->value
                );
                return {};
            }
            using Level = ast::Visibility::Level;
            switch (import->second.second->level) {
            case Level::Public:
                break;
            case Level::Internal:
                if (currentModule->packageName() != mod->packageName()) {
                    logError(
                        first->span, "error", "module {} is not accessible",
                        first->value
                    );
                    return {};
                }
                break;
            case Level::Private:
                if (currentModule != mod) {
                    logError(
                        first->span, "error", "module {} is not accessible",
                        first->value
                    );
                    return {};
                }
                break;
            }
            mod = (*modules)[import->second.first];
            ++first;
        }
        return fmt::format("{}::{}", mod->moduleId, last->value);
    }

    std::optional<Trait> resolve(ast::TraitName& traitName) {
        auto fullPath = prepend(traitName.name);
        if (!fullPath)
            return {};

        auto declaration = traitScope.find(*fullPath);
        if (declaration == traitScope.end()) {
            logError(
                traitName.name.span, "undeclared trait", "{}", traitName.name
            );
            return {};
        }
        std::vector<Type> typeArgs;
        for (auto& typeName : traitName.typeArgumentNames) {
            if (auto type = resolve(typeName)) {
                typeArgs.push_back(std::move(*type));
            } else {
                return {};
            }
        }
        if (!isVisible(declaration->second)) {
            logError(
                traitName.name.span, "error", "trait {} is not accessible", traitName.name
            );
            return {};
        };
        if (declaration->second->typeParameterNames.size() != typeArgs.size()) {
            logError(
                traitName.span,
                "invalid number of type arguments",
                "expected {} type arguments, found {}",
                declaration->second->typeParameterNames.size(), typeArgs.size()
            );
        }

        return Trait{
            declaration->second,
            std::move(typeArgs),
        };
    }

    std::optional<Type> resolve(ast::NamedType& named) {
        if (named.annotation) {
            return Type{builtInTypes["ptr"]};
        }

        if (named.name.str() == "This") {
            if (!selfType) {
                logError(
                    named.name.span, "error",
                    "\"This\" can only be used in methods"
                );
                return {};
            }
            if (named.typeArgumentNames.size() > 0) {
                logError(
                    named.span, "error", "\"This\" does not take type arguments"
                );
                return {};
            }
            return Type{selfType.value()};
        }

        if (named.name.segments.size() == 1) {
            auto typeParameter = typeParameters.find(named.name.str());
            if (typeParameter != typeParameters.end() &&
                named.typeArgumentNames.size() == 0) {
                return Type{type::Parameter{false, typeParameter->second}};
            }
            auto blockTypeParameter =
                blockTypeParameters.find(named.name.str());
            if (blockTypeParameter != blockTypeParameters.end() &&
                named.typeArgumentNames.size() == 0) {
                return Type{type::Parameter{true, blockTypeParameter->second}};
            }
        }
        auto path = prepend(named.name);
        if (!path)
            return {};
        auto type = typeScope.find(*path);
        if (type == typeScope.end()) {
            if (named.name.segments.size() == 1) {
                auto builtin = builtInTypes.find(named.name.str());
                if (builtin != builtInTypes.end()) {
                    if (named.typeArgumentNames.size() != 0) {
                        logError(
                            named.span, "error",
                            "built in type {} does not take type arguments",
                            builtin->second->name
                        );
                        return {};
                    }
                    return std::move(builtin->second);
                }
            }
            logError(named.name.span, "undeclared type", "{}", named.name);
            return {};
        }
        std::vector<Type> typeArgs{};
        for (auto& typeName : named.typeArgumentNames) {
            if (auto type = resolve(typeName)) {
                typeArgs.push_back(std::move(*type));
            } else {
                return {};
            }
        }
        if (!isVisible(type->second)) {
            logError(
                named.name.span, "error", "type {} is not accessible", named.name
            );
            return {};
        };
        if (type->second->typeParameterNames.size() != typeArgs.size()) {
            logError(
                named.span,
                "invalid number of type arguments",
                "expected {} type arguments, found {}",
                type->second->typeParameterNames.size(), typeArgs.size()
            );
        }
        return Type{type->second, std::move(typeArgs)};
    }

    std::optional<type::Tuple> resolve(ast::TupleType& tuple) {
        std::vector<Type> fieldTypes{};
        for (auto& fieldType : tuple.fields) {
            if (auto type = resolve(fieldType)) {
                fieldTypes.push_back(std::move(*type));
            } else {
                return {};
            }
        }
        return type::Tuple{std::move(fieldTypes)};
    }

    std::optional<type::Struct> resolve(ast::StructType& structure) {
        std::vector<std::pair<std::string, Type>> fieldTypes{};
        for (auto& property : structure.properties) {
            if (auto type = resolve(property.typeName)) {
                fieldTypes.push_back({std::move(property.name), *type});
            } else {
                return {};
            }
        }
        std::ranges::sort(fieldTypes, {}, &std::pair<std::string, Type>::first);
        return type::Struct{std::move(fieldTypes)};
    }

    std::optional<Type> resolve(ast::VectorType& vector) {
        if (auto elementType = resolve(*vector.elementType)) {
            return type::Named{typeScope["std::Vector"], {*elementType}};
        }
        return {};
    }

    std::optional<Type> resolve(ast::TypeName& typeName) {
        return std::visit(
            [this](auto& name) -> std::optional<Type> { return resolve(name); },
            typeName.value
        );
    }

    void save(ast::TraitImplementation& traitImplementation) {
        traitImplementation.location = currentModule;
        size_t i = 0;
        for (auto& [_, parameter, bounds] :
             traitImplementation.typeParameterNames) {
            blockTypeParameters[parameter.value] = i;
            for (auto& traitName : bounds) {
                if (auto trait = resolve(traitName)) {
                    traitImplementation.traitBounds.push_back(
                        {Type{type::Parameter{true, i}}, std::move(*trait)}
                    );
                } else {
                    traitImplementation.invalidBounds = true;
                }
            }
            ++i;
        }
        selfType = Type{traitImplementation.type};

        auto trait = resolve(traitImplementation.traitName);
        auto type = resolve(traitImplementation.typeName);

        if (trait && type) {
            for (size_t i = 0; i < traitImplementation.typeParameterNames.size(); ++i) {
                Type param{type::Parameter{true, i}};
                if (!containsType(*type, param) && !containsType(*trait, param)) {
                    logError(traitImplementation.typeParameterNames[i].span, "error", "unbound type parameter {}", traitImplementation.typeParameterNames[i].name.value);
                    blockTypeParameters.clear();
                    selfType.reset();
                    return;
                }
            }

            traitImplementation.trait = *trait;
            traitImplementation.type = *type;
            implementationScope.root.children.push_back(
                {&traitImplementation, {}}
            );
        }

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
        implementationScope.currentBounds.block = &traitImplementation.traitBounds;

        scope.enter();
        scope.add("this", *selfType, false);

        std::ranges::for_each(
            traitImplementation.implementations,
            [&](ast::FunctionDeclaration& func) {
                return (*this)(static_cast<ast::Signature&>(func));
            }
        );
        std::ranges::for_each(
            traitImplementation.implementations,
            [this](auto& arg) { (*this)(arg); }
        );
        scope.exit(controlFlowDepth);

        for (auto& signature :
             traitImplementation.trait.declaration->signatures) {
            auto providedPtr = ranges::find(
                traitImplementation.implementations, signature.name.value,
                [](auto const& fun) { return fun.name.value; }
            );
            if (providedPtr == traitImplementation.implementations.end()) {
                logError(
                    traitImplementation.traitName.span, "missing function",
                    "definition of {} is required by {}", signature.name,
                    traitImplementation.traitName
                );
                continue;
            }
            auto& provided = *providedPtr;

            if (signature.parameters.size() != provided.parameters.size()) {
                logError(
                    static_cast<ast::Signature&>(provided).span,
                    "invalid parameter count",
                    "expected {} parameters, found {}",
                    signature.parameters.size(), provided.parameters.size()
                );
            }

            if (signature.typeParameterNames.size() !=
                provided.typeParameterNames.size()) {
                logError(
                    static_cast<ast::Signature&>(provided).span,
                    "invalid number of type parameters",
                    "expected {} type parameters, found {}",
                    signature.typeParameterNames.size(),
                    provided.typeParameterNames.size()
                );
            }

            std::vector<Type> typeArguments{};
            for (size_t j = 0; j < provided.typeParameterNames.size(); ++j) {
                typeArguments.push_back(type::Parameter{
                    false,
                    j,
                });
            }

            auto traitTypeArgs = traitImplementation.trait.typeArguments;
            traitTypeArgs.push_back(Type{traitImplementation.type});
            for (auto&& [expected, found] :
                 views::zip(signature.parameters, provided.parameters)) {
                auto translated =
                    with(traitTypeArgs, typeArguments, expected.type);
                if (!found.type.isAssignableTo(translated)) {
                    logError(
                        spanOf(found.typeName), "invalid parameter type",
                        "expected {}, found {}", translated, found.typeName
                    );
                }
            }

            auto returnType =
                with(traitTypeArgs, typeArguments, signature.returnType);
            if (!provided.returnType.isAssignableTo(returnType)) {
                logError(
                    provided.returnTypeName ? spanOf(*provided.returnTypeName)
                                            : spanOf(provided.body).first(),
                    "invalid return type", "expected {}, found {}", returnType,
                    provided.returnType
                );
            }

            if (provided.isMutation != signature.isMutation) {
                logError(
                    provided.span,
                    "error", "mutability does not match declaration"
                );
            }
        }

        for (ast::Signature& signature : traitImplementation.implementations) {
            auto expected = ranges::find(
                traitImplementation.trait.declaration->signatures, signature.name.value,
                [](auto const& fun) { return fun.name.value; }
            );
            if (expected == traitImplementation.trait.declaration->signatures.end()) {
                logError(
                    signature.span, "invalid function",
                    "trait {} does not declare a function named {}", traitImplementation.traitName, signature.name
                );
            }
        }


        selfType.reset();
        blockTypeParameters.clear();
    }

    void operator()(ast::TraitDeclaration& traitDeclaration) {
        size_t i = 0;
        for (auto& [_, parameter, bounds] :
             traitDeclaration.typeParameterNames) {
            blockTypeParameters[parameter.value] = i;
            for (auto& traitName : bounds) {
                if (auto trait = resolve(traitName)) {
                    traitDeclaration.traitBounds.push_back(
                        {Type{type::Parameter{true, i}}, std::move(*trait)}
                    );
                }
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

        auto proto = resolve(typeDeclaration.protoName);
        typeDeclaration.proto = proto ? *proto : type::Never{};

        blockTypeParameters.clear();
    }

    void operator()(ast::Signature& function) {
        if (function.isMutation && !selfType) {
            function.isMutation = false;
            logError(function.span, "error", "mut is only allowed on methods");
        }
        function.location = currentModule;
        size_t i = 0;
        for (auto& [_, parameter, bounds] : function.typeParameterNames) {
            typeParameters[parameter.value] = i;
            for (auto& traitName : bounds) {
                if (auto trait = resolve(traitName)) {
                    function.traitBounds.push_back(
                        {Type{type::Parameter{false, i}}, std::move(*trait)}
                    );
                }
            }
            ++i;
        }

        function.returnType =
            function.returnTypeName
                ? resolve(function.returnTypeName.value()) || type::Never{}
                : Type{builtInTypes["null"]};

        for (auto& parameter : function.parameters) {
            parameter.type = resolve(parameter.typeName) || type::Never{};
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

        size_t i = 0;
        for (auto& [_, parameter, __] : function.typeParameterNames) {
            typeParameters[parameter.value] = i;
            ++i;
        }
        if (function.isMutation) {
            scope.bindings[0].isMutable = true;
        }
        scope.enter();
        for (auto& parameter : function.parameters) {
            if (scope.back().second.find(parameter.name.value) != scope.back().second.end()) {
                logError(parameter.name.span, "duplicate binding", "{} has already been declared in this scope", parameter.name.value);
                continue;
            }
            scope.add(parameter.name.value, parameter.type, false);
        }
        implementationScope.currentBounds.local = &function.traitBounds;
        currentReturnType = function.returnType;
        auto returnType = (*this)(function.body);

        scope.exit(controlFlowDepth);
        if (function.isMutation) {
            scope.bindings[0].isMutable = false;
        }

        if (function.returnType != returnType && !isNever(returnType)) {
            logError(
                spanOf(function.body), "invalid return type",
                "expected {}, found {}", function.returnType, returnType
            );
            if (function.returnTypeName) {
                logInfo(
                    spanOf(*function.returnTypeName), "note",
                    "{} declared here", function.returnType
                );
            }
        }

        typeParameters.clear();
    }

    Type chainType;

    std::pair<ast::Expression, Type>
    subsituteOverloadedOperator(ast::Binary<"==">&& binary) {
        return std::visit(
            match{
                [&](ast::Binary<"==">& eq) -> std::pair<ast::Expression, Type> {
                    auto [newExpr, _] =
                        subsituteOverloadedOperator(std::move(eq));
                    auto rhs = (*this)(*binary.rhs);
                    binary.lhs.reset();
                    auto [newExpr2, resType] = subsituteOverloadedOperator(
                        std::move(binary), Type{chainType}, Type{rhs}
                    );
                    chainType = rhs;
                    return {
                        ast::Expression{ast::Binary<"&&">{
                            Span{}, std::move(newExpr), std::move(newExpr2)}},
                        &type::boolean};
                },
                [&](auto&) {
                    auto lhs = (*this)(*binary.lhs);
                    auto rhs = (*this)(*binary.rhs);
                    chainType = rhs;
                    return subsituteOverloadedOperator(
                        std::move(binary), std::move(lhs), std::move(rhs)
                    );
                },
            },
            binary.lhs->value
        );
    }

    template <String op>
    std::pair<ast::Expression, Type>
    subsituteOverloadedOperator(ast::Binary<op>&& binary) {
        return subsituteOverloadedOperator(
            std::move(binary), (*this)(*binary.lhs), (*this)(*binary.rhs)
        );
    }

    template <String op>
    std::pair<ast::Expression, Type> subsituteOverloadedOperator(
        ast::Binary<op>&& binary, Type&& lhsType, Type&& rhsType
    ) {
        if (isNever(lhsType) || isNever(rhsType)) {
            return {std::move(binary), type::Never{}};
        }

        static constexpr auto traitName =
            std::ranges::find(
                operatorTraitNames, std::string_view{op.characters},
                &std::pair<std::string_view, std::string_view>::first
            );

        if (std::holds_alternative<type::BuiltIn*>(lhsType) &&
            std::holds_alternative<type::BuiltIn*>(rhsType)) {
            if (lhsType != rhsType) {
                logError(
                    binary.span, "invalid operands",
                    "built in operator {} requires the same type on both sides",
                    op.characters
                );
                logInfo(
                    spanOf(*binary.lhs), "note",
                    "left hand side of type {} here", lhsType
                );
                logInfo(
                    spanOf(*binary.rhs), "note",
                    "right hand side of type {} here", rhsType
                );
                return {std::move(binary), type::Never{}};
            }

            auto type = check(binary, lhsType);
            binary.type = type;
            return {std::move(binary), type};
        }

        if (traitName == operatorTraitNames.end()) {
            logError(binary.span, "error", "operator {} only accepts built in types as operands", op.characters);
            return {std::move(binary), type::Never{}};
        }

        if (!implementationScope.areSatisfied(
                {{Type{lhsType},
                  Trait{traitScope[std::string{traitName->second}], {Type{rhsType}}}}}
            )) {
            logError(
                binary.span, "invalid operands",
                "types {} and {} do not overload the {} operator", lhsType,
                rhsType, op.characters
            );
        }

        std::cout << binary.rhs.get() << std::endl;
        std::vector<ast::Expression> args{};
        args.push_back(std::move(*binary.rhs));
        auto ref = TraitMethodRef{
            Trait{traitScope[std::string{traitName->second}], {Type{rhsType}}}};
        std::cout << binary.lhs.get() << std::endl;
        // struct PropertyAccess {
        //     Span span;
        //     UPtr<Expression> lhs;
        //     std::optional<TraitName> traitName->second;
        //     lexer::Identifier property;

        //     size_t propertyIdx;
        //     size_t namedDepth;
        //     Type type;
        // };
        auto access = ast::PropertyAccess{
            Span{}, std::move(binary.lhs), {}, {}, 0, 0,
        };
        auto expr = ast::Expression{std::move(access)};
        auto call = ast::Call{
            Span{}, std::move(expr), {}, {std::move(args)}, {std::move(ref)},
        };

        fmt::println("lets check call");
        auto resolvedReturnType = checkCall(
            traitScope[std::string{traitName->second}]->signatures[0], call,
            {rhsType, lhsType}, {}
        );
        fmt::println("done with check call");
        call.type = resolvedReturnType;
        return {std::move(call), std::move(resolvedReturnType)};
    }

    std::pair<ast::Expression, Type>
    subsituteOverloadedOperator(ast::Binary<"=">&& expression) {
        auto type = (*this)(expression);
        return {std::move(expression), type};
    }

    Type operator()(ast::Expression& expression) {
        auto type = std::visit(
            match{
                [&](ast::Variable& expr) {
                    auto [newExpr, type] =
                        resolveVariableOrTypeExpression(std::move(expr));
                    expression = std::move(newExpr);
                    return type;
                },
                [&]<String op>(ast::Binary<op>& expr) {
                    auto [newExpr, type] =
                        subsituteOverloadedOperator(std::move(expr));
                    expression = std::move(newExpr);
                    return type;
                },
                [&](ast::Cast& expr) {
                    auto [newExpr, type] =
                        (*this)(std::move(expr));
                    expression = std::move(newExpr);
                    return type;
                },
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

    Type operator()(ast::TypeExpression&) { throw ""; }

    template <String op> Type operator()(ast::Binary<op>& binary) {
        auto lhs = (*this)(*binary.lhs);
        auto rhs = (*this)(*binary.rhs);
        if (isNever(lhs) || isNever(rhs)) {
            return type::Never{};
        }
        return check(binary);
    }

    Type check(ast::Binary<"+">& addition, Type const& type) {
        if (type == &type::integer) {
            return &type::integer;
        }

        if (type == &type::floating) {
            return &type::floating;
        }

        logError(
            addition.span, "invalid operands",
            "operator + only accepts int or float operands, found {}", type
        );
        return type::Never{};
    }

    Type check(ast::Binary<"==">& equate, Type const& type) {
        return &type::boolean;
    }

    Type check(ast::Binary<"&&">& op, Type const& type) {
        if (type != &type::boolean) {
            logError(
                op.span, "invalid operands",
                "operator && only accepts boolean operands, found {}", type
            );
            return type::Never{};
        }

        return &type::boolean;
    }

    Type check(ast::Binary<"||">& op, Type const& type) {
        if (type != &type::boolean) {
            logError(
                op.span, "invalid operands",
                "operator || only accepts boolean operands, found {}", type
            );
            return type::Never{};
        }

        return &type::boolean;
    }

    Type check(ast::Binary<"<=">& op, Type const& type) {
        if (type != &type::integer && type != &type::character &&
            type != &type::floating) {
            logError(
                op.span, "invalid operands",
                "operator <= only accepts int, float or char operands, found "
                "{}",
                type
            );
            return type::Never{};
        }

        return &type::boolean;
    }

    Type check(ast::Binary<">=">& op, Type const& type) {
        if (type != &type::integer && type != &type::ptr &&
            type != &type::character && type != &type::floating) {
            logError(
                op.span, "invalid operands",
                "operator >= only accepts int, float or char operands, found "
                "{}",
                type
            );
            return type::Never{};
        }

        return &type::boolean;
        ;
    }

    Type check(ast::Binary<">">& op, Type const& type) {
        if (type != &type::integer && type != &type::ptr &&
            type != &type::character && type != &type::floating) {
            logError(
                op.span, "invalid operands",
                "operator > only accepts int, float or char operands, found {}",
                type
            );
            return type::Never{};
        }

        return &type::boolean;
    }

    Type check(ast::Binary<"<">& op, Type const& type) {
        if (type != &type::integer && type != &type::ptr &&
            type != &type::character && type != &type::floating) {
            logError(
                op.span, "invalid operands",
                "operator < only accepts int, float or char operands, found {}",
                type
            );
            return type::Never{};
        }

        return &type::boolean;
    }

    Type check(ast::Binary<"-">& op, Type const& type) {
        if (type == &type::integer) {
            return &type::integer;
        }

        if (type == &type::floating) {
            return &type::floating;
        }

        logError(
            op.span, "invalid operands",
            "operator - only accepts int or float operands, found {}", type
        );
        return type::Never{};
    }

    Type check(ast::Binary<"/">& op, Type const& type) {
        if (type == &type::integer) {
            return &type::integer;
        }

        if (type == &type::floating) {
            return &type::floating;
        }

        logError(
            op.span, "invalid operands",
            "operator / only accepts int or float operands, found {}", type
        );
        return type::Never{};
    }

    Type check(ast::Binary<"!=">& notEquals, Type const& type) {
        return &type::boolean;
    }

    Type check(ast::Binary<"*">& multiplication, Type const& type) {
        if (type == &type::integer) {
            return &type::integer;
        }

        if (type == &type::floating) {
            return &type::floating;
        }

        logError(
            multiplication.span, "invalid operands",
            "operator * only accepts int or float operands, found {}", type
        );
        return type::Never{};
    }

    std::pair<ast::Expression, Type> operator()(ast::Cast&& cast) {
        auto lhs = (*this)(*cast.lhs);
        auto target = resolve(cast.typeName);
        if (!target) return {std::move(cast), type::Never{}};
        if (lhs == *target) {
            // logError(
            //     cast.span, "",
            //     "operator * only accepts int or float operands, found {}", type
            // );
            return {std::move(cast), lhs};
        }

        if (std::holds_alternative<type::BuiltIn*>(lhs) &&
            std::holds_alternative<type::BuiltIn*>(*target)) {
            if (lhs == &type::integer && *target == &type::floating) {
                return {std::move(cast), &type::floating};
            }
            if (lhs == &type::floating && *target == &type::integer) {
                return {std::move(cast), &type::integer};
            }
            logError(
                cast.span, "invalid operands",
                "type {} cannot be cast to type {}", lhs, *target
            );
            return {std::move(cast), type::Never{}};
        }

        if (!implementationScope.areSatisfied(
                {{Type{lhs},
                  Trait{traitScope["std::Into"], {Type{*target}}}}}
            )) {
            logError(
                cast.span, "invalid operands",
                "type {} cannot be cast to type {}", lhs, *target
            );
            return {std::move(cast), type::Never{}};
        }

        std::vector<ast::Expression> args{};
        auto ref = TraitMethodRef{
            Trait{traitScope["std::Into"], {Type{*target}}}};
        auto access = ast::PropertyAccess{
            Span{}, std::move(cast.lhs), {}, {}, 0, 0,
        };
        auto expr = ast::Expression{std::move(access)};
        auto call = ast::Call{
            Span{}, std::move(expr), {}, {std::move(args)}, {std::move(ref)},
        };

        auto resolvedReturnType = checkCall(
            traitScope["std::Into"]->signatures[0], call,
            {*target, lhs}, {}
        );
        call.type = resolvedReturnType;
        return {std::move(call), std::move(resolvedReturnType)};
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
            logError(
                match.span, "error", "body of match expression cannot be empty"
            );
            return type::Never{};
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

    Type operator()(lexer::CharLiteral& character) {
        return builtInTypes["char"];
    }

    Type operator()(lexer::FloatLiteral& cast) {
        return builtInTypes["float"];
    }

    Type operator()(lexer::StringLiteral& cast) {
        return type::Named{typeScope["std::String"], {}};
    }

    Type operator()(ast::IndexAccess& access) {
        auto lhs = (*this)(*access.lhs);
        auto index = (*this)(*access.index);
        type::Named* type = std::get_if<type::Named>(&lhs);
        if (!isNever(index) && index != builtInTypes["int"]) {
            logError(access.span, "error", "index must be an integer type");
        }
        if (!isNever(lhs) &&
            (!type || type->declaration != typeScope["std::Vector"])) {
            logError(access.span, "error", "only vectors can be indexed");
            return type::Never{};
        }
        if (isNever(index) || isNever(lhs)) {
            return type::Never{};
        }
        access.elementType = Type{type->typeArguments[0]};
        return std::move(type->typeArguments[0]);
    }

    Type operator()(ast::Return& _return) {
        auto type = _return.value ? (*this)(*_return.value)
                                  : Type{builtInTypes["null"]};
        if (type != currentReturnType) {
            logError(
                _return.value ? spanOf(*_return.value) : _return.span,
                "invalid return type", "expected {}, found {}",
                currentReturnType, type
            );
        }
        return type::Never{};
    }

    Type operator()(ast::Break& _break) {
        if (breakTypes) {
            breakTypes->push_back(
                _break.value ? (*this)(*_break.value) : builtInTypes["null"]
            );
        } else {
            logError(
                _break.span, "error", "break cannot be used outside of a loop"
            );
        }
        return type::Never{};
    }

    Type operator()(ast::Continue& _continue) {
        if (!breakTypes) {
            breakTypes->push_back(
                _continue.value ? (*this)(*_continue.value)
                                : builtInTypes["null"]
            );
        } else {
            logError(
                _continue.span, "error",
                "break cannot be used outside of a loop"
            );
        }
        return type::Never{};
    }

    Type operator()(ast::Prefix<"-">& op) {
        auto type = (*this)(*op.rhs);
        if (type != &type::integer && type != &type::floating) {
            logError(
                op.span, "invalid operand",
                "operator - only accepts int or float operands, found {}", type
            );
            return type::Never{};
        }
        return type;
    }

    Type operator()(ast::Prefix<"~">& op) {
        if (currentModule->moduleId != "std") {
            logError(
                op.span, "invalid operator",
                "this operator is reserved for internal use"
            );
        }
        return (*this)(*op.rhs);
    }

    Type tryWriteTo(ast::Prefix<"~">& op) {
        if (currentModule->moduleId != "std") {
            logError(
                op.span, "invalid operator",
                "this operator is reserved for internal use"
            );
        }
        return tryWriteTo(*op.rhs);
    }

    Type operator()(ast::Prefix<"!">& op) {
        auto type = (*this)(*op.rhs);
        if (type != &type::boolean) {
            logError(
                op.span, "invalid operand",
                "operator ! only accepts a boolean operand, found {}", type
            );
        }
        return &type::boolean;
    }

    Type operator()(ast::VectorElement& element) {
        return std::visit(*this, element.value);
    }

    Type operator()(ast::Spread& spread) {
        Type spreadeeType = (*this)(spread.value);
        if (isNever(spreadeeType))
            return type::Never{};

        type::Named* spreadee = std::get_if<type::Named>(&spreadeeType);
        if (!spreadee || spreadee->declaration != typeScope["std::Vector"]) {
            logError(
                spread.span, "invalid operand",
                "operator .. only accepts a vector operand, found {}",
                spreadeeType
            );
            return type::Never{};
        }
        return std::move(spreadee->typeArguments[0]);
    }

    Type operator()(ast::VectorLiteral& vector) {
        Type elementType = std::visit(
            match{
                [this](ast::VectorElementType& type) {
                    return resolve(type.typeName) || type::Never{};
                },
                [&](ast::VectorElements& elements) -> Type {
                    if (elements.elements.size() == 0) {
                        logError(
                            vector.span, "error",
                            "empty vector literals must specify the element "
                            "type"
                        );
                        return type::Never{};
                    }
                    Type firstElementType = (*this)(elements.elements[0]);
                    for (auto& element :
                         elements.elements | std::views::drop(1)) {
                        auto type = (*this)(element);
                        if (!isNever(firstElementType) && !isNever(type) &&
                            type != firstElementType) {
                            logError(
                                spanOf(element), "invalid element type",
                                "expected type {}, found", firstElementType,
                                type
                            );
                            logInfo(
                                spanOf(elements.elements[0]), "note",
                                "type {} inferred here", firstElementType
                            );
                        }
                    }
                    return firstElementType;
                },
            },
            vector.content
        );
        vector.elementType = Type{elementType};
        return type::Named{typeScope["std::Vector"], {std::move(elementType)}};
    }

    Type tryWriteTo(auto& target) {
        logError(
            spanOf(target), "invalid assignment",
            "only expressions refering to a modifiable lvalue may be assigned"
        );
        return type::Never{};
    }

    Type tryWriteTo(ast::IndexAccess& access) {
        Type lhsType = tryWriteTo(*access.lhs);
        Type idxType = (*this)(*access.index);
        type::Named* type = std::get_if<type::Named>(&lhsType);
        type::BuiltIn** idx = std::get_if<type::BuiltIn*>(&idxType);
        if (!isNever(idxType) && (!idx || *idx != builtInTypes["int"])) {
            logError(access.span, "error", "index must be an integer type");
        }
        if (!isNever(lhsType) &&
            (!type || type->declaration != typeScope["std::Vector"])) {
            logError(access.span, "error", "only vectors can be indexed");
            return type::Never{};
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
        return std::visit(FieldAccessor{*this, access}, lhsType);
    }

    Type tryWriteTo(ast::TupleFieldAccess& access) {
        Type lhsType = tryWriteTo(*access.lhs);
        return std::visit(
            match{
                [&](type::Tuple const& tuple) -> Type {
                    return Type{tuple.fields[access.propertyIdx]};
                },
                [&](type::Never const&) -> Type { return type::Never{}; },
                [&](auto const&) -> Type {
                    logError(
                        spanOf(*access.lhs), "error",
                        "expected tuple type as field access operand, found {}",
                        lhsType
                    );
                    return type::Never{};
                },
            },
            lhsType
        );
    }

    Type tryWriteTo(ast::Variable& variable) {
        if (variable.name.segments.size() > 1) {
            logError(
                variable.span, "error",
                "variables must reference the current module"
            );
            return type::Never{};
        }

        auto index = scope[variable.name.str()];
        if (!index) {
            logError(
                variable.span, "undeclared variable",
                "{} was not found in the current scope", variable.name.str()
            );
            return type::Never{};
        }
        variable.binding = index.value();
        if (scope[variable.binding].isMutable) {
            return scope[variable.binding].type;
        }
        logError(
            variable.span, "invalid assignment", "{} is a constant",
            variable.name.str()
        );
        return scope[variable.binding].type;
        // logInfo(variable.span, "note", "declared immutable here",
        // variable.name.str());
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
            logError(
                assignment.span, "invalid assignment",
                "type {} is not assignable to {}", lhsType, rhsType
            );
        }
        return std::move(lhsType);
    }

    std::pair<ast::Expression, Type>
    resolveVariableOrTypeExpression(ast::Variable&& variable) {
        if (variable.name.str() == "") {
            auto type = (*this)(variable);
            return {std::move(variable), type};
        }

        if (!scope[variable.name.str()]) {
            ast::NamedType typeName{Span{}, {}, variable.name, {}};
            auto type = resolve(typeName);
            if (!type) {
                return {std::move(variable), type::Never{}};
            }
            auto texpr = ast::TypeExpression{
                Span{},
                ast::TypeName{std::move(typeName)},
                *type
            };
            return {std::move(texpr), Type{type::Named{typeScope["Type"], {}}}};
        }
        auto type = (*this)(variable);
        return {std::move(variable), type};
    }

    Type operator()(ast::Variable& variable) {
        std::cout << "ENTER var" << std::endl;
        if (variable.name.segments.size() > 1) {
            logError(
                variable.span, "error",
                "variables must reference the current module"
            );
            return type::Never{};
        }
        if (variable.name.str() == "") {
            std::cout << "'SVAR " << variable.binding << " "
                      << scope.bindings.size() << std::endl;
            Type t = scope[variable.binding].type;
            std::cout << "sss" << std::endl;
            return t;
        }

        if (variable.name.str() == "this") {
            if (selfType) {
                variable.binding = 0;
                return Type{selfType.value()};
            }
            logError(
                variable.span, "error", "this can only be used within methods"
            );
            return type::Never{};
        }

        auto binding = scope[variable.name.str()];
        if (!binding) {
            logError(
                variable.span, "undeclared variable",
                "{} was not found in the current scope", variable.name.str()
            );
            return type::Never{};
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
        fmt::println("BLOKK {}", block);
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
                property.value = ast::Expression{ast::Variable{
                    property.span,
                    ast::Path{
                        property.span,
                        {lexer::Identifier{property.name, property.span}}}}};
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

        auto named = resolve(structure.name.value());
        if (!named) {
            return type::Never{};
        }
        type::Named type = std::get<type::Named>(*named);

        if (type.declaration->proto != literalType) {
            logError(
                structure.span, "error",
                "type {} does not match the prototype of type {}", literalType,
                *named
            );
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
                        std::cout << "checking PATT" << type << std::endl;
            check.scope.matchMutable = isMutable;
            (*this)(pattern.body, type);
            if (pattern.guard) {
                if (!allowGuards) {
                    check.logError(
                        spanOf(*pattern.guard), "error",
                        "guards are not allowed in this context"
                    );
                }

                auto guardType = check(*pattern.guard);
                if (guardType != check.builtInTypes["bool"]) {
                    check.logError(
                        spanOf(*pattern.guard), "error",
                        "guards must evaluate to a boolean, found {}", guardType
                    );
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
                                check.logError(
                                    spanOf(expression), "error",
                                    "guards must evaluate to a boolean, found "
                                    "{}",
                                    expressionType
                                );
                            }
                        } else {
                            if (!allowGuards) {
                                check.logError(spanOf(expression), "error", "guards are not allowed in this context");
                                return;
                            }
                            body.anonymous = true;
                            check.scope.enter();
                            std::cout << "G " << std::endl;
                            check.scope.add("", type, false);
                            std::cout << "G" << std::endl;
                            check.scope.matchedField.reset();
                            auto [newExpr, _] =
                                check.subsituteOverloadedOperator(
                                    ast::Binary<"==">{
                                        Span{}, std::move(expression),
                                        ast::Expression{ast::Variable{
                                            Span{},
                                            {},
                                            check.scope.bindings.size() - 1}}}
                                );
                            expression = std::move(newExpr);
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
                if (type) {
                    if (type != tupleType && !isNever(tupleType)) {
                        check.logError(
                            tuple.span, "error",
                            "cannot match type {} against {}", *type, tupleType
                        );
                    }
                    target = check.protoOf(std::get<type::Named>(*type));
                }
            }

            type::Tuple const* type = std::get_if<type::Tuple>(&target);
            if (!type && !isNever(tupleType)) {
                check.logError(
                    tuple.span, "error", "type {} is not a tuple", tupleType
                );
                return;
            }

            if (!isNever(tupleType) &&
                type->fields.size() != tuple.fields.size()) {
                check.logError(
                    tuple.span, "error", "expected {} fields, found {}",
                    tuple.fields.size(), type->fields.size()
                );
            }

            for (auto&& [field, type] :
                 views::zip(tuple.fields, type->fields)) {
                Type never{type::Never{}};
                (*this)(field.body, isNever(tupleType) ? never : type);
            }

            tuple.type = tupleType;
        }

        void
        operator()(ast::DestructureVector& vector, Type const& vectorType) {
            if (!allowGuards) {
                check.logError(
                    vector.span, "error",
                    "vector destructures not allowed in this context"
                );
            }
            type::Named const* type = std::get_if<type::Named>(&vectorType);
            if (!isNever(vectorType) &&
                (!type || type->declaration != check.typeScope["std::Vector"]
                )) {
                check.logError(
                    vector.span, "error", "expected a vector type, found {}",
                    vectorType
                );
                return;
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
                                check.logError(
                                    rest.span, "error",
                                    "only a single rest binding may appear in "
                                    "a vector pattern"
                                );
                            } else if (rest.binding) {
                                if (check.scope.back().second.find(rest.binding->value) != check.scope.back().second.end()) {
                                    check.logError(rest.binding->span, "duplicate binding", "{} has already been declared in this scope", rest.binding->value);
                                    return;
                                }
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
                if (type) {
                    if (*type != structureType && !isNever(structureType)) {
                        check.logError(
                            structure.span, "error",
                            "cannot match type {} against {}", *type,
                            structureType
                        );
                    }
                    target = check.protoOf(std::get<type::Named>(*type));
                }
            }
            type::Struct const* type = std::get_if<type::Struct>(&target);

            if (isNever(structureType)) {
                structure.type = structureType;
                return;
            }

            if (!type) {
                check.logError(
                    structure.span, "error", "{} is not struct type", target
                );
                return;
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
                if (!propertyName || propertyName->name.segments.size() != 1) {
                    check.logError(
                        spanOf(property.property.value), "error",
                        "properties must bind to a variable"
                    );
                    return;
                }
                auto prop = std::ranges::find(
                    type.properties, propertyName->name.str(),
                    &std::pair<std::string, Type>::first
                );
                if (prop == type.properties.end()) {
                    check.logError(
                        spanOf(property.property.value), "error",
                        "property {} was not found on type {}",
                        propertyName->name.str(), Type{type::Struct{type}}
                    );
                    return;
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
                        check.logError(
                            spanOf(property.property), "error",
                            "guards are not allowed in this context"
                        );
                        return;
                    }
                    if (type != check.builtInTypes["bool"]) {
                        check.logError(
                            spanOf(property.property), "error",
                            "guards must evaluate to a boolean, found {}", type
                        );
                        return;
                    }
                }
            }
        }
    };

    Type operator()(ast::LetBinding& let) {
        Type type = (*this)(let.initalValue);
        if (let.typeName) {
            auto expected = resolve(let.typeName.value());
            if (expected && !type.isAssignableTo(*expected)) {
                logError(
                    let.span, "error", "type {} is not assignable to {}", type,
                    *expected
                );
            }
            CheckPattern{*this, false, false}(let.binding, *expected);
            return Type{builtInTypes["null"]};
        }
        CheckPattern{*this, false, false}(let.binding, type);
        return Type{builtInTypes["null"]};
    }

    Type operator()(ast::VarBinding& var) {
        std::cout << "VAR BEGIN" << std::endl;
        Type type = (*this)(var.initalValue);
        if (var.typeName) {
            auto expected = resolve(var.typeName.value());
            if (expected && !type.isAssignableTo(*expected)) {
                logError(
                    var.span, "error", "type {} is not assignable to {}", type,
                    *expected
                );
            }
            CheckPattern{*this, false, true}(var.binding, *expected);
            return Type{builtInTypes["null"]};
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

    void expect(Span span, Type const& type, auto& node) {
        Type actual = (*this)(node);
        if (!actual.isAssignableTo(type)) {
            logError(span, "error", "expected type {}, found {}", type, actual);
        }
    }

    Type operator()(ast::If& ifExpr) {
        expect(
            spanOf(*ifExpr.condition), Type{builtInTypes["bool"]},
            *ifExpr.condition
        );

        std::optional<Type> result{type::Never{}};

        ++controlFlowDepth;
        auto trueType = (*this)(*ifExpr.trueBranch);
        mergeBranchTypes(result, trueType);
        if (ifExpr.falseBranch) {
            mergeBranchTypes(result, (*this)(**ifExpr.falseBranch));
        } else {
            mergeBranchTypes(result, &type::null);
        }
        ifExpr.hasSameTypeBranch = ifExpr.falseBranch && result.has_value();
        --controlFlowDepth;

        return ifExpr.hasSameTypeBranch ? std::move(*result)
                                        : Type{builtInTypes["null"]};
    }

    Type operator()(ast::While& loop) {
        std::vector<Type> breaks;
        auto previous = std::exchange(breakTypes, &breaks);
        std::visit(
            match{
                [&](ast::Expression& expression) {
                    expect(
                        spanOf(expression), Type{builtInTypes["bool"]},
                        expression
                    );
                    ++controlFlowDepth;
                    (*this)(*loop.body);
                    --controlFlowDepth;
                },
                [&](ast::LetBinding& binding) {
                    ++controlFlowDepth;
                    scope.enter();
                    auto type = (*this)(binding.initalValue);
                    CheckPattern{*this, true, false}(binding.binding, type);
                    (*this)(*loop.body);
                    scope.exit(controlFlowDepth);
                    --controlFlowDepth;
                },
                [&](ast::VarBinding& binding) {
                    scope.enter();
                    ++controlFlowDepth;
                    auto type = (*this)(binding.initalValue);
                    CheckPattern{*this, true, true}(binding.binding, type);
                    (*this)(*loop.body);
                    scope.exit(controlFlowDepth);
                    --controlFlowDepth;
                },
            },
            loop.condition->value
        );
        breakTypes = previous;
        for (auto& breakType : breaks) {
            if (!breakType.isAssignableTo(Type{builtInTypes["null"]})) {
                logError(
                    loop.span, "error", "expected null, found {}", breakType
                );
            }
        }
        return Type{builtInTypes["null"]};
    }

    Type checkCall(
        ast::Signature const& function, ast::Call& call,
        std::vector<Type> const& blockTypeArguments,
        std::vector<Type> const& typeArguments
    ) {
        std::cout << "checking..." << std::endl;

        if (call.argValues.size() != function.parameters.size()) {
            logError(
                function.span, "invalid parameter count",
                "expected {} parameters, found {}", function.parameters.size(),
                call.argValues.size()
            );
        }

        std::cout << "checking...2" << std::endl;
        if (typeArguments.size() !=
            function.typeParameterNames.size()) {
            logError(
                function.span, "invalid number of type parameters",
                "expected {} type parameters, found {}",
                function.typeParameterNames.size(),
                call.typeArgumentNames.size()
            );
        }
        std::cout << "checking..3" << std::endl;

        auto withres =
            with(blockTypeArguments, typeArguments, function.traitBounds);
        std::cout << "checking..4" << std::endl;

        for (size_t i :
             implementationScope.areSatisfiedAll(std::move(withres))) {
            logError(
                call.span, "trait bounds not satisfied",
                "type {} does not implement {}", withres[i].first,
                withres[i].second
            );
        }
        std::cout << "checking..5 " << blockTypeArguments.size() << typeArguments.size() << std::endl;

        return Type{
            with(blockTypeArguments, typeArguments, function.returnType)};
    }

    struct Inferer {
        TypeChecker& tc;
        std::vector<std::optional<Type>> blockTypeArgs{};
        std::vector<std::optional<Type>> funTypeArgs{};
        std::vector<ast::TypeParameter>* blockTypeParamNames;
        std::vector<ast::TypeParameter>* funTypeParamNames;

        void presetBlock(std::vector<Type> const& args) {
            for (size_t i = 0; i < args.size(); ++i) {
                blockTypeArgs[i] = args[i];
            }
        }
        void presetFun(std::vector<Type> const& args) {
            for (size_t i = 0; i < args.size(); ++i) {
                funTypeArgs[i] = args[i];
            }
        }

        bool match(Type const& first, Type const& second) {
            bool m = tryMatch(blockTypeArgs, funTypeArgs, first, second);
            std::cout << "done match " << std::endl;
            return m;
        }

        void checkArgs(
            ast::Signature& signature, std::vector<Type> const& tArgs,
            std::vector<Type> const& actual
        ) {
            presetFun(tArgs);
            if (signature.parameters.size() != actual.size()) {
                tc.logError(
                    signature.span, "invalid parameter count",
                    "expected {} parameters, found {}",
                    signature.parameters.size(), actual.size()
                );
            }
            for (auto&& [expected, found] :
                 views::zip(signature.parameters, actual)) {
                if (!this->match(expected.type, found)) {
                    tc.logError(
                        signature.span, "invalid parameter type",
                        "expected type {}, found {}", expected.type, found
                    );
                }
            }
        }

        std::pair<std::vector<Type>, std::vector<Type>> get() {
            std::vector<Type> blockTArgs{};
            for (auto&& [i, arg] : blockTypeArgs | views::enumerate) {
                if (!arg) {
                    tc.logError(
                        (*blockTypeParamNames)[i].span, "error",
                        "could not infer type for type parameter {}",
                        (*blockTypeParamNames)[i].name
                    );
                }
                blockTArgs.push_back(arg || type::Never{});
            }
            std::vector<Type> funTArgs{};
            for (auto&& [i, arg] : funTypeArgs | views::enumerate) {
                if (!arg) {
                    tc.logError(
                        (*funTypeParamNames)[i].span, "error",
                        "could not infer type for type parameter {}",
                        (*funTypeParamNames)[i].name
                    );
                }
                funTArgs.push_back(arg || type::Never{});
            }
            return {blockTArgs, funTArgs};
        }
    };

    bool isVisible(ast::FunctionDeclaration* func) {
        return func->annotation ? true : thingIsVisible(func);
    }

    bool isVisible(auto thing) {
        return thingIsVisible(thing);
    }

    bool isVisible(ast::TypeDeclaration* thing, ast::Visibility const& vis) {
        return thingIsVisible(thing, vis) && thingIsVisible(thing);
    }

    bool thingIsVisible(auto thing) {
        return thingIsVisible(thing, thing->visibility);
    }

    bool thingIsVisible(auto thing, ast::Visibility const& vis) {
        using Level = ast::Visibility::Level;
        switch (vis.level) {
        case Level::Public:
            return true;
        case Level::Internal:
            return packageName(thing->fullName) == currentModule->packageName();
        case Level::Private:
            return moduleName(thing->fullName) == currentModule->moduleId;
        }
    }

    Type operator()(ast::Call& call) {
        std::vector<Type> typeArguments{};
        for (auto& typeArg : call.typeArgumentNames) {
            typeArguments.push_back(resolve(typeArg) || type::Never{});
        }

        return std::visit(
            match{
                [&](ast::Variable const& variable) -> Type {
                    std::vector<Type> argTypes{};
                    for (auto& arg : call.argValues) {
                        argTypes.push_back((*this)(arg));
                    }
                    std::cout << "processing function call... " << std::endl;
                    auto name = prepend(variable.name);
                    if (!name)
                        return type::Never{};
                    auto func = funcScope.find(*name);
                    if (func == funcScope.end()) {
                        auto namedType = typeScope.find(*name);
                        if (namedType == typeScope.end()) {
                            logError(
                                variable.span, "undeclared function",
                                "no definition for function {} could be found",
                                variable.name
                            );
                            return type::Never{};
                        }
                        if (!isVisible(namedType->second)) {
                            logError(
                                variable.span, "error",
                                "type {} is not accessible", variable.name
                            );
                            return type::Never{};
                        }
                        if (!isVisible(namedType->second, namedType->second->protoVisibility)) {
                            logError(
                                variable.span, "error",
                                "prototype of type {} is not accessible", variable.name
                            );
                            return type::Never{};
                        }
                        if (namedType->second->typeParameterNames.size() !=
                            typeArguments.size()) {
                            logError(
                                call.span, "invalid number of type parameters",
                                "expected {} type parameters, found {}",
                                namedType->second->typeParameterNames.size(),
                                typeArguments.size()
                            );
                            return type::Never{};
                        }
                        auto constructedType = type::Named{
                            namedType->second,
                            std::move(typeArguments),
                        };
                        type::Tuple* tuple =
                            std::get_if<type::Tuple>(&namedType->second->proto);
                        if (!tuple) {
                            if (call.argValues.size() != 1) {
                                logError(
                                    call.span, "invalid initialization",
                                    "prototype initializers reqire a single "
                                    "value"
                                );
                                return constructedType;
                            }
                            std::cout << "get the proto " << std::endl;
                            auto proto = protoOf(constructedType);
                            std::cout << "get the proto2 " << std::endl;
                            if (!argTypes[0].isAssignableTo(proto)) {
                                logError(
                                    call.span, "invalid initialization",
                                    "type {} does not match the prototype of "
                                    "{}",
                                    argTypes[0],
                                    Type{type::Named{constructedType}}
                                );
                            }
                            std::cout << "get the prot3 " << std::endl;
                            call.target = type::Named{constructedType};
                            std::cout << "get the proto4 " << std::endl;
                            return std::move(Type{std::move(constructedType)});
                        }
                        if (tuple->fields.size() != call.argValues.size()) {
                            logError(
                                call.span, "invalid initialization",
                                "type {} expects {} fields, but {} were "
                                "provided",
                                Type{type::Named{constructedType}},
                                tuple->fields.size(), call.argValues.size()
                            );
                        }
                        for (auto&& [expected, found] :
                             views::zip(tuple->fields, argTypes)) {
                            if (!found.isAssignableTo(
                                    with(typeArguments, {}, expected)
                                )) {
                                logError(
                                    call.span, "invalid initialization",
                                    "type {} is not assignable to type {}",
                                    found, with(typeArguments, {}, expected)
                                );
                            }
                        }
                        call.target = type::Named{constructedType};
                        return std::move(Type{std::move(constructedType)});
                    }

                    if (!isVisible(func->second)) {
                        logError(
                            variable.span, "error",
                            "function {} is not accessible", variable.name
                        );
                        return type::Never{};
                    }

                    std::cout << "found func" << std::endl;

                    Inferer inferer{
                        *this,
                        {},
                        {},
                        nullptr,
                        &func->second->typeParameterNames};
                    inferer.funTypeArgs.resize(
                        func->second->typeParameterNames.size()
                    );
                        std::cout << "will match1" << std::endl;
                    inferer.presetFun(typeArguments);
                        std::cout << "will match2" << std::endl;
                    for (auto&& [param, arg] : views::zip(func->second->parameters, argTypes)) {
                        std::cout << "will match" << std::endl;
                        inferer.match(
                            param.type, arg
                        );
                    }

                    auto [_, tArgs] = inferer.get();
                    std::cout << "checking..15" << std::endl;
                    auto resolvedReturnType =
                        checkCall(*func->second, call, {}, tArgs);
                    std::cout << "checking..0" << std::endl;
                    call.target = Function{
                        func->second,
                        std::move(tArgs),
                    };
                    std::cout << "checking..7" << std::endl;
                    return std::move(resolvedReturnType);
                },
                [&](ast::PropertyAccess& method) -> Type {
                    std::cout << "processing method call... " << std::endl;
                    Type lhsType = (*this)(*method.lhs);
                    std::vector<Type> argTypes{};
                    for (auto& arg : call.argValues) {
                        argTypes.push_back((*this)(arg));
                    }
                    if (isNever(lhsType)) {
                        return type::Never{};
                    }
                    std::cout << "got lhs " << std::endl;

                    // regular method
                    if (!method.traitName) {
                        for (auto impl : implScope) {
                            if (auto named = std::get_if<type::Named>(&impl->type)) {
                                if (!named->declaration) continue;
                            }

                            Inferer inferer{
                                *this, {}, {}, &impl->typeParameterNames, nullptr};
                            inferer.blockTypeArgs.resize(
                                impl->typeParameterNames.size()
                            );
                            std::cout << "gonna " << impl->type << std::endl;
                            if (inferer.match(impl->type, lhsType)) {
                                auto func = std::ranges::find_if(
                                    impl->functions,
                                    [&](ast::Signature& sig) {
                                        return sig.name.value ==
                                            method.property.value;
                                    }
                                );
                                if (func != impl->functions.end()) {
                                    if (!isVisible(func)) {
                                        logError(
                                            call.span, "error",
                                            "method {} is not accessible",
                                            func->name
                                        );
                                        return type::Never{};
                                    }
                                    inferer.funTypeParamNames =
                                        &func->typeParameterNames;
                                    inferer.funTypeArgs.resize(
                                        func->typeParameterNames.size()
                                    );
                                    inferer.checkArgs(
                                        *func, typeArguments, argTypes
                                    );

                                    auto [blockTArgs, tArgs] = inferer.get();
                                    if (func->isMutation) {
                                        tryWriteTo(*method.lhs);
                                    }
                                    auto resolvedReturnType =
                                        checkCall(*func, call, blockTArgs, tArgs);
                                    method.propertyIdx = std::distance(
                                        impl->functions.begin(), func
                                    );
                                    call.target = ImplRef{
                                        impl,
                                        blockTArgs,
                                        tArgs,
                                    };
                                    return resolvedReturnType;
                                }
                            }
                        }
                    }
                    
                    ast::TraitDeclaration* traitDecl = nullptr;
                    std::optional<Trait> resolved{};
                    
                    if (method.traitName) {
                        if (auto trait = resolve(*method.traitName)) {
                            resolved = *trait;
                            traitDecl = resolved->declaration;
                        } else {
                            return type::Never{};
                        }
                    }

                    std::cout << "so trait methdod " << lhsType << std::endl;
                    // maybe it's a trait method
                    std::vector<std::pair<ast::TraitDeclaration*, size_t>>
                        candidates;

                    if (!resolved) {
                        for (auto [_, trait] : traitScope) {
                            auto traitMethod = std::ranges::find_if(
                                trait->signatures,
                                [&](ast::Signature& sig) {
                                    return sig.name.value == method.property.value;
                                }
                            );
                            if (traitMethod != trait->signatures.end()) {
                                candidates.push_back(
                                    {trait,
                                    std::distance(
                                        trait->signatures.begin(), traitMethod
                                    )}
                                );
                            }
                        }
                    } else {
                        auto traitMethod = std::ranges::find_if(
                            resolved->declaration->signatures,
                            [&](ast::Signature& sig) {
                                return sig.name.value == method.property.value;
                            }
                        );
                        if (traitMethod != resolved->declaration->signatures.end()) {
                            candidates.push_back(
                                {resolved->declaration,
                                std::distance(
                                    resolved->declaration->signatures.begin(), traitMethod
                                )}
                            );
                        }
                    }
                    
                    std::cout << "so trait methdod? " << candidates.size()
                            << std::endl;
                    if (candidates.size() == 0) {
                        logError(
                            method.property.span, "undefined method",
                            "no method named {} was found in the current scope",
                            method.property.value
                        );
                        return type::Never{};
                    }
                    if (candidates.size() > 1) {
                        logError(
                            method.property.span, "error",
                            "ambiguous call - multiple candidate trait methods "
                            "found"
                        );
                        return type::Never{};
                    }
                    traitDecl = candidates[0].first;
                    
                    if (!isVisible(traitDecl)) {
                        logError(
                            method.property.span, "error",
                            "trait {} is not accessible", traitDecl->name
                        );
                        return type::Never{};
                    }
                    auto idx = candidates[0].second;
                    Inferer inferer{
                        *this,
                        {},
                        {},
                        &traitDecl->typeParameterNames,
                        &traitDecl->signatures[idx].typeParameterNames};
                    inferer.blockTypeArgs.resize(
                        traitDecl->typeParameterNames.size()
                    );
                    inferer.funTypeArgs.resize(
                        traitDecl->signatures[idx].typeParameterNames.size()
                    );
                    inferer.presetFun(typeArguments);
                    if (resolved)
                        inferer.presetFun(resolved->typeArguments);
                    inferer.checkArgs(
                        traitDecl->signatures[idx], typeArguments, argTypes
                    );
                    auto [traitTypeArguments, tArgs] = inferer.get();

                    Trait trait{traitDecl, std::move(traitTypeArguments)};

                    for (size_t i :
                         implementationScope.areSatisfiedAll({{lhsType, trait}}
                         )) {
                        logError(
                            call.span, "trait bounds not satisfied",
                            "type {} does not implement {}", lhsType, trait
                        );
                    }

                    method.propertyIdx = idx;

                    traitTypeArguments.push_back(Type{lhsType});
                    if (trait.declaration->signatures[idx].isMutation) {
                        tryWriteTo(*method.lhs);
                    }
                    auto resolvedReturnType = checkCall(
                        trait.declaration->signatures[idx], call,
                        traitTypeArguments, tArgs
                    );
                    call.target = TraitMethodRef{
                        trait,
                        std::move(tArgs),
                    };
                    return std::move(resolvedReturnType);
                },
                [&](auto const&) -> Type {
                    logError(
                        spanOf(*call.lhs), "invalid call",
                        "only functions or methods may be called"
                    );
                    return type::Never{};
                },
            },
            call.lhs->value
        );
    }

    Type operator()(ast::PropertyAccess& access) {
        auto type = (*this)(*access.lhs);
        access.namedDepth = 0;
        return std::visit(FieldAccessor{*this, access}, type);
    }

    Type operator()(ast::TupleFieldAccess& access) {
        auto type = (*this)(*access.lhs);
        return std::visit(
            match{
                [&](type::Tuple const& tuple) -> Type {
                    return Type{tuple.fields[access.propertyIdx]};
                },
                [&](type::Never const&) -> Type { return type::Never{}; },
                [&](auto const&) -> Type {
                    logError(
                        spanOf(*access.lhs), "error",
                        "expected tuple type as field access operand, found {}",
                        type
                    );
                    return type::Never{};
                },
            },
            type
        );
    }

    void saveSignatures(ast::Implementation& impl) {
        size_t i = 0;
        for (auto& [_, parameter, bounds] :
             impl.typeParameterNames) {
            blockTypeParameters[parameter.value] = i;
            for (auto& traitName : bounds) {
                if (auto trait = resolve(traitName)) {
                    impl.traitBounds.push_back(
                        {Type{type::Parameter{true, i}}, std::move(*trait)}
                    );
                }
            }
            ++i;
        }

        auto targetType = resolve(impl.typeName);
        if (!targetType)
            return;

        for (size_t i = 0; i < impl.typeParameterNames.size(); ++i) {
            Type param{type::Parameter{true, i}};
            if (!containsType(*targetType, param)) {
                logError(impl.typeParameterNames[i].span, "error", "unbound type parameter {}", impl.typeParameterNames[i].name.value);
                blockTypeParameters.clear();
                return;
            }
        }

        impl.type = *targetType;
        std::cout << "got impl type " << impl.type << std::endl;
        selfType = Type{impl.type};

        scope.enter();
        scope.add("this", *selfType, false);
        std::ranges::for_each(
            impl.functions,
            [&](ast::FunctionDeclaration& func) {
                return (*this)(static_cast<ast::Signature&>(func));
            }
        );

        scope.exit(controlFlowDepth);
        selfType.reset();
        blockTypeParameters.clear();
        implScope.push_back(&impl);
    }

    void operator()(ast::Implementation& impl) {
        selfType = Type{impl.type};
        size_t i = 0;
        for (auto& [_, parameter, __] : impl.typeParameterNames) {
            blockTypeParameters[parameter.value] = i++;
        }
        implementationScope.currentBounds.block = &impl.traitBounds;

        scope.enter();
        scope.add("this", *selfType, false);
        std::ranges::for_each(impl.functions, [this](auto& arg) {
            (*this)(arg);
        });
        scope.exit(controlFlowDepth);
        selfType.reset();
        blockTypeParameters.clear();
    }

    void check(std::vector<Module*>& program) {
        typeScope["Type"] = new ast::TypeDeclaration{
            Span::null(),
            lexer::Identifier{"Type", Span::null()},
            {},
            {{}, ast::Visibility::Level::Public},
            ast::TypeName{},
            {{}, ast::Visibility::Level::Public},
            type::Struct{{{"size", Type{builtInTypes["int"]}}}},
        };
        modules = &program;

        for (auto& mod : program) {
            currentModule = mod;
            for (auto redeclaration :  collectFunctions(funcScope, mod->program, mod->moduleId)) {
                logError(redeclaration->span, "duplicate declaration", "function {} was already declared", redeclaration->name.value);
            }
            for (auto redeclaration :  collectNamedTypes(typeScope, mod->program, mod->moduleId)) {
                logError(redeclaration->span, "duplicate declaration", "type {} was already declared", redeclaration->name.value);
            }
            for (auto redeclaration :  collectTraitDeclarations(traitScope, mod->program, mod->moduleId)) {
                logError(redeclaration->span, "duplicate declaration", "trait {} was already declared", redeclaration->name.value);
            }
            for (auto& impl : mod->program.implementations) {
                for (auto& func : impl.functions) {
                    func.fullName =
                        fmt::format("{}::{}", mod->moduleId, func.name.value);
                }
            }
        }

        for (auto& mod : program) {
            currentModule = mod;
            for (auto& type : mod->program.typeDeclarations) {
                (*this)(type);
            }
        }
        for (auto& mod : program) {
            currentModule = mod;
            for (auto& trait : mod->program.traitDeclarations) {
                std::cout << "got " << trait.name.value << std::endl;
                (*this)(trait);
            }
        }
        for (auto& mod : program) {
            currentModule = mod;
            for (auto& trait : mod->program.traitImplementations) {
                save(trait);
            }
        }
        for (auto& mod : program) {
            currentModule = mod;
            for (auto& trait : mod->program.implementations) {
                saveSignatures(trait);
            }
        }
        for (auto& mod : program) {
            currentModule = mod;
            for (ast::Signature& function : mod->program.functions) {
                (*this)(function);
            }
        }
        for (auto& mod : program) {
            currentModule = mod;
            std::cout << "setting mod to " << mod->moduleId
                      << mod->imports.size() << std::endl;
            for (auto& function : mod->program.functions) {
                (*this)(function);
            }
        }
        for (auto& mod : program) {
            currentModule = mod;
            for (auto& trait : mod->program.implementations) {
                (*this)(trait);
            }
        }
        for (auto& trait : implementationScope.root.children) {
            currentModule = trait.impl->location;
            (*this)(*trait.impl);
        }

        for (auto& error : implementationScope.removeDuplicates()) {
            log.diagnostics.push_back(std::move(error));
        }
    }
};
