#ifndef ICY_GRAMMAR_H
#define ICY_GRAMMAR_H

#include "./ast.h"

#define RULE(type, name)                                                       \
    constexpr auto name = Rule<type>{};                                        \
    template <> constexpr auto ruleFor<type>

namespace parser {
using namespace ast;

template <class L, class... ops> struct PrecedenceGroup {};
template <class T, class P> struct As {};

template <class T> struct Rule {
    using type = T;

    template <String... ops>
    consteval auto binaryGroup(Rule<lexer::Punctuation<ops>>...) {
        return Rule<PrecedenceGroup<
            T, As<Binary<ops>, std::tuple<lexer::Punctuation<ops>, T>>...>>{};
    }

    template <class... ops> consteval auto postfixGroup(Rule<ops>...) {
        return Rule<PrecedenceGroup<T, ops...>>{};
    }
};

template <class S, class T, bool emitTrailingSeparatorInfo = false>
struct SeparatedWith {};

template <String separator, class T>
auto consteval separatedWith(Rule<T> = {}) {
    return Rule<SeparatedWith<lexer::Punctuation<separator>, T>>{};
}

template <String separator, class T>
auto consteval separatedWithTrailing(Rule<T> = {}) {
    return Rule<SeparatedWith<lexer::Punctuation<separator>, T, true>>{};
}

template <class T, class P> auto consteval as(Rule<P> = {}) {
    return Rule<As<T, P>>{};
}

template <class T> struct OptionOrDefault {};

template <class T> auto consteval optionOrDefault(Rule<T>) {
    return Rule<OptionOrDefault<T>>{};
}

template <class M, class P> struct SingleOrMultiple {};

template <class M, class P> consteval auto singleOrMultipleAs(Rule<P>) {
    return Rule<SingleOrMultiple<M, P>>{};
}

template <class T> auto consteval option(Rule<T>) {
    return Rule<std::optional<T>>{};
}

template <class T> auto consteval list(Rule<T>) {
    return Rule<std::vector<T>>{};
}

template <class T> struct WithPostfixes {};

template <class T> auto consteval withPostfixes(Rule<T>) {
    return Rule<WithPostfixes<T>>{};
}

template <class T1, class T2> consteval auto operator+(Rule<T1>, Rule<T2>) {
    return Rule<std::tuple<T1, T2>>{};
}

template <class T, class... Ts>
consteval auto operator+(Rule<std::tuple<Ts...>>, Rule<T>) {
    return Rule<std::tuple<Ts..., T>>{};
}

template <class T1, class T2> consteval auto operator|(Rule<T1>, Rule<T2>) {
    return Rule<std::variant<T1, T2>>{};
}

template <class T, class... Ts>
consteval auto operator|(Rule<std::variant<Ts...>>, Rule<T>) {
    return Rule<std::variant<Ts..., T>>{};
}

template <String punctuation> consteval auto operator""_p() {
    return Rule<lexer::Punctuation<punctuation>>{};
}

template <String keyword> consteval auto operator""_kw() {
    return Rule<lexer::Keyword<keyword>>{};
}

// these extra structs are needed because the prefix rules are recursive
struct PrefixExpression {};
struct PrefixGuard {};
struct PrefixScrutinee {};
struct FunctionBody {};
struct FunctionWithVisibility {};
struct Mutability {};
struct TypeOpenUnion {};
struct TypeOpenTuple {};
struct TypeOpen {};

template <class T> constexpr bool ruleFor = false;

constexpr auto typeName = Rule<TypeName>{};
constexpr auto typeOpenUnion = Rule<TypeOpenUnion>{};
constexpr auto typeOpen = Rule<TypeOpen>{};

template <class T> consteval auto createPrecedenceHierarchy(Rule<T>) {
    return Rule<T>{}
        .postfixGroup(as<Cast>("as"_kw + typeName))
        .binaryGroup("*"_p, "/"_p)
        .binaryGroup("+"_p, "-"_p)
        .binaryGroup("<"_p, ">"_p, "<="_p, ">="_p)
        .binaryGroup("=="_p, "!="_p)
        .binaryGroup("&&"_p)
        .binaryGroup("||"_p);
}

template <class T> consteval auto createGuardPrecedenceHierarchy(Rule<T>) {
    return Rule<T>{}
        .binaryGroup("*"_p, "/"_p)
        .binaryGroup("+"_p, "-"_p)
        .binaryGroup("<"_p, ">"_p, "<="_p, ">="_p)
        .binaryGroup("=="_p, "!="_p)
        .binaryGroup("&&"_p)
        .binaryGroup("||"_p);
}

constexpr auto path = Rule<Path>{};
constexpr auto ident = Rule<lexer::Identifier>{};
constexpr auto integer = Rule<lexer::IntegerLiteral>{};
constexpr auto floating = Rule<lexer::FloatLiteral>{};
constexpr auto string = Rule<lexer::StringLiteral>{};
constexpr auto character = Rule<lexer::CharLiteral>{};
constexpr auto expression = Rule<Expression>{};
constexpr auto block = Rule<Block>{};
constexpr auto tuple = Rule<TupleLiteral>{};
constexpr auto vector = Rule<VectorLiteral>{};
constexpr auto pattern = Rule<Pattern>{};
constexpr auto destructure = Rule<Destructure>{};
constexpr auto prefixGuard = Rule<PrefixGuard>{};
constexpr auto prefixExpression = Rule<PrefixExpression>{};
constexpr auto visibility = Rule<Visibility>{};
constexpr auto mutability = Rule<Mutability>{};
constexpr auto destructureVariant = Rule<DestructureVariant>{};

// clang-format off

RULE(Variable, variable) = path;
RULE(Property, property) = ident + option(":"_p + expression);
RULE(StructLiteral, structLiteral) = "{"_p + separatedWith<",">(property) + "}"_p;
RULE(VariantLiteral, variantLiteral) = "."_p + ident + option("{"_p + expression + "}"_p);

constexpr auto scrutineePrimary = as<Expression>(
    variable | structLiteral | variantLiteral |
    (tuple | expression) | vector | character
);

RULE(PrefixScrutinee, scrutineePrefix) = as<Expression>(
    as<Prefix<"!">>("!"_p + Rule<PrefixScrutinee>{}) |
    as<Prefix<"-">>("-"_p + Rule<PrefixScrutinee>{}) | withPostfixes(scrutineePrimary)
);

constexpr auto scrutinee = as<Expression>(createPrecedenceHierarchy(scrutineePrefix));
constexpr auto openPattern = singleOrMultipleAs<DestructureTuple>(pattern);

RULE(MatchArm, matchArm)    = as<Break>("break"_kw + option(expression)) 
                            | as<Return>("return"_kw + option(expression)) 
                            | as<Continue>("continue"_kw + option(expression)) 
                            | expression;

RULE(MatchCase, matchCase) = openPattern + "=>"_p + matchArm;
RULE(Match, _match) = "match"_kw + scrutinee + "{"_p + separatedWith<",">(matchCase) + "}"_p;

constexpr auto primaryGuard = as<Expression>(
    variable | integer | string | floating | character
);

constexpr auto primaryExpression = as<Expression>(
    string 
    | (Rule<Variable>{} | structLiteral | variantLiteral) 
    | (structLiteral | block) 
    | (tuple | expression) 
    | variantLiteral
    | integer 
    | character
    | floating 
    | vector 
    | _match
);

constexpr auto postfixGuard = withPostfixes(primaryGuard);
constexpr auto postfixExpression = withPostfixes(primaryExpression);
constexpr auto orExpression = createPrecedenceHierarchy(prefixExpression);
constexpr auto guard = createGuardPrecedenceHierarchy(prefixGuard);

constexpr auto tupleExpression = as<Expression>(singleOrMultipleAs<TupleLiteral>(expression));

RULE(PropertyDeclaration, propertyDeclaration) = ident + ":"_p + typeOpenUnion;
RULE(VariantDeclaration, variantDeclaration) = ident + option(":"_p + typeName);
RULE(NamedType, namedType) = option("@"_p + as<Annotation>(ident)) + path + optionOrDefault("<"_p + separatedWith<",">(typeOpenUnion) + ">"_p);
RULE(VectorType, vectorType) = "["_p + typeOpen + "]"_p;

RULE(TraitName, traitName) = path + optionOrDefault("<"_p + separatedWith<",">(typeOpenUnion) + ">"_p);

RULE(Return, _return) = "return"_kw + option(tupleExpression);
RULE(Continue, _continue) = "continue"_kw + option(tupleExpression);
RULE(Break, _break) = "break"_kw + option(tupleExpression);
RULE(Parameter, parameter) = ident + ":"_p + typeOpenUnion;
RULE(LetBinding, letBinding) = "let"_kw + openPattern + option(":"_p + typeOpenUnion) + "="_p + tupleExpression;
RULE(VarBinding, varBinding) = "var"_kw + openPattern + option(":"_p + typeOpenUnion) + "="_p + tupleExpression;

RULE(BlockItem, blockItem)  = tupleExpression 
                            | _break 
                            | _return 
                            | _continue
                            | letBinding 
                            | varBinding;

RULE(Spread, spread) = ".."_p + expression;
RULE(VectorElement, vectorElement) = spread | expression;
RULE(VectorElements, vectorElements) = separatedWith<",">(vectorElement);
RULE(VectorElementType, vectorElementType) = ":"_p + typeOpen;
RULE(VectorLiteral, vectorLiteral) = "["_p + (vectorElementType | vectorElements) + "]"_p;

RULE(PropertyPattern, propertyPattern) = guard + option("as"_kw + destructure);
RULE(DestructureStruct, destructureStruct) = "{"_p + separatedWith<",">(propertyPattern) + "}"_p;
RULE(DestructureTuple, destructureTuple) = "("_p + separatedWith<",">(pattern) + ")"_p;
RULE(RestElements, restElements) = ".."_p + option(ident);
RULE(ElementPattern, elementPattern) = restElements | pattern;
RULE(DestructureVector, destructureVector) = "["_p + separatedWith<",">(elementPattern) + "]"_p;
RULE(Destructure, _destructure) = destructureStruct | destructureTuple | destructureVector | destructureVariant;
RULE(Condition, condition) = letBinding | varBinding | expression;
RULE(If, ifExpression) = "if"_kw + "("_p + condition + ")"_p + expression + option("else"_kw + expression);
RULE(While, whileExpression) = "while"_kw + "("_p + condition + ")"_p + expression;
RULE(Promotion, promotion) = "promote"_kw + ident;

RULE(PrefixExpression, _prefixExpression) = as<Expression>(
    as<Prefix<"!">>("!"_p + prefixExpression) 
    | as<Prefix<"-">>("-"_p + prefixExpression) 
    | as<Prefix<"~">>("~"_p + prefixExpression) 
    | ifExpression 
    | whileExpression 
    | postfixExpression
);

RULE(PrefixGuard, _prefixGuard) = as<Expression>(
    as<Prefix<"!">>("!"_p + prefixGuard) 
    | as<Prefix<"-">>("-"_p + prefixGuard) 
    | postfixGuard
);

RULE(TypeParameter, typeParameter) = ident + optionOrDefault("is"_kw + separatedWith<"&">(traitName));
constexpr auto typeParameterList = optionOrDefault("<"_p + separatedWith<",">(typeParameter) + ">"_p);

RULE(Signature, signature) = option("@"_p + as<Annotation>(ident)) + mutability + ident +  typeParameterList + "("_p + separatedWith<",">(parameter) + ")"_p + option(":"_p + typeOpen);
RULE(TypeDeclaration, typeDeclaration) = "type"_kw + ident + typeParameterList + visibility + typeOpen;
RULE(TraitDeclaration, traitDeclaration) = "trait"_kw + ident + typeParameterList + "{"_p + list(signature) + "}"_p;
RULE(FunctionBody, functionBody) = as<Expression>(as<Expression>("->"_p + tupleExpression) | as<Expression>(block));
RULE(FunctionDeclaration, functionDeclaration) = signature + functionBody;
RULE(Import, import) = "import"_kw + string + "as"_kw + ident;

constexpr auto traitBound = as<std::pair<TypeName, TraitName>>(typeName + "is"_kw + traitName);

// these rules are only used in the parsePostfixes function, but cannot be inlined because clang cries
constexpr auto indexAccess = "["_p + tupleExpression + "]"_p;
constexpr auto cast = "as"_kw + typeName;
constexpr auto call =
    optionOrDefault(":<"_p + separatedWith<",">(typeOpenUnion) + ">"_p) + "("_p +
    separatedWith<",">(expression) + ")"_p;
constexpr auto propertyAccess = option("<"_p + traitName + ">"_p) + ident;
constexpr auto postfixMatch = "match"_kw + "{"_p + separatedWith<",">(matchCase) + "}"_p;

// clang-format on

} // namespace parser

#endif
