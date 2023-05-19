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

template <class T> constexpr bool ruleFor = false;

constexpr auto typeName = Rule<TypeName>{};

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

constexpr auto ident = Rule<lexer::Identifier>{};
constexpr auto integer = Rule<lexer::IntegerLiteral>{};
constexpr auto floating = Rule<lexer::FloatLiteral>{};
constexpr auto string = Rule<lexer::StringLiteral>{};
constexpr auto expression = Rule<Expression>{};
constexpr auto block = Rule<Block>{};
constexpr auto tuple = Rule<TupleLiteral>{};
constexpr auto vector = Rule<VectorLiteral>{};
constexpr auto pattern = Rule<Pattern>{};
constexpr auto destructure = Rule<Destructure>{};
constexpr auto prefixGuard = Rule<PrefixGuard>{};
constexpr auto prefixExpression = Rule<PrefixExpression>{};

// clang-format off

RULE(Property, property) = ident + option(":"_p + expression);
RULE(StructLiteral, structLiteral) = "{"_p + separatedWith<",">(property) + "}"_p;

constexpr auto scrutineePrimary = as<Expression>(
    as<Variable>(ident) | structLiteral |
    (tuple | expression) | vector
);

RULE(PrefixScrutinee, scrutineePrefix) = as<Expression>(
    as<Prefix<"!">>("!"_p + Rule<PrefixScrutinee>{}) |
    as<Prefix<"-">>("-"_p + Rule<PrefixScrutinee>{}) | withPostfixes(scrutineePrimary)
);

constexpr auto scrutinee = as<Expression>(createPrecedenceHierarchy(scrutineePrefix));

RULE(MatchArm, matchArm)    = as<Break>("break"_kw + option(expression)) 
                            | as<Return>("return"_kw + option(expression)) 
                            | as<Continue>("continue"_kw + option(expression)) 
                            | expression;

RULE(MatchCase, matchCase) = pattern + "=>"_p + matchArm;
RULE(Match, _match) = "match"_kw + scrutinee + "{"_p + separatedWith<",">(matchCase) + "}"_p;

constexpr auto primaryGuard = as<Expression>(
    as<Variable>(ident) | integer | string | floating
);

constexpr auto primaryExpression = as<Expression>(
    string 
    | (Rule<Variable>{} | structLiteral) 
    | (structLiteral | block) 
    | (tuple | expression) 
    | integer 
    | floating 
    | vector 
    | _match
);

constexpr auto postfixGuard = withPostfixes(primaryGuard);
constexpr auto postfixExpression = withPostfixes(primaryExpression);
constexpr auto orExpression = createPrecedenceHierarchy(prefixExpression);
constexpr auto guard = createPrecedenceHierarchy(prefixGuard);

constexpr auto tupleExpression = as<Expression>(singleOrMultipleAs<TupleLiteral>(expression));
constexpr auto openPattern = singleOrMultipleAs<DestructureTuple>(pattern);

RULE(PropertyDeclaration, propertyDeclaration) = ident + ":"_p + typeName;
RULE(NamedType, namedType) = ident + optionOrDefault("<"_p + separatedWith<",">(typeName) + ">"_p);
RULE(TupleType, tupleType) = "("_p + separatedWith<",">(typeName) + ")"_p;
RULE(StructType, structType) = "{"_p + separatedWith<",">(propertyDeclaration) + "}"_p;
RULE(VectorType, vectorType) = "["_p + typeName + "]"_p;
RULE(TypeName, _typeName) = namedType | tupleType | structType | vectorType;

RULE(TraitName, traitName) = ident + optionOrDefault("<"_p + separatedWith<",">(typeName) + ">"_p);

RULE(Return, _return) = "return"_kw + option(tupleExpression);
RULE(Continue, _continue) = "continue"_kw + option(tupleExpression);
RULE(Break, _break) = "break"_kw + option(tupleExpression);
RULE(Parameter, parameter) = ident + ":"_p + typeName;
RULE(LetBinding, letBinding) = "let"_kw + openPattern + option(":"_p + typeName) + "="_p + tupleExpression;
RULE(VarBinding, varBinding) = "var"_kw + openPattern + option(":"_p + typeName) + "="_p + tupleExpression;

RULE(BlockItem, blockItem)  = tupleExpression 
                            | _break 
                            | _return 
                            | _continue
                            | letBinding 
                            | varBinding;

RULE(Spread, spread) = ".."_p + expression;
RULE(VectorElement, vectorElement) = spread | expression;
RULE(VectorElements, vectorElements) = separatedWith<",">(vectorElement);
RULE(VectorElementType, vectorElementType) = ":"_p + typeName;
RULE(VectorLiteral, vectorLiteral) = "["_p + (vectorElementType | vectorElements) + "]"_p;

RULE(PropertyPattern, propertyPattern) = guard + option(":"_p + destructure);
RULE(DestructureStruct, destructureStruct) = "{"_p + separatedWith<",">(propertyPattern) + "}"_p;
RULE(DestructureTuple, destructureTuple) = "("_p + separatedWith<",">(pattern) + ")"_p;
RULE(RestElements, restElements) = ".."_p + option(ident);
RULE(ElementPattern, elementPattern) = restElements | pattern;
RULE(DestructureVector, destructureVector) = "["_p + separatedWith<",">(elementPattern) + "]"_p;
RULE(Destructure, _destructure) = destructureStruct | destructureTuple | destructureVector;
RULE(Condition, condition) = letBinding | varBinding | expression;
RULE(If, ifExpression) = "if"_kw + "("_p + condition + ")"_p + expression + option("else"_kw + expression);
RULE(While, whileExpression) = "while"_kw + "("_p + condition + ")"_p + expression;

RULE(PrefixExpression, _prefixExpression) = as<Expression>(
    as<Prefix<"!">>("!"_p + prefixExpression) 
    | as<Prefix<"-">>("-"_p + prefixExpression) 
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

RULE(Signature, signature) = "fun"_kw + ident +  typeParameterList + "("_p + separatedWith<",">(parameter) + ")"_p + option(":"_p + typeName);
RULE(TypeDeclaration, typeDeclaration) = "type"_kw + ident + typeParameterList + typeName;
RULE(TraitDeclaration, traitDeclaration) = "trait"_kw + ident + typeParameterList + "{"_p + list(signature) + "}"_p;
RULE(FunctionDeclaration, functionDeclaration) = signature + as<Expression>(as<Expression>("->"_p + tupleExpression) | as<Expression>(block));
RULE(TraitImplementation, traitImplementation) = "def"_kw + typeParameterList + typeName + "as"_kw + traitName + "{"_p + list(functionDeclaration) + "}"_p;

// these rules are only used in the parsePostfixes function, but cannot be inlined because clang cries
constexpr auto indexAccess = "["_p + tupleExpression + "]"_p;
constexpr auto cast = "as"_kw + typeName;
constexpr auto call =
    optionOrDefault(":<"_p + separatedWith<",">(typeName) + ">"_p) + "("_p +
    separatedWith<",">(expression) + ")"_p;
constexpr auto propertyAccess = option("<"_p + traitName + ">"_p) + ident;

// clang-format on

} // namespace parser

#endif
