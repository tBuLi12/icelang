#ifndef ICY_AST_H
#define ICY_AST_H

#include "../lexer/lexer.h"
#include "../utils.h"

#include <optional>
#include <vector>

using Lexer = lexer::WithPunctuations<
    ",", ".", "(", ")", "[", "]", "<", ">", "<=", ">=", "->", "{", "}", ":",
    ":<", "/", "*", "+", "-", ";", "=", "!", "==", "!=", "&&", "||", "..", "=>",
    "&">::
    Lexer<
        "fun", "type", "break", "return", "continue", "if", "while", "else",
        "match", "let", "var", "as", "is", "trait", "def">;

template <class T>
concept Token = contains<T, Lexer::Token>;

namespace ast {
struct Expression;

template <String op> struct Binary {
    Span span;
    UPtr<Expression> lhs;
    UPtr<Expression> rhs;
};

template <String op> struct Prefix {
    Span span;
    UPtr<Expression> rhs;
};

struct Condition;

struct If {
    Span span;
    UPtr<Condition> condition;
    UPtr<Expression> trueBranch;
    std::optional<UPtr<Expression>> falseBranch;
    bool hasSameTypeBranch;
};

struct While {
    Span span;
    UPtr<Condition> condition;
    UPtr<Expression> body;
};

struct TypeName;

struct TraitName {
    Span span;
    lexer::Identifier name;
    std::vector<TypeName> typeArgumentNames;
};

struct NamedType {
    Span span;
    lexer::Identifier name;
    std::vector<TypeName> typeArgumentNames;
};

struct TupleType {
    Span span;
    std::vector<TypeName> fields;
};

struct PropertyDeclaration;

struct StructType {
    Span span;
    std::vector<PropertyDeclaration> properties;
};

struct VectorType {
    Span span;
    UPtr<TypeName> elementType;
};

struct TypeName {
    std::variant<NamedType, TupleType, StructType, VectorType> value;
};

struct Cast {
    Span span;
    UPtr<Expression> lhs;
    TypeName typeName;
};

struct MatchCase;

struct Match {
    Span span;
    UPtr<Expression> scrutinee;
    std::vector<MatchCase> body;
};

struct BlockItem;

struct Block {
    Span span;
    std::vector<BlockItem> items;
    bool hasTrailingExpression;
};

struct PropertyDeclaration {
    Span span;
    std::string name;
    TypeName typeName;
};

struct TupleLiteral {
    Span span;
    std::vector<Expression> fields;
};

struct Property;

struct StructLiteral {
    Span span;
    std::vector<Property> properties;
    std::optional<lexer::Identifier> name;
};

struct VectorElementType {
    Span span;
    TypeName typeName;
};

struct VectorElement;
struct VectorElements {
    Span span;
    std::vector<VectorElement> elements;
};

struct VectorLiteral {
    Span span;
    std::variant<VectorElementType, VectorElements> content;
};

struct Call {
    Span span;
    UPtr<Expression> lhs;
    std::vector<TypeName> typeArgumentNames;
    std::vector<Expression> argValues;
};

struct PropertyAccess {
    Span span;
    UPtr<Expression> lhs;
    std::optional<TraitName> traitName;
    lexer::Identifier property;
};

struct TupleFieldAccess {
    Span span;
    UPtr<Expression> lhs;
    lexer::IntegerLiteral propertyIdx;
};

struct IndexAccess {
    Span span;
    UPtr<Expression> lhs;
    UPtr<Expression> index;
};

struct Variable {
    Span span;
    std::string name;
};

using ExpressionValue = std::variant<
    Variable, Block, lexer::IntegerLiteral, lexer::FloatLiteral,
    lexer::StringLiteral, TupleLiteral, StructLiteral, If, While, Match, Call,
    PropertyAccess, TupleFieldAccess, IndexAccess, Cast, Binary<"+">,
    Binary<"*">, Binary<"==">, Binary<"!=">, Binary<"&&">, Binary<"||">,
    Binary<">=">, Binary<"<=">, Binary<"<">, Binary<">">, Binary<"=">,
    Binary<"/">, Binary<"-">, Prefix<"-">, Prefix<"!">, VectorLiteral>;

struct Expression {
    ExpressionValue value;

    Expression(VariantElement<ExpressionValue> auto&& _value)
        : value(std::move(_value)){};
};

struct Spread {
    Span span;
    Expression value;
};

struct VectorElement {
    std::variant<Spread, Expression> value;
};

struct Destructure;
struct PropertyPattern;

struct DestructureStruct {
    Span span;
    std::vector<PropertyPattern> properties;
    std::optional<lexer::Identifier> name;
};

struct ElementPattern;
struct Pattern;

struct DestructureTuple {
    Span span;
    std::vector<Pattern> fields;
    std::optional<lexer::Identifier> name;
};

struct DestructureVector {
    Span span;
    std::vector<ElementPattern> items;
};

using DestructureValue =
    std::variant<DestructureStruct, DestructureTuple, DestructureVector>;

struct Destructure {
    DestructureValue value;

    Destructure(VariantElement<DestructureValue> auto&& _value)
        : value(std::move(_value)){};
    Destructure(std::vector<Pattern>&& _value);
    Destructure(DestructureValue&& _value);
};

// struct GuardedPattern {
//     Span span;
//     Destructure pattern;
//     std::optional<Expression> guard;
// };

using PatternValue = std::variant<Expression, Destructure>;

struct PatternBody {
    PatternValue value;

    PatternBody(VariantElement<PatternValue> auto&& _value)
        : value(std::move(_value)){};
};

struct Pattern {
    Span span;
    PatternBody body;
    std::optional<Expression> guard;

    Pattern(DestructureTuple&& destructure);
    Pattern(Span _span, PatternBody&& _body, std::optional<Expression>&& guard);
};

struct PropertyPattern {
    Span span;
    Expression property;
    std::optional<PatternBody> pattern;
};

struct RestElements {
    Span span;
    std::optional<lexer::Identifier> binding;
};

struct ElementPattern {
    std::variant<RestElements, Pattern> value;
};

struct Property {
    Span span;
    std::string name;
    std::optional<Expression> value;
};

struct Return {
    Span span;
    std::optional<Expression> value;
};

struct Continue {
    Span span;
    std::optional<Expression> value;
};

struct Break {
    Span span;
    std::optional<Expression> value;
};

struct LetBinding {
    Span span;
    Pattern binding;
    std::optional<TypeName> typeName;
    Expression initalValue;
};

struct VarBinding {
    Span span;
    Pattern binding;
    std::optional<TypeName> typeName;
    Expression initalValue;
};

struct BlockItem {
    std::variant<Expression, Break, Return, Continue, LetBinding, VarBinding>
        value;
};

struct MatchArm {
    std::variant<Break, Return, Continue, Expression> value;
};

struct MatchCase {
    Span span;
    Pattern pattern;
    MatchArm value;
};

struct Condition {
    std::variant<LetBinding, VarBinding, Expression> value;
};

struct Parameter {
    Span span;
    lexer::Identifier name;
    TypeName typeName;
};

struct TypeParameter {
    Span span;
    lexer::Identifier name;
    std::vector<TraitName> traitBounds;
};

struct Signature {
    Span span;
    lexer::Identifier name;
    std::vector<TypeParameter> typeParameterNames;
    std::vector<Parameter> parameters;
    std::optional<TypeName> returnTypeName;
};

struct FunctionDeclaration : Signature {
    Span span;
    Expression body;

    FunctionDeclaration(Span _span, Signature&& _signature, Expression&& _body)
        : Signature{std::move(_signature)}, span(_span),
          body(std::move(_body)) {}
};

struct TypeDeclaration {
    Span span;
    lexer::Identifier name;
    std::vector<TypeParameter> typeParameterNames;
    TypeName protoName;
};

struct TraitDeclaration {
    Span span;
    lexer::Identifier name;
    std::vector<TypeParameter> typeParameterNames;
    std::vector<Signature> signatures;
};

struct TraitImplementation {
    Span span;
    std::vector<TypeParameter> typeParameterNames;
    TypeName typeName;
    TraitName traitName;
    std::vector<FunctionDeclaration> implementations;
};

struct AST {
    std::vector<TypeDeclaration> typeDeclarations;
    std::vector<TraitDeclaration> traitDeclarations;
    std::vector<TraitImplementation> traitImplementations;
    std::vector<FunctionDeclaration> functions;
};
} // namespace ast


#ifdef __clang__
#define CLANG_RUNTIME(string)  fmt::runtime(string)
#else
#define CLANG_RUNTIME(string)  string
#endif

template <> struct fmt::formatter<ast::Property> : formatter<std::string> {
    auto format(ast::Property const& property, auto& ctx) const {
        return property.value ? fmt::format_to(
                                    ctx.out(), "{}:{}", property.name,
                                    property.value.value()
                                )
                              : fmt::format_to(ctx.out(), "{}", property.name);
    }
};

template <>
struct fmt::formatter<ast::PropertyDeclaration> : formatter<std::string> {
    auto format(ast::PropertyDeclaration const& property, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "{}:{}", property.name, property.typeName
        );
    }
};

template <> struct fmt::formatter<ast::Break> : formatter<std::string> {
    auto format(ast::Break const& _break, auto& ctx) const {
        if (_break.value) {
            return fmt::format_to(ctx.out(), "break({})", _break.value.value());
        }
        return fmt::format_to(ctx.out(), "break");
    }
};

template <> struct fmt::formatter<ast::Return> : formatter<std::string> {
    auto format(ast::Return const& _return, auto& ctx) const {
        if (_return.value) {
            return fmt::format_to(
                ctx.out(), "return({})", _return.value.value()
            );
        }
        return fmt::format_to(ctx.out(), "return");
    }
};

template <> struct fmt::formatter<ast::Continue> : formatter<std::string> {
    auto format(ast::Continue const& _continue, auto& ctx) const {
        if (_continue.value) {
            return fmt::format_to(
                ctx.out(), "continue({})", _continue.value.value()
            );
        }
        return fmt::format_to(ctx.out(), "continue");
    }
};

template <> struct fmt::formatter<ast::BlockItem> : formatter<std::string> {
    auto format(ast::BlockItem const& item, auto& ctx) const {
        std::visit(
            [&](auto const& item) { fmt::format_to(ctx.out(), "{}", item); },

            item.value
        );
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::LetBinding> : formatter<std::string> {
    auto format(ast::LetBinding const& letBinding, auto& ctx) const {
        fmt::format_to(ctx.out(), "let({}", letBinding.binding);
        if (letBinding.typeName) {
            fmt::format_to(ctx.out(), ":{}", letBinding.typeName.value());
        }
        fmt::format_to(ctx.out(), ",{})", letBinding.initalValue);
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::VarBinding> : formatter<std::string> {
    auto format(ast::VarBinding const& varBinding, auto& ctx) const {
        fmt::format_to(ctx.out(), "var({}", varBinding.binding);
        if (varBinding.typeName) {
            fmt::format_to(ctx.out(), ":{}", varBinding.typeName.value());
        }
        fmt::format_to(ctx.out(), ",{})", varBinding.initalValue);
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::Condition> : formatter<std::string> {
    auto format(ast::Condition const& condition, auto& ctx) const {
        std::visit(
            [&](auto const& value) {
                fmt::format_to(ctx.out(), CLANG_RUNTIME("{}"), value);
            },
            condition.value
        );
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::StructType> : formatter<std::string> {
    auto format(ast::StructType const& structure, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "struct({})", fmt::join(structure.properties, ",")
        );
    }
};

template <> struct fmt::formatter<ast::TupleType> : formatter<std::string> {
    auto format(ast::TupleType const& tuple, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "tuple({})", fmt::join(tuple.fields, ",")
        );
    }
};

template <> struct fmt::formatter<ast::NamedType> : formatter<std::string> {
    auto format(ast::NamedType const& named, auto& ctx) const {
        if (named.typeArgumentNames.size() > 0) {
            return fmt::format_to(
                ctx.out(), "{}<{}>", named.name,
                fmt::join(named.typeArgumentNames, ",")
            );
        }
        return fmt::format_to(ctx.out(), "{}", named.name);
    }
};

template <> struct fmt::formatter<ast::VectorType> : formatter<std::string> {
    auto format(ast::VectorType const& vector, auto& ctx) const {
        return fmt::format_to(ctx.out(), CLANG_RUNTIME("vector({})"), *vector.elementType);
    }
};

template <> struct fmt::formatter<ast::TypeName> : formatter<std::string> {
    auto format(ast::TypeName const& typeName, auto& ctx) const {
        std::visit(
            [&](auto const& name) { fmt::format_to(ctx.out(), "{}", name); },
            typeName.value
        );
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::TraitName> : formatter<std::string> {
    auto format(ast::TraitName const& trait, auto& ctx) const {
        if (trait.typeArgumentNames.size() > 0) {
            return fmt::format_to(
                ctx.out(), "{}<{}>", trait.name,
                fmt::join(trait.typeArgumentNames, ",")
            );
        }
        return fmt::format_to(ctx.out(), "{}", trait.name);
    }
};

template <String op>
struct fmt::formatter<ast::Binary<op>> : formatter<std::string> {
    auto format(ast::Binary<op> const& binary, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), CLANG_RUNTIME("{}({},{})"), op.characters, *binary.lhs,
            *binary.rhs
        );
    }
};

template <String op>
struct fmt::formatter<ast::Prefix<op>> : formatter<std::string> {
    auto format(ast::Prefix<op> const& prefix, auto& ctx) const {
        return fmt::format_to(ctx.out(), "{}({})", op.characters, *prefix.rhs);
    }
};

template <>
struct fmt::formatter<lexer::IntegerLiteral> : formatter<std::string> {
    auto format(lexer::IntegerLiteral const& integer, auto& ctx) const {
        return fmt::format_to(ctx.out(), "{}", integer.value);
    }
};

template <>
struct fmt::formatter<lexer::FloatLiteral> : formatter<std::string> {
    auto format(lexer::FloatLiteral const& floating, auto& ctx) const {
        return fmt::format_to(ctx.out(), "{}", floating.value);
    }
};

template <>
struct fmt::formatter<lexer::StringLiteral> : formatter<std::string> {
    auto format(lexer::StringLiteral const& string, auto& ctx) const {
        return fmt::format_to(ctx.out(), "\"{}\"", string.value);
    }
};

template <> struct fmt::formatter<ast::Variable> : formatter<std::string> {
    auto format(ast::Variable const& variable, auto& ctx) const {
        return fmt::format_to(ctx.out(), "{}", variable.name);
    }
};

template <> struct fmt::formatter<lexer::Identifier> : formatter<std::string> {
    auto format(lexer::Identifier const& variable, auto& ctx) const {
        return fmt::format_to(ctx.out(), "{}", variable.value);
    }
};

template <> struct fmt::formatter<ast::StructLiteral> : formatter<std::string> {
    auto format(ast::StructLiteral const& structure, auto& ctx) const {
        fmt::format_to(ctx.out(), "struct");
        if (structure.name) {
            fmt::format_to(ctx.out(), " {}", structure.name.value());
        }
        return fmt::format_to(
            ctx.out(), "({})", fmt::join(structure.properties, ",")
        );
    }
};

template <> struct fmt::formatter<ast::TupleLiteral> : formatter<std::string> {
    auto format(ast::TupleLiteral const& tuple, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "tuple({})", fmt::join(tuple.fields, ",")
        );
    }
};

template <> struct fmt::formatter<ast::Block> : formatter<std::string> {
    auto format(ast::Block const& block, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "block({}{})", fmt::join(block.items, ","),
            block.hasTrailingExpression ? "" : ","
        );
    }
};

template <>
struct fmt::formatter<ast::PropertyAccess> : formatter<std::string> {
    auto format(ast::PropertyAccess const& access, auto& ctx) const {
        if (access.traitName) {
            return fmt::format_to(
                ctx.out(), "property({},{},{})", access.traitName.value(),
                access.property, *access.lhs
            );
        }
        return fmt::format_to(
            ctx.out(), "property({},{})", access.property, *access.lhs
        );
    }
};

template <>
struct fmt::formatter<ast::TupleFieldAccess> : formatter<std::string> {
    auto format(ast::TupleFieldAccess const& access, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "field({},{})", access.propertyIdx, *access.lhs
        );
    }
};

template <> struct fmt::formatter<ast::IndexAccess> : formatter<std::string> {
    auto format(ast::IndexAccess const& access, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "index({},{})", *access.index, *access.lhs
        );
    }
};

template <> struct fmt::formatter<ast::Call> : formatter<std::string> {
    auto format(ast::Call const& call, auto& ctx) const {
        if (call.typeArgumentNames.size() > 0) {
            return fmt::format_to(
                ctx.out(), "call<{}>({},{})",
                fmt::join(call.typeArgumentNames, ","), *call.lhs,
                fmt::join(call.argValues, ",")
            );
        }
        return fmt::format_to(
            ctx.out(), "call({},{})", *call.lhs, fmt::join(call.argValues, ",")
        );
    }
};

template <> struct fmt::formatter<ast::VectorLiteral> : formatter<std::string> {
    auto format(ast::VectorLiteral const& vectorLiteral, auto& ctx) const {
        std::visit(
            [&](auto const& value) {
                fmt::format_to(ctx.out(), "vector({})", value);
            },
            vectorLiteral.content
        );
        return ctx.out();
    }
};

template <>
struct fmt::formatter<ast::VectorElements> : formatter<std::string> {
    auto format(ast::VectorElements const& elements, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "{}", fmt::join(elements.elements, ",")
        );
    }
};

template <> struct fmt::formatter<ast::VectorElement> : formatter<std::string> {
    auto format(ast::VectorElement const& element, auto& ctx) const {
        std::visit(
            [&](auto const& value) { fmt::format_to(ctx.out(), "{}", value); },
            element.value
        );
        return ctx.out();
    }
};

template <>
struct fmt::formatter<ast::VectorElementType> : formatter<std::string> {
    auto format(ast::VectorElementType const& elementType, auto& ctx) const {
        return fmt::format_to(ctx.out(), ":{}", elementType.typeName);
    }
};

template <> struct fmt::formatter<ast::Spread> : formatter<std::string> {
    auto format(ast::Spread const& spread, auto& ctx) const {
        return fmt::format_to(ctx.out(), "..{}", spread.value);
    }
};

template <> struct fmt::formatter<ast::If> : formatter<std::string> {
    auto format(ast::If const& ifExpr, auto& ctx) const {
        if (ifExpr.falseBranch) {
            return fmt::format_to(
                ctx.out(), "if({},{},{})", *ifExpr.condition,
                *ifExpr.trueBranch, **ifExpr.falseBranch
            );
        }
        return fmt::format_to(
            ctx.out(), "if({},{})", *ifExpr.condition, *ifExpr.trueBranch
        );
    }
};

template <> struct fmt::formatter<ast::While> : formatter<std::string> {
    auto format(ast::While const& whileExpr, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "while({},{})", *whileExpr.condition, *whileExpr.body
        );
    }
};

template <> struct fmt::formatter<ast::Expression> : formatter<std::string> {
    auto format(ast::Expression const& expression, auto& ctx) const {
        std::visit(
            [&](auto const& value) { fmt::format_to(ctx.out(), CLANG_RUNTIME("{}"), value); },
            expression.value
        );
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::Match> : formatter<std::string> {
    auto format(ast::Match const& match, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "match({},{})", *match.scrutinee,
            fmt::join(match.body, ",")
        );
    }
};

template <> struct fmt::formatter<ast::MatchCase> : formatter<std::string> {
    auto format(ast::MatchCase const& matchCase, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "{}=>{}", matchCase.pattern, matchCase.value
        );
    }
};

template <> struct fmt::formatter<ast::MatchArm> : formatter<std::string> {
    auto format(ast::MatchArm const& arm, auto& ctx) const {
        std::visit(
            [&](auto const& value) { fmt::format_to(ctx.out(), "{}", value); },
            arm.value
        );
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::Cast> : formatter<std::string> {
    auto format(ast::Cast const& cast, auto& ctx) const {
        return fmt::format_to(ctx.out(), "as({},{})", *cast.lhs, cast.typeName);
    }
};

template <> struct fmt::formatter<ast::Pattern> : formatter<std::string> {
    auto format(ast::Pattern const& pattern, auto& ctx) const {
        if (pattern.guard) {
            fmt::format_to(ctx.out(), "guard(");
        }
        std::visit(
            [&](auto const& value) { fmt::format_to(ctx.out(), "{}", value); },
            pattern.body.value
        );
        if (pattern.guard) {
            fmt::format_to(ctx.out(), ",{})", pattern.guard.value());
        }
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::RestElements> : formatter<std::string> {
    auto format(ast::RestElements const& pattern, auto& ctx) const {
        if (pattern.binding) {
            return fmt::format_to(ctx.out(), "..{}", pattern.binding.value());
        }
        return fmt::format_to(ctx.out(), "..");
    }
};

template <>
struct fmt::formatter<ast::ElementPattern> : formatter<std::string> {
    template <class FormatContext>
    auto format(ast::ElementPattern const& pattern, FormatContext& ctx) const {
        std::visit(
            [&](auto const& value) { fmt::format_to(ctx.out(), "{}", value); },
            pattern.value
        );
        return ctx.out();
    }
};

template <>
struct fmt::formatter<ast::PropertyPattern> : formatter<std::string> {
    template <class FormatContext>
    auto format(ast::PropertyPattern const& pattern, FormatContext& ctx) const {
        if (pattern.pattern) {
            fmt::format_to(
                ctx.out(), "{}:{}", pattern.property, pattern.pattern.value()
            );
        } else {
            fmt::format_to(ctx.out(), "{}", pattern.property);
        }
        return ctx.out();
    }
};

template <>
struct fmt::formatter<ast::PatternBody> : formatter<std::string> {
    template <class FormatContext>
    auto format(ast::PatternBody const& body, FormatContext& ctx) const {
        std::visit(
            [&](auto const& value) { fmt::format_to(ctx.out(), "{}", value); },
            body.value
        );
        return ctx.out();
    }
};


template <>
struct fmt::formatter<ast::DestructureStruct> : formatter<std::string> {
    auto format(ast::DestructureStruct const& structure, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "p-struct({})", fmt::join(structure.properties, ",")
        );
    }
};

template <>
struct fmt::formatter<ast::DestructureTuple> : formatter<std::string> {
    auto format(ast::DestructureTuple const& tuple, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "p-tuple({})", fmt::join(tuple.fields, ",")
        );
    }
};

template <>
struct fmt::formatter<ast::DestructureVector> : formatter<std::string> {
    auto format(ast::DestructureVector const& vector, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "p-vector({})", fmt::join(vector.items, ",")
        );
    }
};

template <> struct fmt::formatter<ast::Destructure> : formatter<std::string> {
    auto format(ast::Destructure const& pattern, auto& ctx) const {
        std::visit(
            [&](auto const& value) { fmt::format_to(ctx.out(), "{}", value); },
            pattern.value
        );
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::Parameter> : formatter<std::string> {
    auto format(ast::Parameter const& parameter, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "{}:{}", parameter.name, parameter.typeName
        );
    }
};

template <> struct fmt::formatter<ast::TypeParameter> : formatter<std::string> {
    auto format(ast::TypeParameter const& parameter, auto& ctx) const {
        if (parameter.traitBounds.size() > 0) {
            return fmt::format_to(
                ctx.out(), "{}:()", parameter.name,
                fmt::join(parameter.traitBounds, "&")
            );
        }
        return fmt::format_to(ctx.out(), "{}", parameter.name);
    }
};

template <>
struct fmt::formatter<std::vector<ast::TypeParameter>>
    : formatter<std::string> {
    auto
    format(std::vector<ast::TypeParameter> const& parameters, auto& ctx) const {
        if (parameters.size() > 0) {
            return fmt::format_to(
                ctx.out(), "<{}>", fmt::join(parameters, ",")
            );
        }
        return ctx.out();
    }
};

template <> struct fmt::formatter<ast::Signature> : formatter<std::string> {
    auto format(ast::Signature const& signature, auto& ctx) const {
        if (signature.returnTypeName) {
            return fmt::format_to(
                ctx.out(), "fun{}({}:{},{})", signature.typeParameterNames,
                signature.name, signature.returnTypeName.value(),
                fmt::join(signature.parameters, ",")
            );
        }
        return fmt::format_to(
            ctx.out(), "fun{}({},{})", signature.typeParameterNames,
            signature.name, fmt::join(signature.parameters, ",")
        );
    }
};

template <>
struct fmt::formatter<ast::FunctionDeclaration> : formatter<std::string> {
    auto format(ast::FunctionDeclaration const& function, auto& ctx) const {
        if (function.returnTypeName) {
            return fmt::format_to(
                ctx.out(), "fun{}({}:{},{},{})", function.typeParameterNames,
                function.name, function.returnTypeName.value(),
                fmt::join(function.parameters, ","), function.body
            );
        }
        return fmt::format_to(
            ctx.out(), "fun{}({},{},{})", function.typeParameterNames,
            function.name, fmt::join(function.parameters, ","), function.body
        );
    }
};

template <>
struct fmt::formatter<ast::TraitDeclaration> : formatter<std::string> {
    auto format(ast::TraitDeclaration const& trait, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "trait{}({},{})", trait.typeParameterNames, trait.name,
            fmt::join(trait.signatures, ",")
        );
    }
};

template <>
struct fmt::formatter<ast::TraitImplementation> : formatter<std::string> {
    auto
    format(ast::TraitImplementation const& implementation, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "def{}({},{},{})", implementation.typeParameterNames,
            implementation.typeName, implementation.traitName,
            fmt::join(implementation.implementations, ",")
        );
    }
};

template <>
struct fmt::formatter<ast::TypeDeclaration> : formatter<std::string> {
    auto format(ast::TypeDeclaration const& type, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "type{}({},{})", type.typeParameterNames, type.name,
            type.protoName
        );
    }
};

template <> struct fmt::formatter<ast::AST> : formatter<std::string> {
    auto format(ast::AST const& program, auto& ctx) const {
        return fmt::format_to(
            ctx.out(), "{}{}{}{}", fmt::join(program.functions, ","),
            fmt::join(program.traitDeclarations, ","),
            fmt::join(program.typeDeclarations, ","),
            fmt::join(program.traitImplementations, ",")
        );
    }
};

struct SpanPrinter {
    Source source;

    void printSpan(auto const& value)
        requires requires { value.span; }
    {
        std::cout << logs::SpannedMessage{
            source,
            value.span,
            "span of",
            fmt::format("{}", value),
        };
    }

    void printSpan(auto const& value) {
        std::visit([this](auto const& val) { printSpan(val); }, value.value);
    }

    void operator()(ast::Expression const& value) {
        std::visit([this](auto const& val) { (*this)(val); }, value.value);
    }

    void operator()(ast::Cast const& cast) {
        printSpan(cast);
        (*this)(*cast.lhs);
        (*this)(cast.typeName);
    }

    void operator()(ast::Pattern const& pattern) {
        std::visit(*this, pattern.body.value);
    }

    void operator()(ast::Destructure const& pattern) {
        std::visit(*this, pattern.value);
    }

    void operator()(ast::DestructureStruct const& structure) {
        printSpan(structure);
        std::ranges::for_each(structure.properties, *this);
    }

    void operator()(ast::DestructureTuple const& tuple) {
        printSpan(tuple);
        std::ranges::for_each(tuple.fields, *this);
    }

    void operator()(ast::DestructureVector const& vector) {
        printSpan(vector);
        std::ranges::for_each(vector.items, *this);
    }

    void operator()(ast::PropertyPattern const& pattern) {
        printSpan(pattern);
        printSpan(pattern.property);
        if (pattern.pattern) {
            printSpan(pattern.pattern.value());
        }
    }

    void operator()(ast::RestElements const& pattern) {
        printSpan(pattern);
        if (pattern.binding) {
            printSpan(pattern.binding.value());
        }
    }

    void operator()(ast::ElementPattern const& pattern) {
        std::visit(*this, pattern.value);
    }

    template <String op> void operator()(ast::Binary<op> const& binary) {
        printSpan(binary);
        (*this)(*binary.lhs);
        (*this)(*binary.rhs);
    }

    template <String op> void operator()(ast::Prefix<op> const& prefix) {
        printSpan(prefix);
        (*this)(*prefix.rhs);
    }

    void operator()(Token auto const& token) {
        printSpan(token);
    }

    void operator()(ast::Variable const& variable) {
        printSpan(variable);
    }

    void operator()(ast::StructLiteral const& structure) {
        printSpan(structure);
        std::ranges::for_each(structure.properties, *this);
    }

    void operator()(ast::TupleLiteral const& tuple) {
        printSpan(tuple);
        std::ranges::for_each(tuple.fields, *this);
    }

    void operator()(ast::Block const& block) {
        printSpan(block);
        std::ranges::for_each(block.items, *this);
    }

    void operator()(ast::PropertyAccess const& access) {
        printSpan(access);
        (*this)(*access.lhs);
        if (access.traitName) {
            (*this)(access.traitName.value());
        }
    }

    void operator()(ast::TupleFieldAccess const& access) {
        printSpan(access);
        (*this)(*access.lhs);
    }

    void operator()(ast::IndexAccess const& access) {
        printSpan(access);
        (*this)(*access.lhs);
    }

    void operator()(ast::Call const& call) {
        printSpan(call);
        (*this)(*call.lhs);
        std::ranges::for_each(call.typeArgumentNames, *this);
        std::ranges::for_each(call.argValues, *this);
    }

    void operator()(ast::VectorLiteral const& vectorLiteral) {
        printSpan(vectorLiteral);
        std::visit(
            match{
                [&](ast::VectorElementType const& type) {
                    printSpan(type);
                    (*this)(type.typeName);
                },
                [&](ast::VectorElements const& elements) {
                    printSpan(elements);
                    std::ranges::for_each(elements.elements, *this);
                }},
            vectorLiteral.content
        );
    }

    void operator()(ast::VectorElement const& element) {
        std::visit(*this, element.value);
    }

    void operator()(ast::Spread const& spread) {
        printSpan(spread);
        (*this)(spread.value);
    }

    void operator()(ast::If const& ifExpr) {
        printSpan(ifExpr);
        (*this)(*ifExpr.condition);
        (*this)(*ifExpr.trueBranch);
        if (ifExpr.falseBranch) {
            (*this)(*ifExpr.falseBranch.value());
        }
    }

    void operator()(ast::While const& whileExpr) {
        printSpan(whileExpr);
        (*this)(*whileExpr.condition);
        (*this)(*whileExpr.body);
    }

    void operator()(ast::TypeName const& typeName) {
        std::visit(
            match{
                [&](ast::StructType const& structure) {
                    printSpan(structure);
                    std::ranges::for_each(structure.properties, *this);
                },
                [&](ast::TupleType const& tuple) {
                    printSpan(tuple);
                    std::ranges::for_each(tuple.fields, *this);
                },
                [&](ast::VectorType const& vector) {
                    printSpan(vector);
                    (*this)(*vector.elementType);
                },
                [&](ast::NamedType const& named) {
                    printSpan(named);
                    printSpan(named.name);
                    std::ranges::for_each(named.typeArgumentNames, *this);
                }},
            typeName.value
        );
    }

    void operator()(ast::TraitName const& traitName) {
        printSpan(traitName);
        printSpan(traitName.name);
        std::ranges::for_each(traitName.typeArgumentNames, *this);
    }

    void operator()(ast::Condition const& condition) {
        std::visit([&](auto const& value) { (*this)(value); }, condition.value);
    }

    void operator()(ast::VarBinding const& varBinding) {
        printSpan(varBinding);
        (*this)(varBinding.binding);
        if (varBinding.typeName) {
            (*this)(varBinding.typeName.value());
        }
        (*this)(varBinding.initalValue);
    }

    void operator()(ast::LetBinding const& letBinding) {
        printSpan(letBinding);
        (*this)(letBinding.binding);
        if (letBinding.typeName) {
            (*this)(letBinding.typeName.value());
        }
        (*this)(letBinding.initalValue);
    }

    void operator()(ast::Break const& _break) {
        printSpan(_break);
        if (_break.value) {
            (*this)(_break.value.value());
        }
    }

    void operator()(ast::Return const& _return) {
        printSpan(_return);
        if (_return.value) {
            (*this)(_return.value.value());
        }
    }

    void operator()(ast::Continue const& _continue) {
        printSpan(_continue);
        if (_continue.value) {
            (*this)(_continue.value.value());
        }
    }

    void operator()(ast::BlockItem const& item) {
        std::visit((*this), item.value);
    }

    void operator()(ast::PropertyDeclaration const& property) {
        printSpan(property);
        (*this)(property.typeName);
    }

    void operator()(ast::Property const& property) {
        printSpan(property);
        if (property.value) {
            (*this)(property.value.value());
        }
    }

    void operator()(ast::Match const& match) {
        printSpan(match);
        (*this)(*match.scrutinee);
        std::ranges::for_each(match.body, *this);
    }

    void operator()(ast::MatchCase const& matchCase) {
        printSpan(matchCase);
        (*this)(matchCase.pattern);
        (*this)(matchCase.value);
    }

    void operator()(ast::MatchArm const& arm) {
        std::visit([this](auto const& value) { (*this)(value); }, arm.value);
    }

    void operator()(ast::TypeParameter const& parameter) {
        printSpan(parameter);
        (*this)(parameter.name);
        std::ranges::for_each(parameter.traitBounds, *this);
    }

    void operator()(ast::Parameter const& parameter) {
        printSpan(parameter);
        (*this)(parameter.name);
        (*this)(parameter.typeName);
    }

    void operator()(ast::AST const& ast) {
        std::ranges::for_each(ast.functions, *this);
        std::ranges::for_each(ast.typeDeclarations, *this);
        std::ranges::for_each(ast.traitDeclarations, *this);
        std::ranges::for_each(ast.traitImplementations, *this);
    }

    void operator()(ast::Signature const& signature) {
        printSpan(signature.name);
        std::ranges::for_each(signature.typeParameterNames, *this);
        std::ranges::for_each(signature.parameters, *this);
        if (signature.returnTypeName) {
            (*this)(signature.returnTypeName.value());
        }
    }

    void operator()(ast::FunctionDeclaration const& function) {
        printSpan(function);
        (*this)(static_cast<ast::Signature const&>(function));
        (*this)(function.body);
    }

    void operator()(ast::TypeDeclaration const& type) {
        printSpan(type);
        (*this)(type.name);
        std::ranges::for_each(type.typeParameterNames, *this);
        (*this)(type.protoName);
    }

    void operator()(ast::TraitDeclaration const& trait) {
        printSpan(trait);
        std::ranges::for_each(trait.typeParameterNames, *this);
        std::ranges::for_each(trait.signatures, *this);
    }

    void operator()(ast::TraitImplementation const& implementation) {
        printSpan(implementation);
        std::ranges::for_each(implementation.typeParameterNames, *this);
        (*this)(implementation.typeName);
        (*this)(implementation.traitName);
        std::ranges::for_each(implementation.implementations, *this);
    }
};

#endif
