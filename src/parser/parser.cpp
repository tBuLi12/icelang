#ifndef ICY_PARSER_H
#define ICY_PARSER_H

#include "../lexer/lexer.h"
#include "../utils.h"
#include "./grammar.h"
#include <fmt/format.h>
#include <sstream>
#include <tuple>

// macro, because clang doesn't like the type alias template
#define PARSED(type) typename decltype(parse(Rule<type>{}))::value_type

namespace parser {
using namespace ast;

template <class T> constexpr bool mayDiscard = false;

template <String keyword>
constexpr bool mayDiscard<lexer::Keyword<keyword>> = true;

template <String punctuation>
constexpr bool mayDiscard<lexer::Punctuation<punctuation>> = true;

template <class T> struct Spanned {
    Span span;
    T value;

    using value_type = T;

    operator T&&() && {
        return std::move(value);
    }

    Spanned(Spanned<T>&&) = default;
    Spanned(T&& _value);
    Spanned(Span _span, T&& _value) : span(_span), value(std::move(_value)){};
    Spanned(){};
};

template <class... Ts> Span spanOf(std::variant<Ts...> const& variant);
template <class... Ts> Span& assignSpanOf(std::variant<Ts...>& variant);

template <class T> Span spanOf(Spanned<T> const& spanned) {
    return spanned.span;
}

template <class T> Span& assignSpanOf(Spanned<T>& spanned) {
    return spanned.span;
}

Span& assignSpanOf(auto& variant)
    requires requires {
        variant.value;
        requires !requires { variant.span; };
    }
{
    return assignSpanOf(variant.value);
}

Span spanOf(auto const& variant)
    requires requires {
        variant.value;
        requires !requires { variant.span; };
    }
{
    return spanOf(variant.value);
}

Span& assignSpanOf(auto& node)
    requires requires { node.span; }
{
    return node.span;
}

Span spanOf(auto const& node)
    requires requires { node.span; }
{
    return node.span;
}

template <class... Ts> Span spanOf(std::variant<Ts...> const& variant) {
    return std::visit([](auto const& item) { return spanOf(item); }, variant);
}
template <class... Ts> Span& assignSpanOf(std::variant<Ts...>& variant) {
    return std::visit([](auto& item) { return assignSpanOf(item); }, variant);
}

template <class T>
Spanned<T>::Spanned(T&& _value)
    : span(spanOf(_value)), value(std::move(_value)){};

template <class T> T make(auto&&... args) {
    return std::apply(
        [](auto&&... args) { return T{std::move(args)...}; },
        std::tuple_cat(std::tuple{std::move(args)}...)
    );
}

template <class T>
T make(auto&& spanned)
    requires requires(T t) {
        t.span;
        spanned.span;
    }
{
    return make<T>(spanned.span, spanned.value);
}

template <class T, class... Ts>
T make(std::variant<Ts...>&& variant)
    requires(!std::is_constructible_v<T, std::variant<Ts...> &&>)
{
    return std::visit(
        [](auto&& val) { return make<T>(std::move(val)); }, std::move(variant)
    );
}

constexpr bool isRule(auto) {
    return false;
}

template <class T> constexpr bool isRule(Rule<T>) {
    return true;
}

// template for checking wether a rule has an epsilon production
template <class T> constexpr bool nonNullable = true;
template <class T> constexpr bool nonNullable<std::optional<T>> = false;
template <class T> constexpr bool nonNullable<OptionOrDefault<T>> = false;
template <class... Ts>
constexpr bool nonNullable<std::tuple<Ts...>> = (nonNullable<Ts> || ...);
template <class... Ts>
constexpr bool nonNullable<std::variant<Ts...>> = (nonNullable<Ts> && ...);
template <class S, class T, bool b>
constexpr bool nonNullable<SeparatedWith<S, T, b>> = false;

// template representing the string to display when a given rule failed to parse
template <class T> constexpr String expected = T::expected;

template <String keyword>
constexpr String expected<lexer::Keyword<keyword>> = keyword;

template <String punctuation>
constexpr String expected<lexer::Punctuation<punctuation>> = punctuation;

template <>
constexpr String expected<lexer::StringLiteral> = "a string literal";
template <> constexpr String expected<lexer::Identifier> = "an identifier";
template <> constexpr String expected<Pattern> = "a pattern";
template <> constexpr String expected<Expression> = "an expression";
template <> constexpr String expected<TupleFieldAccess> = "a number";
template <> constexpr String expected<PropertyAccess> = "a property name";
template <> constexpr String expected<Block> = "a block";

template <class L, class... ops>
constexpr String expected<PrecedenceGroup<L, ops...>> = expected<L>;

template <class T, class... Ts>
constexpr String expected<std::tuple<T, Ts...>> =
    expected<T> + String{" or "} + expected<std::tuple<Ts...>>;

template <class T, class... Ts>
    requires(nonNullable<T>)
constexpr String expected<std::tuple<T, Ts...>> = expected<T>;

template <class T> constexpr String expected<std::optional<T>> = expected<T>;
template <class T> constexpr String expected<OptionOrDefault<T>> = expected<T>;
template <class T> constexpr String expected<std::vector<T>> = expected<T>;
template <class S, class T, bool b>
constexpr String expected<SeparatedWith<S, T, b>> = expected<T>;

template <class T, class R> constexpr String expected<As<T, R>> = expected<T>;
template <class T, class M> constexpr String expected<SingleOrMultiple<M, T>> = expected<T>;

template <class T, class... Ts>
constexpr String expected<std::variant<T, Ts...>> =
    expected<T> + ((" or " + expected<Ts>)+...);

template <class T>
    requires(isRule(ruleFor<T>))
constexpr String expected<T> = expected<typename decltype(ruleFor<T>)::type>;

template <class... Ts, std::invocable F>
auto operator||(std::optional<std::variant<Ts...>>&& current, F&& tryNext) {
    if (current) {
        return current;
    }

    auto next = tryNext();
    if (!next) {
        return std::optional<std::variant<Ts...>>{};
    }

    return std::optional{make<std::variant<Ts...>>(std::move(next.value()))};
}

template <class... Ts>
auto concatResult(Spanned<std::tuple<Ts...>>&& lhs, auto&& nextValue) {
    Spanned next{std::move(nextValue)};
    lhs.span = lhs.span.to(next.span);

    if constexpr (mayDiscard<decltype(next.value)>) {
        return lhs;
    } else {
        return Spanned{
            lhs.span,
            std::tuple_cat(
                std::move(lhs.value),
                std::tuple<decltype(next.value)>{std::move(next.value)}
            ),
        };
    }
}

template <class... Ts, std::invocable F>
auto operator&&(std::optional<Spanned<std::tuple<Ts...>>>&& lhs, F&& getNext)
    -> std::optional<
        decltype(concatResult(std::move(lhs.value()), getNext().value()))> {
    if (!lhs) {
        return {};
    }

    auto next = getNext();
    if (!next) {
        return {};
    }

    return std::optional{
        concatResult(std::move(lhs.value()), std::move(next.value())),
    };
}

struct UnexpectedToken {
    std::string_view expected;
};

class Parser : public logs::MessageLog {
  public:
    Parser(Source source) : lexer(source) {
        currentToken = lexer.next();
    }

    Lexer lexer;
    Lexer::Token currentToken;
    bool tokensWereConsumed = false;
    bool currentTokenMayBeUnexpected = false;

    Lexer::Token nextToken() {
        tokensWereConsumed = true;
        return std::exchange(currentToken, lexer.next());
    }

    template <Token T> std::optional<T> parse(Rule<T>) {
        if (std::holds_alternative<T>(currentToken)) {
            currentTokenMayBeUnexpected = false;
            return std::get<T>(nextToken());
        }
        return {};
    }

    void logUnexpectedToken(std::string_view expected) {
        if (!currentTokenMayBeUnexpected) {
            currentTokenMayBeUnexpected = true;
            diagnostics.push_back(logs::SpannedMessage{
                lexer.source,
                spanOf(currentToken),
                "unexpected token",
                fmt::format("expected {}", expected),
            });
        }
    }

    template <class R, class T> T expect(std::optional<T>&& parsed = {}) {
        if (parsed) {
            return std::move(parsed.value());
        }
        if constexpr (mayDiscard<T>) {
            logUnexpectedToken(expected<R>);
            return T{};
        }
        throw UnexpectedToken{expected<R>};
    }

    template <class R, class T>
    std::optional<T> handleMissingValue(std::optional<T>&& parsed) {
        if (tokensWereConsumed) {
            return expect<R>(std::move(parsed));
        }
        return parsed;
    }

    template <class... Ts> auto parse(Rule<std::tuple<Ts...>>) {
        bool tokensWerePreviouslyConsumed =
            std::exchange(tokensWereConsumed, false);

        auto parsed =
            (std::optional{Spanned{spanOf(currentToken), std::tuple<>{}}} &&
             ... &&
             [this] { return handleMissingValue<Ts>(parse(Rule<Ts>{})); });

        tokensWereConsumed = tokensWereConsumed || tokensWerePreviouslyConsumed;

        using Result = typename decltype(parsed)::value_type::value_type;

        if constexpr (std::tuple_size_v<Result> == 1) {
            if (parsed) {
                return std::optional{Spanned{
                    parsed->span,
                    std::move(std::get<0>(parsed->value)),
                }};
            }
            return std::optional<Spanned<std::tuple_element_t<0, Result>>>{};
        } else {
            return parsed;
        }
    }

    template <class... Ts> auto parse(Rule<std::variant<Ts...>>) {
        return (
            std::optional<VariantWithoutDuplicates<PARSED(Ts)...>>{} || ... ||
            [this] { return parse(Rule<Ts>{}); }
        );
    }

    template <class T> auto parse(Rule<std::optional<T>>) {
        auto parsed = parse(Rule<T>{});
        using Result = decltype(Spanned{std::move(parsed.value())}.value);
        if (!parsed) {
            return std::optional{
                Spanned{Span::null(), std::optional<Result>{}}};
        }

        Spanned spanned{std::move(parsed.value())};
        return std::optional{Spanned{
            spanned.span,
            std::optional<Result>{std::move(spanned.value)},
        }};
    }

    template <class T> auto parse(Rule<OptionOrDefault<T>>) {
        auto parsed = parse(Rule<T>{});
        if (parsed) {
            return parsed;
        }
        PARSED(T) empty{};
        assignSpanOf(empty) = Span::null();
        return std::optional{std::move(empty)};
    }

    template <class S, class T, bool emitTrailingSeparatorInfo>
    auto parse(Rule<SeparatedWith<S, T, emitTrailingSeparatorInfo>>) {
        std::vector<PARSED(T)> items{};
        Span span = spanOf(currentToken);
        bool hasTrailing = true;

        while (auto nextItem = parse(Rule<T>{})) {
            items.push_back(std::move(nextItem.value()));
            auto separator = parse(Rule<S>{});
            if (!separator) {
                span = span.to(spanOf(nextItem.value()));
                hasTrailing = false;
                break;
            };
            span = span.to(separator->span);
        }

        if constexpr (emitTrailingSeparatorInfo) {
            return std::optional{
                Spanned{span, std::pair{std::move(items), hasTrailing}},
            };
        } else {
            return std::optional{Spanned{span, std::move(items)}};
        }
    }

    template <class T> auto parse(Rule<std::vector<T>>) {
        std::vector<PARSED(T)> items{};
        while (auto nextItem = parse(Rule<T>{})) {
            items.push_back(std::move(nextItem.value()));
        }

        Span span = items.size() == 0
                        ? Span::null()
                        : spanOf(items.front()).to(spanOf(items.back()));

        return std::optional{Spanned{span, std::move(items)}};
    }

    template <class T>
    auto parse(Rule<T>)
        requires(isRule(ruleFor<T>))
    {
        return match{
            [this]<class R, class P>(Rule<As<R, P>>) {
                return parse(as<R>(Rule<P>{}));
            },
            [this](auto) { return parse(as<T>(ruleFor<T>)); },
        }(ruleFor<T>);
    }

    template <class T, class R> bool parseRhs(Expression& lhs, Rule<R> = {}) {
        auto rhs = parse(Rule<R>{});
        if (!rhs) {
            return false;
        }

        rhs->span = spanOf(lhs).to(spanOf(rhs.value()));
        lhs.value = make<T>(rhs->span, std::move(lhs), std::move(rhs->value));
        return true;
    }

    template <class T, class P> std::optional<T> parse(Rule<As<T, P>>) {
        auto parsed = parse(Rule<P>{});
        if (!parsed) {
            return {};
        }

        return make<T>(std::move(parsed.value()));
    }

    template <class T> std::optional<Expression> parse(Rule<WithPostfixes<T>>) {
        auto primary = parse(Rule<T>{});
        if (!primary) {
            return {};
        }
        parsePostfixes(primary.value());
        return std::move(primary.value());
    }

    void parsePostfixes(Expression& lhs) {
        while (parse(Rule<lexer::Punctuation<".">>{} /* using "."_p here causes
                                                          linker errors god
                                                        knows why (clang only)*/

               ) &&
                   // specifying the rhs rules here inline makes clang claim
                   // this is not a const context or sth
                   (parseRhs<TupleFieldAccess>(lhs, integer) ||
                    parseRhs<PropertyAccess>(lhs, propertyAccess) ||
                    (expect<
                         std::variant<TupleFieldAccess, PropertyAccess>,
                         int /*whatever, not void*/>(),
                     true)) ||
               parseRhs<Call>(lhs, call) ||
               parseRhs<IndexAccess>(lhs, indexAccess)) {}
        // apparently clang doesn't like this function for some reason (g++
        // doesn't complain)...
    }

    template <class L, class... Operator, class... Rhs>
    std::optional<Expression>
    parse(Rule<PrecedenceGroup<L, As<Operator, Rhs>...>>) {
        auto lhs = parse(Rule<L>{});
        if (!lhs) {
            return {};
        }
        while ((parseRhs<Operator, Rhs>(lhs.value()) || ...)) {}
        return lhs;
    }

    // terminates the recursive call below
    void continueParsingExpression(Expression& lhs, Rule<PrefixGuard>) {}

    template <class L, class... Operator, class... Rhs>
    void
    continueParsingExpression(Expression& lhs, Rule<PrecedenceGroup<L, As<Operator, Rhs>...>>) {
        continueParsingExpression(lhs, Rule<L>{});
        while ((parseRhs<Operator, Rhs>(lhs) || ...)) {}
    }

    std::optional<Expression> parse(Rule<Expression>) {
        auto parsed = parse(orExpression + option("="_p + expression));
        if (!parsed) {
            return {};
        }

        auto [lhs, rhs] = std::move(parsed->value);
        if (rhs) {
            return Binary<"=">{
                parsed->span,
                std::move(lhs),
                std::move(rhs.value()),
            };
        }
        return std::optional{std::move(lhs)};
    }

    std::optional<Expression>
    parse(Rule<std::variant<TupleLiteral, Expression>>) {
        auto parsed =
            parse("("_p + separatedWithTrailing<",">(expression) + ")"_p);
        if (!parsed) {
            return {};
        }
        auto [items, hasTrailing] = std::move(parsed->value);
        if (items.size() == 1 && !hasTrailing) {
            spanOf(items[0]) = parsed->span;
            return std::move(items[0]);
        }
        return TupleLiteral{parsed->span, std::move(items)};
    }

    std::optional<Expression>
    parse(Rule<std::variant<Variable, StructLiteral>>) {
        auto parsed =
            parse(ident + option("{"_p + separatedWith<",">(property) + "}"_p));
        if (!parsed) {
            return {};
        }

        auto [name, literal] = std::move(parsed->value);
        if (literal) {
            return Expression{StructLiteral{
                parsed->span,
                std::move(literal.value()),
                std::move(name),
            }};
        }
        return Expression{Variable{name.span, std::move(name)}};
    }

    std::optional<Pattern> parse(Rule<Pattern>) {
        auto name = parse(ident);
        if (name) {
            auto tuple = parse(destructureTuple);
            if (tuple) {
                tuple->name = std::move(name);
                tuple->span = name->span.to(tuple->span);
                return parseExplicitGuard(Destructure{std::move(tuple.value())}
                );
            }

            auto structure = parse(destructureStruct);
            if (structure) {
                structure->name = std::move(name);
                structure->span = name->span.to(structure->span);
                return parseExplicitGuard(Destructure{
                    std::move(structure.value())});
            }

            Expression guardPattern{
                Variable{name->span, std::move(name.value())}};
            continueParsingExpression(guardPattern, guard);
            return Pattern{spanOf(guardPattern), std::move(guardPattern), {}};
        }
        auto parsed = parse(destructure);
        if (parsed) {
            return parseExplicitGuard(std::move(parsed.value()));
        }

        auto expression = parse(guard);
        if (expression) {
            return Pattern{spanOf(expression.value()), std::move(expression.value()), {}};
        }

        return {};
    }

    Pattern parseExplicitGuard(PatternBody&& body) {
        auto guard = parse("if"_kw + orExpression);
        if (guard) {
            return Pattern{
                spanOf(body).to(guard->span), std::move(body),
                std::optional{std::move(guard->value)}};
        }
        return Pattern{spanOf(body), std::move(body), {}};
    }

    template <class M, class P>
    auto parse(Rule<SingleOrMultiple<M, P>>) -> std::optional<PARSED(P)> {
        auto parsedd = parse(separatedWithTrailing<",">(Rule<P>{}));
        auto [parsed, hasTrailing] = std::move(parsedd->value);

        if (parsed.size() == 0) {
            return {};
        }
        if (parsed.size() == 1 && !hasTrailing) {
            return PARSED(P){std::move(parsed[0])};
        }
        return PARSED(P){M{parsedd->span, std::move(parsed)}};
    }

    template <class T> auto mustParse(Rule<T>) {
        return expect<T>(parse(Rule<T>{}));
    }

    std::optional<Expression> parse(Rule<std::variant<StructLiteral, Block>>) {
        auto first = parse("{"_p + (blockItem | "}"_p));
        if (!first) {
            return {};
        }

        BlockItem* blockItem = std::get_if<BlockItem>(&first->value);
        if (!blockItem) {
            return StructLiteral{first->span};
        }

        Expression* firstExpression =
            std::get_if<Expression>(&blockItem->value);
        Variable* var = firstExpression
                            ? std::get_if<Variable>(&firstExpression->value)
                            : nullptr;

        if (var) {
            if (auto closingBrace = parse("}"_p)) {
                std::vector<Property> v{};
                v.push_back(Property{
                    var->span,
                    std::move(var->name),
                    std::optional<Expression>{},
                });
                return StructLiteral{
                    spanOf(first.value()).to(spanOf(closingBrace.value())),
                    std::move(v),
                    {}};
            }
            if (parse(":"_p)) {
                auto rest = mustParse(
                    expression +
                    optionOrDefault(","_p + separatedWith<",">(property)) +
                    "}"_p
                );
                auto [firstValue, restProps] = std::move(rest.value);
                restProps.insert(
                    restProps.begin(),
                    Property{
                        var->span.to(spanOf(firstValue)),
                        std::move(var->name),
                        std::optional{std::move(firstValue)},
                    }
                );
                return StructLiteral{
                    spanOf(first.value()).to(rest.span),
                    std::move(restProps),
                };
            }
        }

        return continueParsingBlock(
            spanOf(first.value()),
            BlockItem{std::move(std::get<BlockItem>(first->value))}
        );
    }

    std::optional<Block> parse(Rule<Block>) {
        auto parsed =
            parse("{"_p + separatedWithTrailing<";">(blockItem) + "}"_p);
        if (!parsed) {
            return {};
        }

        auto [items, hasTrailingSemicolon] = std::move(parsed->value);
        bool hasTrailingExpression = !hasTrailingSemicolon && items.size() > 0;
        return Block{parsed->span, std::move(items), hasTrailingExpression};
    }

    Block continueParsingBlock(Span openingBrace, BlockItem&& firstItem) {
        auto parsed = mustParse(
            optionOrDefault(";"_p + separatedWithTrailing<";">(blockItem)) +
            "}"_p
        );
        auto [items, hasTrailing] = std::move(parsed.value);
        Span span = openingBrace.to(parsed.span);
        items.emplace(items.begin(), std::move(firstItem));
        return Block{span, std::move(items), !hasTrailing};
    }

    auto parseDeclaration() {
        static constexpr auto declaration =
            functionDeclaration | typeDeclaration | traitDeclaration |
            traitImplementation | Rule<lexer::EndOfFile>{};
        while (true) {
            try {
                if (auto parsed = parse(declaration)) {
                    return std::move(parsed.value());
                }
            } catch (UnexpectedToken& error) {
                logUnexpectedToken(error.expected);
                continue;
            }
            logUnexpectedToken("a declaration");
            nextToken();
        }
    }

    AST parseProgram() {
        AST program{};

        bool endOfFileReached = false;
        while (!endOfFileReached) {
            std::visit(
                match{
                    [&](FunctionDeclaration&& declaration) {
                        program.functions.push_back(std::move(declaration));
                    },
                    [&](TypeDeclaration&& declaration) {
                        program.typeDeclarations.push_back(std::move(declaration
                        ));
                    },
                    [&](TraitDeclaration&& declaration) {
                        program.traitDeclarations.push_back(
                            std::move(declaration)
                        );
                    },
                    [&](TraitImplementation&& declaration) {
                        program.traitImplementations.push_back(
                            std::move(declaration)
                        );
                    },
                    [&](lexer::EndOfFile&&) { endOfFileReached = true; },
                },
                parseDeclaration()
            );
        }

        return program;
    }
};
} // namespace parser

std::pair<ast::AST, logs::MessageLog> parseProgram(Source source) {
    parser::Parser parser{source};
    return {
        parser.parseProgram(),
        std::move(static_cast<logs::MessageLog>(parser))};
}

#endif
