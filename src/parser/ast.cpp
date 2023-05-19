#include "./ast.h"

namespace ast {
Destructure::Destructure(DestructureValue&& _value) : value(std::move(_value)){};
Destructure::Destructure(std::vector<Pattern>&& _value)
    : value(DestructureTuple{Span{}, std::move(_value)}){};

Pattern::Pattern(DestructureTuple&& destructure) : span(destructure.span), body(Destructure{std::move(destructure)}) {};
Pattern::Pattern(Span _span, PatternBody&& _body, std::optional<Expression>&& _guard) : span(_span), body(std::move(_body)), guard(std::move(_guard)) {};
} // namespace ast
