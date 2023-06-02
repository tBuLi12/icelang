#include "./ast.h"

namespace ast {
Destructure::Destructure(DestructureValue&& _value)
    : value(std::move(_value)){};
Destructure::Destructure(std::vector<Pattern>&& _value)
    : value(DestructureTuple{Span{}, std::move(_value)}){};

Pattern::Pattern(DestructureTuple&& destructure)
    : span(destructure.span), body(Destructure{std::move(destructure)}){};
Pattern::Pattern(
    Span _span, PatternBody&& _body, std::optional<Expression>&& _guard
)
    : span(_span), body(std::move(_body)), guard(std::move(_guard)){};

Expression::Expression(ExpressionValue&& _value) : value(std::move(_value)){};

Variable::Variable(Path&& _path) : span(_path.span), name(std::move(_path)){};
Variable::Variable(Span _span, Path&& _path)
    : span(_span), name(std::move(_path)){};
Variable::Variable(Span _span, Path&& _path, size_t _binding)
    : span(_span), name(std::move(_path)), binding(_binding){};
} // namespace ast

std::ostream& operator<<(std::ostream& stream, ast::Path const& path) {
    return stream << fmt::format("{}", path);
}

std::string ast::Path::str() const {
    return fmt::format("{}", *this);
}
