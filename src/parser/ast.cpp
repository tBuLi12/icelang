#include "./ast.h"

namespace ast {
Destructure::Destructure(PatternValue&& _value) : value(std::move(_value)){};
Destructure::Destructure(std::vector<Pattern>&& _value)
    : value(DestructureTuple{Span{}, std::move(_value)}){};
} // namespace ast
