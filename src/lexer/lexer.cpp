#include "lexer.h"

namespace lexer {
void UnknownPunctuationError::printHeaderTo(std::ostream& stream) const {
    stream << Red{"unknown punctuation"} << ": " << punctuation << std::endl;
}

void UnrecognizedTokenError::printHeaderTo(std::ostream& stream) const {
    stream << Red{"unrecognized token"} << ": " << character << std::endl;
}

void UnknownEscapeSequence::printHeaderTo(std::ostream& stream) const {
    stream << Red{"unknown escape sequence"} << ": \\" << character
           << std::endl;
}

void NumericLiteralTooLarge::printHeaderTo(std::ostream& stream) const {
    stream << Red{"numeric literal too large"} << ": "
           << "cannot exceed " << std::numeric_limits<size_t>::max()
           << std::endl;
}
} // namespace lexer
