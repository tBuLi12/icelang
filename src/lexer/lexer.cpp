#include "lexer.h"

namespace lexer {
    void UnknownPunctuationError::printHeaderTo(std::ostream& stream) {
        stream << Red{"unknown punctuation"} << ": " << punctuation << std::endl;
    }

    void UnrecognizedTokenError::printHeaderTo(std::ostream& stream) {
        stream << Red{"unrecognized token"} << ": " << character << std::endl;
    }

    void UnknownEscapeSequence::printHeaderTo(std::ostream& stream) {
        stream << Red{"unknown escape sequence"} << ": " << character << std::endl;
    }

    void NumericLiteralTooLarge::printHeaderTo(std::ostream& stream) {
        stream << Red{"numeric literal too large"} << ": " << "cannot exceed " << std::num << std::endl;
    }
}
