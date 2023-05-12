#include "logs.h"

#include <cmath>

Span Span::to(Span other) {
    if (firstLine == std::numeric_limits<size_t>::max()) {
        return other;
    }

    if (other.firstLine == std::numeric_limits<size_t>::max()) {
        return *this;
    }

    return Span{
        firstLine,
        other.lastLine,
        beginOffset,
        beginHighlightOffset,
        other.endHighlightOffset,
    };
}

Span Span::extendBack(size_t offset) {
    return Span{
        firstLine,          lastLine,
        beginOffset,        beginHighlightOffset - offset,
        endHighlightOffset,
    };
}

Span Span::null() {
    return Span{std::numeric_limits<size_t>::max()};
}

std::ostream& operator<<(std::ostream& stream, Red redText) {
    return stream << setRedColor << redText.text << resetColor;
}

std::ostream& operator<<(std::ostream& stream, Span const& span) {
    return stream << span.firstLine << ' ' << span.lastLine << ' '
                  << span.beginHighlightOffset << ' ' << span.endHighlightOffset
                  << ' ' << span.beginOffset;
}

size_t numberOfDigits(size_t number) {
    return number > 1 ? std::log10(number) + 1 : 1;
}

struct logs::SpannedMessage::Formatter {
    std::ostream& stream;
    Source source;
    Span span;
    size_t marginWidth;

    Formatter(std::ostream& _stream, logs::SpannedMessage const& message)
        : stream(_stream), source(message.source), span(message.span),
          marginWidth(numberOfDigits(span.lastLine + 1)) {}

    void printLineNumber(size_t lineNumber) const {
        stream << lineNumber + 1
               << std::string(marginWidth - numberOfDigits(lineNumber + 1), ' ')
               << '|';
    }

    void printRedSquiggles() const {
        stream << std::string(span.beginHighlightOffset, ' ')
               << Red{std::string(
                      span.endHighlightOffset - span.beginHighlightOffset, '^'
                  )};
    }

    void printLine(size_t lineNumber) const {
        size_t column = 0;
        while (source.stream.peek() != EOF && source.stream.peek() != '\n') {
            char nextCharacter = source.stream.get();

            if (lineNumber == span.firstLine &&
                column == span.beginHighlightOffset) {
                stream << setRedColor;
            } else if (lineNumber == span.lastLine && column == span.endHighlightOffset) {
                stream << resetColor;
            }

            stream << nextCharacter;

            ++column;
        }
    }

    void print() const {
        std::string margin(marginWidth, ' ');

        stream << margin << '|' << std::endl;

        source.stream.clear();
        source.stream.seekg(span.beginOffset);

        size_t currentLine = span.firstLine;
        while (source.stream.peek() != EOF && currentLine <= span.lastLine) {
            stream << resetColor;

            printLineNumber(currentLine);

            if (currentLine > span.firstLine) {
                stream << setRedColor;
            }

            printLine(currentLine++);

            source.stream.get();
            stream << '\n';
        }
        stream << resetColor << margin << '|';

        if (span.firstLine == span.lastLine) {
            printRedSquiggles();
        }

        stream << std::endl
               << margin << "@ " << source.name << ':' << span.firstLine + 1
               << ':' << span.beginHighlightOffset + 1 << std::endl;
    }
};

namespace logs {
std::ostream& operator<<(std::ostream& stream, SpannedMessage const& message) {
    stream << Red{message.type} << ": " << message.header << std::endl;
    message.printBodyTo(stream);
    return stream;
}

void SpannedMessage::printBodyTo(std::ostream& stream) const {
    Formatter{stream, *this}.print();
}
} // namespace logs
