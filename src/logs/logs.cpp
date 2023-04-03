#include "logs.h"

Span Span::to(Span Other) {
    return Span{
        firstLine,
        other.lastLine,
        beginOffset,
        beginHighlightOffset,
        other.endHighlightOffset,
    }
}

std::ostream& operator<<(std::ostream& stream, Red redText) {
    stream << "\u001b[31;1m" << redText.text << "\u001b[0m";
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
          marginWidth(numberOfDigits(span.lastLine)) {}

    void printLineNumber(size_t width, size_t lineNumber) {
        stream << lineNumber + 1
               << std::string(width - numberOfDigits(lineNumber + 1), ' ')
               << '|';
    }

    void printRedSquiggles() {
        stream << std::string(span.beginHighlightOffset, ' ') << Red{std::string(
                      span.endHighlightOffset - span.beginHighlightOffset, '^'
                  )};
    }

    void print() {
        std::string margin(marginWidth, ' ');

        stream << margin << '|' << std::endl;

        source.stream.seekg(span.beginOffset);

        size_t currentLine = span.firstLine;
        while (source.stream.peek() != EOF && currentLine <= span.lastLine) {
            stream << resetColor;
            printLineNumber(marginWidth, currentLine);

            if (currentLine > span.firstLine) {
                stream << redColor;
            }

            size_t column = 0;
            while (source.stream.peek() != EOF && source.stream.peek() != '\n'
            ) {
                char next = source.stream.get();

                if (currentLine == span.firstLine &&
                    column == span.beginHighlightOffset) {
                    stream << redColor;
                } else if (currentLine == span.lastLine && column == span.endHighlightOffset) {
                    stream << resetColor;
                }

                stream << next;

                ++column;
            }
            ++currentLine;
            source.stream.get();
            stream << '\n';
        }
        stream << margin << '|';

        if (span.firstLine == span.lastLine) {
            printRedSquiggles();
        }

        stream << std::endl
               << margin << "@ " << source.name << ':' << span.firstLine + 1
               << ':' << span.beginHighlightOffset << std::endl;
    }
};

namespace logs {
void SpannedMessage::printTo(std::ostream& stream) {
    printHeaderTo(stream);
    Formatter{stream, *this}.print();
}
} // namespace logs
