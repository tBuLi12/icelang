#ifndef ICY_LOGS_H
#define ICY_LOGS_H

#define _ITERATOR_DEBUG_LEVEL 0

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

struct Span {
    size_t firstLine;
    size_t lastLine;
    size_t beginOffset;
    size_t beginHighlightOffset;
    size_t endHighlightOffset;

    Span to(Span other);
    static Span null();
    Span extendBack(size_t offset);
    Span first();
    bool operator==(Span const&) const = default;
};

std::ostream& operator<<(std::ostream& stream, Span const& span);

struct Source {
    std::istream& stream;
    std::string_view name;
};

static constexpr auto setRedColor = "\u001b[31;1m";
static constexpr auto setBlueColor = "\u001b[36;1m";
static constexpr auto resetColor = "\u001b[0m";

struct Red {
    static constexpr auto start = "\u001b[31;1m";
    std::string_view text;
};

struct Blue {
    static constexpr auto start = "\u001b[36;1m";
    std::string_view text;
};

std::ostream& operator<<(std::ostream& stream, Red redText);


namespace logs {
enum class Level {
    Error,
    Info
};

struct SpannedMessage {
    Source source;
    Span span;
    std::string_view type;
    std::string header;
    Level level = Level::Error;

    template <class Color = Red>
    struct Formatter;

    void printBodyTo(std::ostream&) const;
};

std::ostream& operator<<(std::ostream& stream, SpannedMessage const& redText);

class MessageLog {
  public:
    std::vector<logs::SpannedMessage> diagnostics{};

    void printDiagnosticsTo(std::ostream& stream) {
        for (auto& message : diagnostics) {
            stream << message;
        }
        diagnostics.clear();
    }

    bool errorsAreEmpty() const {
        return diagnostics.size() == 0;
    }
};
} // namespace logs

#endif
