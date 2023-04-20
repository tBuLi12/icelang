#ifndef ICY_LOGS_H
#define ICY_LOGS_H

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
    Span extendBack(size_t offset);
    bool operator==(Span const&) const = default;
};

std::ostream& operator<<(std::ostream& stream, Span const& span);

struct Source {
    std::istream& stream;
    std::string_view name;
};

static constexpr auto setRedColor = "\u001b[31;1m";
static constexpr auto resetColor = "\u001b[0m";

struct Red {
    std::string_view text;
};

std::ostream& operator<<(std::ostream& stream, Red redText);

namespace logs {
class Message {
  public:
    virtual void printTo(std::ostream&) const = 0;
    virtual ~Message() = default;
};

class SpannedMessage : public Message {
  public:
    SpannedMessage(Span _span, Source _source) : span(_span), source(_source) {}

    virtual void printHeaderTo(std::ostream&) const = 0;
    void printTo(std::ostream&) const override;

    Span span;

  protected:
    Source source;

  private:
    struct Formatter;
};
} // namespace logs

#endif
