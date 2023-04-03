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
};

struct Source {
    std::istream& stream;
    std::string_view name;
};

struct Red {
    std::string_view text;
};

std::ostream& operator<<(std::ostream& stream, Red redText);

namespace logs {
class Message {
  public:
    virtual void printTo(std::ostream&) = 0;
    virtual ~Message() = default;
};

class SpannedMessage : public Message {
    struct Formatter;

  protected:
    Span span;
    Source source;

  public:
    SpannedMessage(Span _span, Source _source) : span(_span), source(_source) {}

    virtual void printHeaderTo(std::ostream&) = 0;
    void printTo(std::ostream&) override;
};
} // namespace logs

#endif
