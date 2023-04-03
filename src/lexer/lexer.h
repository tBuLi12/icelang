#ifndef ICY_LEXER_H
#define ICY_LEXER_H

#include "../logs/logs.h"
#include "../string/string.h"
#include <array>
#include <iostream>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <variant>

namespace lexer {

struct Identifier : std::string {
    bool operator==(Identifier const&) const = default;
};

template <String name> struct Keyword {
    bool operator==(Keyword const&) const = default;
};

template <String punctuation> struct Punctuation {
    bool operator==(Punctuation const&) const = default;
};

namespace literal {
struct Numeric {
    size_t value;
    bool operator==(Numeric const&) const = default;
};
struct String : std::string {
    String(std::string&& value) : std::string{value} {}
    bool operator==(String const&) const = default;
};
}; // namespace literal

struct EndOfFile {
    bool operator==(EndOfFile const&) const = default;
};

template <class T> struct FoldOptional : std::optional<T> {
    FoldOptional<T> operator||(auto&& tryNext) {
        if (*this) {
            return *this;
        }
        return tryNext();
    }
};

template <class T> FoldOptional(std::optional<T>&&) -> FoldOptional<T>;

constexpr auto escapeSequences = [] {
    std::array<std::optional<char>, 256> escapes{'a'};
    escapes['t'] = '\t';
    escapes['\''] = '\'';
    escapes['"'] = '\"';
    escapes['\\'] = '\\';
    escapes['b'] = '\b';
    escapes['r'] = '\r';
    escapes['n'] = '\n';
    escapes['0'] = '\0';
    return escapes;
}();

class UnknownPunctuationError : public logs::SpannedMessage {
    std::string punctuation;

  public:
    UnknownPunctuationError(
        Span _span, Source _source, std::string&& _punctuation
    )
        : logs::SpannedMessage{_span, _source}, punctuation(_punctuation) {}

    void printHeaderTo(std::ostream&) override;
};

class UnrecognizedTokenError : public logs::SpannedMessage {
    char character;

  public:
    UnrecognizedTokenError(Span _span, Source _source, char _character)
        : logs::SpannedMessage{_span, _source}, character(_character) {}

    void printHeaderTo(std::ostream&) override;
};

class UnknownEscapeSequence : public logs::SpannedMessage {
    char character;

  public:
    UnknownEscapeSequence(Span _span, Source _source, char _character)
        : logs::SpannedMessage{_span, _source}, character(_character) {}

    void printHeaderTo(std::ostream&) override;
};

class NumericLiteralTooLarge : public logs::SpannedMessage {
  public:
    NumericLiteralTooLarge(Span _span, Source _source)
        : logs::SpannedMessage{_span, _source} {}

    void printHeaderTo(std::ostream&) override;
};

template <String... punctuations> struct WithPunctuations {
    template <String... keywords> class Lexer {
      public:
        using Token = std::variant<
            Keyword<keywords>..., Punctuation<punctuations>...,
            literal::Numeric, literal::String, Identifier, EndOfFile>;

        Lexer(Source _source) : source(_source) {
            currentToken = parseNextToken();
        }

        Token const& peek() const noexcept {
            return currentToken;
        }

        Token next() {
            return std::exchange(currentToken, parseNextToken());
        }

        void printDiagnosticsTo(std::ostream& stream) {
            for (auto& message : diagnostics) {
                message->printTo(stream);
            }
            diagnostics.clear();
        }

      private:
        Token currentToken;
        Source source;
        size_t offset = 0;
        size_t column = 0;
        size_t line = 0;
        std::vector<std::unique_ptr<logs::Message>> diagnostics{};

        Span currentSpan() {
            return Span {
                line, line, offset, column, column
            };
        }

        template <class M>
        void logMessage(M&& message) {
            diagnostics.push_back(std::make_unique<M>{std::move(message)});
        }

        int nextCharacter() {
            int next = source.stream.get();
            offset++;

            if (next == '\n') {
                column = 0;
                line++;
            } else {
                column++;
            }

            return next;
        }

        int currentCharacter() const {
            return source.stream.peek();
        }

        template <String prefix>
        std::optional<Token> tryParsePunctuationWithPrefix()
            requires((punctuations.startsWith(prefix) || ...))
        {
            auto token = (FoldOptional<Token>{} || ... || [this] {
                if (punctuations.length > prefix.length &&
                    currentCharacter() == punctuations[prefix.length]) {
                    nextCharacter();

                    static constexpr auto sliced =
                        punctuations.template slice<(prefix.length + 1)>();
                    return FoldOptional{
                        tryParsePunctuationWithPrefix<sliced>(),
                    };
                }
                return FoldOptional<Token>{};
            });

            if (token) {
                return token;
            }

            if constexpr (((punctuations == prefix) || ...)) {
                return Punctuation<prefix>{};
            }

            logMessage(
                    UnknownPunctuationError{
                    Span{
                        line,
                        line,
                        offset - column,
                        column - prefix.length,
                        column,
                    },
                    source,
                    std::string{prefix.characters} +
                        std::string{static_cast<char>(nextCharacter())},
                }
            );

            return {};
        }

        template <String prefix>
        std::optional<Token> tryParsePunctuationWithPrefix() {
            return {};
        }

        std::optional<Token> tryParsePunctuationOrComment() {
            if (!std::ispunct(currentCharacter()) ||
                currentCharacter() == '"') {
                return {};
            }

            if (currentCharacter() == '\\') {
                nextCharacter();
                if (currentCharacter() == '\\') {
                    discardComment();
                } else {
                    return tryParsePunctuationWithPrefix<"/">();
                }
            }
            return tryParsePunctuationWithPrefix<"">();
        }

        void discardComment() {
            while (nextCharacter() != '\n') {}
        }

        void discardNumericLiteralTail() {
            while (std::isdigit(currentCharacter())) {
                nextCharacter();
            }
        }

        void trimLeadingWhitespace() {
            while (std::isspace(currentCharacter())) {
                nextCharacter();
            }
        }

        std::string getNextWord() {
            std::string word{};
            while (std::isalnum(currentCharacter())) {
                word.push_back(nextCharacter());
            }
            return std::move(word);
        }

        std::optional<Token> tryParseKeywordOrIdentifier() {
            if (!std::isalpha(currentCharacter())) {
                return {};
            }

            auto word = getNextWord();
            auto keyword = (FoldOptional<Token>{} || ... || [&] {
                if (keywords == word) {
                    return FoldOptional<Token>{Keyword<keywords>{}};
                }
                return FoldOptional<Token>{};
            });

            if (keyword) {
                return keyword;
            }
            return Identifier{word};
        }

        bool checkedAdd(size_t& first, size_t second) {
            if (std::numeric_limits<size_t>::max - second < first) {
                return false;
            }
            first += second;
            return true;
        }

        bool checkedTimesTen(size_t& factor) {
            if (std::numeric_limits<size_t>::max / 10 < factor) {
                return false;
            }
            factor *= 10;
            return true;
        }

        std::optional<Token> tryParseNumericLiteral() {
            if (!std::isdigit(currentCharacter())) {
                return {};
            }

            Span beginSpan = currentSpan();
            size_t value = 0;

            do {
                size_t digit = static_cast<size_t>(nextCharacter()) - 48;
                if (!checkedTimesTen(value) || !checkedAdd(value, digit)) {
                    discardNumericLiteralTail();
                    logMessage(NumericLiteralTooLarge{
                        beginSpan.to(currentSpan),
                        source,
                    });
                    break;
                }
            } while (std::isdigit(currentCharacter()));

            return literal::Numeric{value};
        }

        std::optional<Token> tryParseStringLiteral() {
            if (currentCharacter() != '"') {
                return {};
            }

            nextCharacter();
            std::string literal{};

            while (currentCharacter() != '"') {
                char next = nextCharacter();
                if (next == '\\') {
                    char escapedCharacter = nextCharacter();
                    auto escape = escapeSequences[escapedCharacter];
                    if (escape) {
                        literal.push_back(escape.value());
                    } else {
                        logMessage(UnknownEscapeSequence{
                            Span{line, line, offset, column - 1, column},
                            Source{source},
                            escapedCharacter
                        });
                    }
                } else {
                    literal.push_back(next);
                }
            };
            nextCharacter();

            return literal::String{std::move(literal)};
        }

        Token parseNextToken() {
            while (true) {
                size_t offsetBeforeTrying = offset;
                trimLeadingWhitespace();

                auto token = tryParseKeywordOrIdentifier();
                if (token) {
                    return token.value();
                }

                token = tryParsePunctuationOrComment();
                if (token) {
                    return token.value();
                }

                token = tryParseNumericLiteral();
                if (token) {
                    return token.value();
                }

                token = tryParseStringLiteral();
                if (token) {
                    return token.value();
                }

                if (currentCharacter() == EOF) {
                    return EndOfFile{};
                }

                if (offsetBeforeTrying == offset) {
                    logMessage(UnrecognizedTokenError{
                        Span{line, line, offset, column - 1, column},
                        source,
                        static_cast<char>(nextCharacter()),
                    });
                }
            }
        }
    };
};
} // namespace lexer

#endif
