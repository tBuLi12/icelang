#ifndef ICY_LEXER_H
#define ICY_LEXER_H

#include "../logs/logs.h"
#include "../string/string.h"
#include <array>
#include <fmt/format.h>
#include <iostream>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <variant>

namespace lexer {

struct Identifier {
    std::string value;
    Span span;
    bool operator==(Identifier const&) const = default;

    operator std::string&&() && {
        return std::move(value);
    }
};

template <String name> struct Keyword {
    Span span;
    bool operator==(Keyword const&) const = default;
};

template <String punctuation> struct Punctuation {
    Span span;
    bool operator==(Punctuation const&) const = default;
};

struct IntegerLiteral {
    size_t value;
    Span span;

    bool operator==(IntegerLiteral const&) const = default;

    operator size_t() {
        return value;
    }
};
struct FloatLiteral {
    double value;
    Span span;

    bool operator==(FloatLiteral const&) const = default;

    operator double() {
        return value;
    }
};
struct StringLiteral {
    std::string value;
    Span span;

    bool operator==(StringLiteral const&) const = default;
};

struct EndOfFile {
    Span span;
    bool operator==(EndOfFile const&) const = default;
};

template <class T>
std::optional<T>
operator||(std::optional<T>&& current, std::invocable auto&& tryNext) {
    if (current) {
        return current;
    }
    return tryNext();
}

constexpr std::array<std::pair<char, char>, 8> escapeSequences{{
    {'t', '\t'},
    {'\'', '\''},
    {'"', '"'},
    {'\\', '\\'},
    {'b', '\b'},
    {'r', '\r'},
    {'n', '\n'},
    {'0', '\0'},
}};

template <String... punctuations> struct WithPunctuations {
    template <String... keywords> class Lexer : public logs::MessageLog {

      public:
        using Token = std::variant<
            Keyword<keywords>..., Punctuation<punctuations>...,
            IntegerLiteral, FloatLiteral, StringLiteral, Identifier,
            EndOfFile>;

        Lexer(Source _source) : source(_source) {}

        Token next() {
            auto nextToken = getNextToken();
            lastTokenWasDot =
                std::holds_alternative<Punctuation<".">>(nextToken);
            return nextToken;
        }

        void resetInternalState() {
            lastTokenWasDot = false;
        }

        Source source;

      private:
        Token currentToken;
        size_t offset = 0;
        size_t column = 0;
        size_t line = 0;
        bool lastTokenWasDot = false;

        Token getNextToken() {
            while (currentCharacter() != EOF) {
                size_t offsetBeforeTrying = offset;
                discardLeadingWhitespace();

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
                    return EndOfFile{currentSpan().extendBack(1)};
                }

                if (offsetBeforeTrying == offset) {
                    logMessage(
                        currentSpan().extendBack(1),
                        "unrecognized token",
                        std::string{static_cast<char>(getNextCharacter())}
                    );
                }
            }
            return EndOfFile{currentSpan().extendBack(1)};
        }

        Span currentSpan() const {
            return Span{line, line, offset - column, column, column};
        }

        void logMessage(Span span, std::string_view type, std::string&& header) {
            diagnostics.push_back(logs::SpannedMessage{
                source,
                span,
                type,
                std::move(header),
            });
        }

        int getNextCharacter() {
            int nextCharacter = source.stream.get();

            if (nextCharacter != EOF) {
                ++offset;

                if (nextCharacter == '\n') {
                    column = 0;
                    ++line;
                } else {
                    ++column;
                }
            }

            return nextCharacter;
        }

        int currentCharacter() const {
            return source.stream.peek();
        }

        template <String prefix, String punctuation>
        std::optional<Token> tryParseSinglePunctuation() {
            if (currentCharacter() == punctuation[prefix.length()]) {
                getNextCharacter();

                static constexpr auto longerPrefix =
                    truncate<prefix.length() + 1>(punctuation);
                return tryParsePunctuationWithPrefix<longerPrefix>();
            }
            return std::optional<Token>{};
        }

        template <String prefix>
        std::optional<Token> tryParsePunctuationWithPrefix()
            requires((punctuations.startsWith(prefix) || ...))
        {
            auto token = (std::optional<Token>{} || ... || [this] {
                if constexpr (punctuations.length() > prefix.length() && punctuations.startsWith(prefix)) {
                    return tryParseSinglePunctuation<prefix, punctuations>();
                }
                return std::optional<Token>{};
            });

            if (token) {
                return token;
            }

            if constexpr (((punctuations == prefix) || ...)) {
                return Punctuation<prefix>{
                    currentSpan().extendBack(prefix.length())};
            }

            auto unknownPuncutation = std::string{prefix.characters} +
                                      static_cast<char>(getNextCharacter());

            logMessage(
                currentSpan().extendBack(prefix.length() + 1),
                "unknown punctuation",
                std::move(unknownPuncutation)
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

            if (currentCharacter() == '/') {
                getNextCharacter();
                if (currentCharacter() == '/') {
                    discardComment();
                    return {};
                } else {
                    return tryParsePunctuationWithPrefix<"/">();
                }
            }
            return tryParsePunctuationWithPrefix<"">();
        }

        void discardComment() {
            char nextCharacter = getNextCharacter();
            while (nextCharacter != '\n' && nextCharacter != EOF) {
                nextCharacter = getNextCharacter();
            }
        }

        void discardNumericLiteralTail() {
            while (std::isdigit(currentCharacter())) {
                getNextCharacter();
            }
        }

        void discardLeadingWhitespace() {
            while (std::isspace(currentCharacter())) {
                getNextCharacter();
            }
        }

        std::string getNextWord() {
            std::string word{};
            while (std::isalnum(currentCharacter())) {
                word.push_back(getNextCharacter());
            }

            return word;
        }

        std::optional<Token> tryParseKeywordOrIdentifier() {
            if (!std::isalpha(currentCharacter())) {
                return {};
            }

            auto word = getNextWord();
            auto keyword = (std::optional<Token>{} || ... || [&] {
                if (keywords == word) {
                    return std::optional<Token>{Keyword<keywords>{
                        currentSpan().extendBack(keywords.length())}};
                }
                return std::optional<Token>{};
            });

            if (keyword) {
                return keyword;
            }
            Span span = currentSpan().extendBack(word.length());
            return Identifier{std::move(word), span};
        }

        static bool tryPushDigit(size_t& number, size_t digit) {
            if (std::numeric_limits<size_t>::max() / 10 < number) {
                return false;
            }
            number *= 10;

            if (std::numeric_limits<size_t>::max() - number < digit) {
                return false;
            }
            number += digit;

            return true;
        }

        bool tryGetNextDigitOf(size_t& number) {
            size_t digit = static_cast<size_t>(getNextCharacter()) - '0';

            if (!tryPushDigit(number, digit)) {
                discardNumericLiteralTail();
                return false;
            }
            return true;
        }

        std::optional<Token> tryParseNumericLiteral() {
            if (!std::isdigit(currentCharacter())) {
                return {};
            }

            Span beginSpan = currentSpan();
            size_t value = 0;

            do {
                if (!tryGetNextDigitOf(value)) {
                    logMessage(
                        beginSpan.to(currentSpan()),
                        "numeric literal too large",
                        fmt::format("cannot exceed {}", std::numeric_limits<size_t>::max())
                    );
                }
            } while (std::isdigit(currentCharacter()));

            if (lastTokenWasDot || currentCharacter() != '.') {
                return IntegerLiteral{value, beginSpan.to(currentSpan())};
            }

            getNextCharacter();
            double divisor = 1;
            size_t fractionalValue = 0;
            while (std::isdigit(currentCharacter())) {
                divisor *= 10;
                if (!tryGetNextDigitOf(fractionalValue)) {
                    logMessage(
                        beginSpan.to(currentSpan()),
                        "numeric literal too large",
                        fmt::format("cannot exceed {}", std::numeric_limits<size_t>::max())
                    );
                }
            }

            return FloatLiteral{
                static_cast<double>(value) +
                    static_cast<double>(fractionalValue) / divisor,
                beginSpan.to(currentSpan()),
            };
        }

        std::optional<Token> tryParseStringLiteral() {
            if (currentCharacter() != '"') {
                return {};
            }

            auto beginSpan = currentSpan();

            getNextCharacter();
            std::string literal{};

            while (currentCharacter() != '"') {
                char character = getNextCharacter();

                if (character == EOF) {
                    logMessage(
                        currentSpan().extendBack(1),
                        "non-terminated string literal",
                        "expected \""
                    );
                    return {};
                } else if (character == '\\') {
                    escapeNextCharacter(literal);
                } else {
                    literal.push_back(character);
                }
            };
            getNextCharacter();

            return StringLiteral{
                std::move(literal), beginSpan.to(currentSpan())};
        }

        void escapeNextCharacter(std::string& literal) {
            int escapee = getNextCharacter();
            if (escapee == EOF) {
                logMessage(
                    currentSpan().extendBack(1),
                    "unknown escape sequence",
                    "\\(EOF)"
                );
            } else {
                auto resolved = std::ranges::find(
                    escapeSequences, escapee, &std::pair<char, char>::first
                );
                if (resolved != escapeSequences.end()) {
                    literal.push_back(resolved->second);
                } else {
                    logMessage(
                        currentSpan().extendBack(2),
                        "unknown escape sequence",
                        fmt::format("\\{}", static_cast<char>(escapee))
                    );
                }
            }
        }
    };
};
} // namespace lexer

#endif
