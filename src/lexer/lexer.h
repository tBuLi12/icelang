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
    Span span;
    bool operator==(Identifier const&) const = default;
};

template <String name> struct Keyword {
    Span span;
    bool operator==(Keyword const&) const = default;
};

template <String punctuation> struct Punctuation {
    Span span;
    bool operator==(Punctuation const&) const = default;
};

namespace literal {
struct Numeric {
    size_t value;
    Span span;

    bool operator==(Numeric const&) const = default;
};
struct String : std::string {
    Span span;

    String(std::string&& value, Span _span)
        : std::string{std::move(value)}, span(_span) {}
    bool operator==(String const&) const = default;
};
}; // namespace literal

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

constexpr auto escapeSequences = [] {
    std::array<std::optional<char>, 256> escapes{};
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
        Source _source, Span _span, std::string&& _punctuation
    )
        : logs::SpannedMessage{_span, _source}, punctuation(_punctuation) {}

    void printHeaderTo(std::ostream&) const override;
};

class UnrecognizedTokenError : public logs::SpannedMessage {
    char character;

  public:
    UnrecognizedTokenError(Source _source, Span _span, char _character)
        : logs::SpannedMessage{_span, _source}, character(_character) {}

    void printHeaderTo(std::ostream&) const override;
};

class UnknownEscapeSequence : public logs::SpannedMessage {
    char character;

  public:
    UnknownEscapeSequence(Source _source, Span _span, char _character)
        : logs::SpannedMessage{_span, _source}, character(_character) {}

    void printHeaderTo(std::ostream&) const override;
};

class NumericLiteralTooLarge : public logs::SpannedMessage {
  public:
    NumericLiteralTooLarge(Source _source, Span _span)
        : logs::SpannedMessage{_span, _source} {}

    void printHeaderTo(std::ostream&) const override;
};

struct NonTerminatedStringLiteral : public logs::SpannedMessage {
    NonTerminatedStringLiteral(Source _source, Span _span)
        : logs::SpannedMessage{_span, _source} {}

    void printHeaderTo(std::ostream&) const override;
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

        Span currentSpan() const {
            return Span{line, line, offset - column, column, column};
        }

        template <typename M, typename... Args>
            requires std::derived_from<M, logs::Message>
        void logMessage(Args&&... args) {
            diagnostics.push_back(
                std::make_unique<M>(source, std::forward<Args>(args)...)
            );
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
            if (punctuation.length() > prefix.length() &&
                currentCharacter() == punctuation[prefix.length()]) {
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
                return tryParseSinglePunctuation<prefix, punctuations>();
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

            logMessage<UnknownPunctuationError>(
                currentSpan().extendBack(prefix.length() + 1),
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
            return Identifier{word, currentSpan().extendBack(word.length())};
        }

        bool checkedAdd(size_t& first, size_t second) {
            if (std::numeric_limits<size_t>::max() - second < first) {
                return false;
            }
            first += second;
            return true;
        }

        bool checkedTimesTen(size_t& factor) {
            if (std::numeric_limits<size_t>::max() / 10 < factor) {
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
                size_t digit = static_cast<size_t>(getNextCharacter()) - '0';
                if (!checkedTimesTen(value) || !checkedAdd(value, digit)) {
                    discardNumericLiteralTail();
                    logMessage<NumericLiteralTooLarge>(beginSpan.to(currentSpan(
                    )));
                    break;
                }
            } while (std::isdigit(currentCharacter()));

            return literal::Numeric{value, beginSpan.to(currentSpan())};
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
                    logMessage<NonTerminatedStringLiteral>(
                        currentSpan().extendBack(1)
                    );
                    return {};
                } else if (character == '\\') {
                    escapeNextCharacter(literal);
                } else {
                    literal.push_back(character);
                }
            };
            getNextCharacter();

            return literal::String{
                std::move(literal), beginSpan.to(currentSpan())};
        }

        void escapeNextCharacter(std::string& literal) {
            int escapee = getNextCharacter();
            if (escapee == EOF) {
                logMessage<UnknownEscapeSequence>(
                    currentSpan().extendBack(1), ' '
                );
            } else {
                auto resolved = escapeSequences[escapee];
                if (resolved) {
                    literal.push_back(resolved.value());
                } else {
                    logMessage<UnknownEscapeSequence>(
                        currentSpan().extendBack(2), escapee
                    );
                }
            }
        }

        Token parseNextToken() {
            while (true) {
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
                    logMessage<UnrecognizedTokenError>(
                        currentSpan().extendBack(1),
                        static_cast<char>(getNextCharacter())
                    );
                }
            }
        }
    };
};
} // namespace lexer

#endif
