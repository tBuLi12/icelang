#include <sstream>

#include "lexer.h"
#include <gtest/gtest.h>

TEST(LexerTest, Keywords) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun", "if">;
    std::stringstream source{", fun word if other"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        Lexer::Token{lexer::Punctuation<",">{(Span{0, 0, 0, 0, 1})}}
    );
    EXPECT_EQ(
        lexer.next(), Lexer::Token{lexer::Keyword<"fun">{(Span{0, 0, 0, 2, 5})}}
    );
    EXPECT_EQ(
        lexer.next(),
        Lexer::Token{(lexer::Identifier{"word", (Span{0, 0, 0, 6, 10})})}
    );
    EXPECT_EQ(
        lexer.next(),
        Lexer::Token{lexer::Keyword<"if">{(Span{0, 0, 0, 11, 13})}}
    );
    EXPECT_EQ(
        lexer.next(),
        Lexer::Token{(lexer::Identifier{"other", Span{0, 0, 0, 14, 19}})}
    );
}

TEST(LexerTest, StringLiteral) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{", word \"a string literal\""};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        Lexer::Token{lexer::Punctuation<",">{(Span{0, 0, 0, 0, 1})}}
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"word", Span{0, 0, 0, 2, 6}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::StringLiteral{
            std::string{"a string literal"}, Span{0, 0, 0, 7, 25}}})
    );
}

TEST(LexerTest, EscapeSequences) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{"\"string\\bs\\t\\n\\\\\""};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::StringLiteral{
            std::string{"string\bs\t\n\\"}, Span{0, 0, 0, 0, 17}}})
    );
}

TEST(LexerTest, NumericLiterals) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{"10 34 word 7"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::IntegerLiteral{10, Span{0, 0, 0, 0, 2}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::IntegerLiteral{34, Span{0, 0, 0, 3, 5}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"word", Span{0, 0, 0, 6, 10}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::IntegerLiteral{7, Span{0, 0, 0, 11, 12}}})
    );
}

TEST(LexerTest, PunctuationFactoring) {
    using Lexer = lexer::WithPunctuations<".", "===", "==", "=">::Lexer<"fun">;
    std::stringstream source{"10 == === = == fun"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::IntegerLiteral{10, Span{0, 0, 0, 0, 2}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<"==">{Span{0, 0, 0, 3, 5}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<"===">{Span{0, 0, 0, 6, 9}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<"=">{Span{0, 0, 0, 10, 11}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<"==">{Span{0, 0, 0, 12, 14}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Keyword<"fun">{Span{0, 0, 0, 15, 18}}})
    );
}

TEST(LexerTest, Comment) {
    using Lexer = lexer::WithPunctuations<".", ",,,", ",,", ",">::Lexer<"fun">;
    std::stringstream source{"source code// comment here\nfun code\n"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"source", (Span{0, 0, 0, 0, 6})}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"code", (Span{0, 0, 0, 7, 11})}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Keyword<"fun">{(Span{1, 1, 27, 0, 3})}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"code", (Span{1, 1, 27, 4, 8})}})
    );
}

TEST(LexerTest, UnknownPunctuation) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{", hmm < ident word"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<",">{Span{0, 0, 0, 0, 1}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"hmm", Span{0, 0, 0, 2, 5}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"ident", Span{0, 0, 0, 8, 13}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"word", Span{0, 0, 0, 14, 18}}})
    );

    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);

    std::stringstream expectedErrors{};
    expectedErrors << Red{"unknown punctuation"} << ": <" << std::endl
                   << " |" << std::endl
                   << resetColor << "1|"
                   << ", hmm " << Red{"<"} << " ident word" << std::endl
                   << " |"
                   << "      " << Red{"^"} << std::endl
                   << " @ file.icy:1:7" << std::endl;

    EXPECT_EQ(errors.str(), expectedErrors.str());
}

TEST(LexerTest, UnrecognizedEscapeSequence) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{"\"blah\\h stuff\""};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(), (Lexer::Token{lexer::StringLiteral{
                          std::string{"blah stuff"}, Span{0, 0, 0, 0, 14}}})
    );

    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);

    std::stringstream expectedErrors{};
    expectedErrors << Red{"unknown escape sequence"} << ": \\h" << std::endl
                   << " |" << std::endl
                   << resetColor << "1|"
                   << "\"blah" << Red{"\\h"} << " stuff\"" << std::endl
                   << " |"
                   << "     " << Red{"^^"} << std::endl
                   << " @ file.icy:1:6" << std::endl;

    EXPECT_EQ(errors.str(), expectedErrors.str());
}

TEST(LexerTest, NumericLiteralTooLarge) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{"fun 191239823471232742893746237382374 word"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Keyword<"fun">{(Span{0, 0, 0, 0, 3})}})
    );
    EXPECT_EQ(
        lexer.next(), (Lexer::Token{lexer::IntegerLiteral{
                          1912398234712327428, (Span{0, 0, 0, 4, 37})}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"word", (Span{0, 0, 0, 38, 42})}})
    );

    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);

    std::stringstream expectedErrors{};
    expectedErrors << Red{"numeric literal too large"} << ": cannot exceed "
                   << std::numeric_limits<size_t>::max() << std::endl
                   << " |" << std::endl
                   << resetColor << "1|"
                   << "fun " << Red{"191239823471232742893746237382374"}
                   << " word" << std::endl
                   << " |"
                   << "    " << Red{"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"}
                   << std::endl
                   << " @ file.icy:1:5" << std::endl;

    EXPECT_EQ(errors.str(), expectedErrors.str());
}

TEST(LexerTest, NonTerminatedComment) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<>;
    std::stringstream source{"// non terminated comment"};
    Lexer lexer{Source{source, ""}};
    EXPECT_EQ(
        lexer.next(), (Lexer::Token{lexer::EndOfFile{Span{0, 0, 0, 24, 25}}})
    );
}

TEST(LexerTest, NonTerminatedString) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<>;
    std::stringstream source{R"("a string literal)"};
    Lexer lexer{Source{source, ""}};
    EXPECT_NE(
        lexer.next(),
        (Lexer::Token{lexer::StringLiteral{
            std::string{"a string literal"}, Span{0, 0, 0, 0, 17}}})
    );
    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);
    EXPECT_FALSE(errors.str().empty());
}

TEST(LexerTest, NonTerminatedEscape) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<>;
    std::stringstream source{R"("a string literal\)"};
    Lexer lexer{Source{source, ""}};
    EXPECT_NE(
        lexer.next(),
        (Lexer::Token{lexer::StringLiteral{
            std::string{R"(a string literal\)"}, Span{0, 0, 0, 0, 18}}})
    );
    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);
    EXPECT_FALSE(errors.str().empty());
}

TEST(LexerTest, FloatLiteral) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<>;
    std::stringstream source{"34.00452"};
    Lexer lexer{Source{source, ""}};
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::FloatLiteral{34.00452, Span{0, 0, 0, 0, 8}}})
    );
    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);
    EXPECT_TRUE(errors.str().empty());
}

TEST(LexerTest, FloatLiteralWithoutDecimal) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<>;
    std::stringstream source{"21."};
    Lexer lexer{Source{source, ""}};
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::FloatLiteral{21.0, Span{0, 0, 0, 0, 3}}})
    );
    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);
    EXPECT_TRUE(errors.str().empty());
}

TEST(LexerTest, SomeWeirdGeneric) {
    using Lexer = lexer::WithPunctuations<",", "." ,"(", ")", "[", "]", "<", ">", "->", "{", "}", ":", "/", "*", "+", "-", ";", "=">::Lexer<>;
    std::stringstream source{"<()>"};
    Lexer lexer{Source{source, ""}};
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<"<">{Span{0, 0, 0, 0, 1}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<"(">{Span{0, 0, 0, 1, 2}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<")">{Span{0, 0, 0, 2, 3}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<">">{Span{0, 0, 0, 3, 4}}})
    );
    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);
    EXPECT_TRUE(errors.str().empty());
}

TEST(LexerTest, TupleAccessRecognition) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<>;
    std::stringstream source{"tuple.0.1"};
    Lexer lexer{Source{source, ""}};
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Identifier{"tuple", Span{0, 0, 0, 0, 5}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<".">{Span{0, 0, 0, 5, 6}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::IntegerLiteral{0, Span{0, 0, 0, 6, 7}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::Punctuation<".">{Span{0, 0, 0, 7, 8}}})
    );
    EXPECT_EQ(
        lexer.next(),
        (Lexer::Token{lexer::IntegerLiteral{1, Span{0, 0, 0, 8, 9}}})
    );
    std::stringstream errors{};
    lexer.printDiagnosticsTo(errors);
    EXPECT_TRUE(errors.str().empty());
}
