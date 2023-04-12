#include <sstream>

#include "lexer.h"
#include <gtest/gtest.h>

TEST(LexerTest, KeywordTest) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun", "if">;
    std::stringstream source{", fun word if other"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Punctuation<",">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Keyword<"fun">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"word"}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Keyword<"if">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"other"}});
}

TEST(LexerTest, StringLiteral) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{", word \"a string literal\""};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Punctuation<",">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"word"}});
    EXPECT_EQ(
        lexer.next(),
        Lexer::Token{lexer::literal::String{std::string{"a string literal"}}}
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
        Lexer::Token{lexer::literal::String{std::string{"string\bs\t\n\\"}}}
    );
}

TEST(LexerTest, NumericLiterals) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{"10 34 word 7"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::literal::Numeric{10}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::literal::Numeric{34}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"word"}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::literal::Numeric{7}});
}

TEST(LexerTest, PunctuationFactoring) {
    using Lexer = lexer::WithPunctuations<".", "===", "==", "=">::Lexer<"fun">;
    std::stringstream source{"10 == === = == fun"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::literal::Numeric{10}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Punctuation<"==">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Punctuation<"===">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Punctuation<"=">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Punctuation<"==">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Keyword<"fun">{}});
}

TEST(LexerTest, Comment) {
    using Lexer = lexer::WithPunctuations<".", ",,,", ",,", ",">::Lexer<"fun">;
    std::stringstream source{"source code// comment here\nfun code\n"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"source"}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"code"}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Keyword<"fun">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"code"}});
}

TEST(LexerTest, UnknownPunctuation) {
    using Lexer = lexer::WithPunctuations<".", ",">::Lexer<"fun">;
    std::stringstream source{", hmm < ident word"};
    Lexer lexer{Source{
        source,
        "file.icy",
    }};

    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Punctuation<",">{}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"hmm"}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"ident"}});
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"word"}});

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
        lexer.next(),
        Lexer::Token{lexer::literal::String{std::string{"blah stuff"}}}
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

    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Keyword<"fun">{}});
    EXPECT_EQ(
        lexer.next(), Lexer::Token{lexer::literal::Numeric{1912398234712327428}}
    );
    EXPECT_EQ(lexer.next(), Lexer::Token{lexer::Identifier{"word"}});

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
