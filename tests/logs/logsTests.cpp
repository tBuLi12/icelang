#include <sstream>

#include "logs.h"
#include <gtest/gtest.h>


struct Error : logs::SpannedMessage {
    void printHeaderTo(std::ostream& stream) override {
        stream << "an error: details" << std::endl;
    }

    Error(Span _span, Source _source) : SpannedMessage{_span, _source} {}
};

TEST(LogsTest, SpannedMessage) {
    std::stringstream source{"one line of source code\nsecond line of source "
                             "code\nthird line of source code"};

    std::stringstream error{};
    Error{Span{8, 10, 0, 4, 11}, Source{source, "some_file"}}.printTo(error);

    std::stringstream expectedError{};
    expectedError << "an error: details" << std::endl
                  << "  |" << std::endl
                  << resetColor << "9 |one " << redColor
                  << "line of source code" << std::endl
                  << resetColor << "10|" << redColor
                  << "second line of source code" << std::endl
                  << resetColor << "11|" << redColor << "third line "
                  << resetColor << "of source code" << std::endl
                  << "  |" << std::endl
                  << "  @ some_file:9:4" << std::endl;

    EXPECT_EQ(error.str(), expectedError.str());
}

TEST(LogsTest, SpannedMessageOneLine) {
    std::stringstream source{"just one line of source code"};

    std::stringstream error{};
    Error{Span{5, 5, 0, 5, 13}, Source{source, "some_file"}}.printTo(error);

    std::stringstream expectedError{};
    expectedError << "an error: details" << std::endl
                  << " |" << std::endl
                  << resetColor << "6|just " << redColor << "one line"
                  << resetColor << " of source code" << std::endl
                  << " |     " << redColor << "^^^^^^^^" << resetColor
                  << std::endl
                  << " @ some_file:6:5" << std::endl;

    EXPECT_EQ(error.str(), expectedError.str());
}
