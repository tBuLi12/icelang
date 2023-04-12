#include <sstream>

#include "logs.h"
#include <gtest/gtest.h>

struct Error : logs::SpannedMessage {
    void printHeaderTo(std::ostream& stream) const override {
        stream << "an error: details" << std::endl;
    }

    Error(Span _span, Source _source) : SpannedMessage{_span, _source} {}
};

TEST(LogsTest, SpannedMessageMultipleLines) {
    std::stringstream source{"one line of source code\nsecond line of source "
                             "code\nthird line of source code"};

    std::stringstream error{};
    Error{Span{8, 10, 0, 4, 11}, Source{source, "some_file"}}.printTo(error);

    std::stringstream expectedError{};
    expectedError << "an error: details" << std::endl
                  << "  |" << std::endl
                  << resetColor << "9 |one " << setRedColor
                  << "line of source code" << std::endl
                  << resetColor << "10|" << setRedColor
                  << "second line of source code" << std::endl
                  << resetColor << "11|" << setRedColor << "third line "
                  << resetColor << "of source code" << std::endl
                  << "  |" << std::endl
                  << "  @ some_file:9:5" << std::endl;

    EXPECT_EQ(error.str(), expectedError.str());
}

TEST(LogsTest, SpannedMessageOneLine) {
    std::stringstream source{"just one line of source code"};

    std::stringstream error{};
    Error{Span{5, 5, 0, 5, 13}, Source{source, "some_file"}}.printTo(error);

    std::stringstream expectedError{};
    expectedError << "an error: details" << std::endl
                  << " |" << std::endl
                  << resetColor << "6|just " << setRedColor << "one line"
                  << resetColor << " of source code" << std::endl
                  << " |     " << setRedColor << "^^^^^^^^" << resetColor
                  << std::endl
                  << " @ some_file:6:6" << std::endl;

    EXPECT_EQ(error.str(), expectedError.str());
}
