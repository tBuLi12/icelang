#include "logs.h"

#include <sstream>
#include <gtest/gtest.h>

TEST(LogsTest, SpannedMessageMultipleLines) {
    std::stringstream source{"one line of source code\nsecond line of source "
                             "code\nthird line of source code"};

    std::stringstream error{};
    error << logs::SpannedMessage{Source{source, "some_file"}, Span{8, 10, 0, 4, 11}, "an error", "details"};

    std::stringstream expectedError{};
    expectedError << setRedColor << "an error" << resetColor << ": details" << std::endl
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
    error << logs::SpannedMessage{Source{source, "some_file"}, Span{5, 5, 0, 5, 13}, "an error", "details"};

    std::stringstream expectedError{};
    expectedError << setRedColor << "an error" << resetColor << ": details" << std::endl
                  << " |" << std::endl
                  << resetColor << "6|just " << setRedColor << "one line"
                  << resetColor << " of source code" << std::endl
                  << " |     " << setRedColor << "^^^^^^^^" << resetColor
                  << std::endl
                  << " @ some_file:6:6" << std::endl;

    EXPECT_EQ(error.str(), expectedError.str());
}
