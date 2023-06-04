#include <gtest/gtest.h>
#include "jit.h"

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT int32_t dostuff() {
  std::cout << "dostuffWWWW "  << std::endl;
  return 0;
}


TEST(CompilerTest, SimplestOk) {
    EXPECT_EQ(jit(R"(
        fun main(): int -> 0
    )"), Result::Ok);
}

TEST(CompilerTest, SimplestFail) {
    EXPECT_EQ(jit(R"(
        fun main(): int -> 1
    )"), Result::Failed);
}

TEST(CompilerTest, VectorIndexing) {
    EXPECT_EQ(jit(R"(
        fun main(): int {
            let a = [1, 0, 1];
            a[1]
        }
    )"), Result::Ok);
}

TEST(CompilerTest, Vec) {
    EXPECT_EQ(jit(R"(
        fun main(): int {
            let a = [1, 0, 1];
            a[0]
        }
    )"), Result::Failed);
}

TEST(CompilerTest, Match) {
    EXPECT_EQ(jit(R"(
        fun main(): int {
            match (0, 1, 0) {
                (a, b, 0) => a,
                (a, b, c) => 0,
            }
        }
    )"), Result::Ok);
}



