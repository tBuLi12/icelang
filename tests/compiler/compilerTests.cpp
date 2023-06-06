#include <gtest/gtest.h>
#include "jit.h"

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

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

TEST(CompilerTest, VectorIndexingFail) {
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

TEST(CompilerTest, MatchGuard) {
    EXPECT_EQ(jit(R"(
        fun main(): int {
            match (0, 1, 0) {
                (a, b, 0) if b == 0 => 1,
                (a, b, c) => 0,
            }
        }
    )"), Result::Ok);
}

extern "C" DLLEXPORT int32_t isSeven(int32_t number) {
  return number == 7 ? 0 : 1;
}

TEST(CompilerTest, StringLength) {
    EXPECT_EQ(jit(R"(
        @extern fun isSeven(n: int): int {}
        fun main(): int {
            isSeven("aaaaaaa".proto.length())
        }
    )"), Result::Ok);
}

extern "C" DLLEXPORT int32_t isSampleString(char* ptr, int32_t number) {
  return std::string_view{ptr, static_cast<size_t>(number)} == "sample string" ? 0 : 1;
}

TEST(CompilerTest, StringValue) {
    EXPECT_EQ(jit(R"(
        @extern fun isSampleString(ptr: @ptr int, n: int): int {}
        fun main(): int {
            let sample = "sample string";
            isSampleString(sample.proto.rawBuffer(), sample.proto.length())
        }
    )"), Result::Ok);
}

TEST(CompilerTest, Function) {
    EXPECT_EQ(jit(R"(
        fun func(): int -> 0
        fun main(): int -> func()
    )"), Result::Ok);
}

TEST(CompilerTest, DuplicateDeclaration) {
    EXPECT_EQ(jit(R"(
        fun func(): int -> 0
        fun func(): int -> 1
        fun main(): int -> func()
    )"), Result::DoesntCompile);
}

TEST(CompilerTest, MismatchedReturnType) {
    EXPECT_EQ(jit(R"(
        fun main(): int -> "some string"
    )"), Result::DoesntCompile);
}