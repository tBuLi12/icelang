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

TEST(CompilerTest, Add) {
    EXPECT_EQ(jit(R"(
        @extern fun isSeven(n: int): int {}
        fun main(): int -> isSeven(3 + 4)
    )"), Result::Ok);
}

extern "C" DLLEXPORT int32_t isTwelve(int32_t number) {
  return number == 12 ? 0 : 1;
}

TEST(CompilerTest, Substract) {
    EXPECT_EQ(jit(R"(
        @extern fun isTwelve(n: int): int {}
        fun main(): int -> isTwelve(17 - 5)
    )"), Result::Ok);
}

TEST(CompilerTest, Multiply) {
    EXPECT_EQ(jit(R"(
        @extern fun isTwelve(n: int): int {}
        fun main(): int -> isTwelve(3 * 4)
    )"), Result::Ok);
}

extern "C" DLLEXPORT int32_t isThirtyThree(int32_t number) {
  return number == 33 ? 0 : 1;
}

TEST(CompilerTest, Divide) {
    EXPECT_EQ(jit(R"(
        @extern fun isThirtyThree(n: int): int {}
        fun main(): int -> isThirtyThree(100 / 3)
    )"), Result::Ok);
}

extern "C" DLLEXPORT int32_t isTrue(bool condition) {
  return condition ? 0 : 1;
}

TEST(CompilerTest, EqualityYes) {
    EXPECT_EQ(jit(R"(
        @extern fun isTrue(n: bool): int {}
        fun main(): int -> isTrue(2 == 2)
    )"), Result::Ok);
}

TEST(CompilerTest, EqualityNo) {
    EXPECT_EQ(jit(R"(
        @extern fun isTrue(n: bool): int {}
        fun main(): int -> isTrue(2 == 8)
    )"), Result::Failed);
}

TEST(CompilerTest, InequalityYes) {
    EXPECT_EQ(jit(R"(
        @extern fun isTrue(n: bool): int {}
        fun main(): int -> isTrue(5 != 3)
    )"), Result::Ok);
}

TEST(CompilerTest, InequalityNo) {
    EXPECT_EQ(jit(R"(
        @extern fun isTrue(n: bool): int {}
        fun main(): int -> isTrue(3 != 3)
    )"), Result::Failed);
}

extern "C" DLLEXPORT int32_t isNegativeThree(int32_t value) {
  return value == -3 ? 0 : 1;
}

TEST(CompilerTest, UnaryMinus) {
    EXPECT_EQ(jit(R"(
        @extern fun isNegativeThree(n: int): int {}
        fun main(): int -> isNegativeThree(-3)
    )"), Result::Ok);
}

TEST(CompilerTest, LogicalNegation) {
    EXPECT_EQ(jit(R"(
        @extern fun isTrue(n: bool): int {}
        fun main(): int -> isTrue(!(3 == 5))
    )"), Result::Ok);
}

TEST(CompilerTest, IfYes) {
    EXPECT_EQ(jit(R"(
        fun main(): int {
            let a = 3;
            if (a == 3) {
                return 1;
            };
            0
        }
    )"), Result::Failed);
}

TEST(CompilerTest, IfNo) {
    EXPECT_EQ(jit(R"(
        fun main(): int {
            let a = 3;
            if (a == 7) {
                return 1;
            };
            0
        }
    )"), Result::Ok);
}

extern "C" DLLEXPORT int32_t isNine(int32_t value) {
  return value == 9 ? 0 : 1;
}

TEST(CompilerTest, While) {
    EXPECT_EQ(jit(R"(
        @extern fun isNine(value: int): int {}
        fun main(): int {
            var c = 0;
            while (c < 8) {
                c = c + 3;
            };
            isNine(c)
        }
    )"), Result::Ok);
}

TEST(CompilerTest, WhileLet) {
    EXPECT_EQ(jit(R"(
        @extern fun isNine(value: int): int {}
        fun main(): int {
            var c = 0;
            while (let b < 8 = c) {
                c = c + 3;
            };
            isNine(c)
        }
    )"), Result::Ok);
}

TEST(CompilerTest, Type) {
    EXPECT_EQ(jit(R"(
        @extern fun isNine(value: int): int {}

        type Test { field: int }
        
        fun main(): int {
            let t = Test { field: 9 };
            isNine(t.field)
        }
    )"), Result::Ok);
}

TEST(CompilerTest, Method) {
    EXPECT_EQ(jit(R"(
        @extern fun isNine(value: int): int {}

        type Test { field: int }

        def Test {
            fun getField(): int -> this.field
        }
        
        fun main(): int {
            let t = Test { field: 9 };
            isNine(t.getField())
        }
    )"), Result::Ok);
}

