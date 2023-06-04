// import "std:" as std

// trait Test {
//     fun num(): int
// }

// type TestT {
//     val : int
// }

// def TestT as Test {
//     fun num(): int -> this.val
// }

// trait MyThing {
//     fun doit(): int
// }

// def [std::String] as MyThing {
//     fun doit(): int -> 0
// }

// def<T> [T] as MyThing {
//     fun doit(): int -> 1
// }

// def<T is MyThing & std::Add<int>> [T] as MyThing {
//     fun doit(): int -> 1
// }

// @extern fun dostuff(a: int): int {}

        fun main(): int {
            match (0, 1, 0) {
                (a, b, 0) => a,
                (a, b, c) => 0,
            }
        }
