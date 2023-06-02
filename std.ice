import "other" as other
import "third" as third
import "test:name" as tstd

trait Add<T> {
    fun add(other: T): T
}

def int as Add<int> {
    fun add(other: int): int -> this + other
}

trait Multiply<T> {
    fun multiply(other: T): T
}

def int as Multiply<int> {
    fun multiply(other: int): int -> this * other
}

trait Equate<T> {
    fun eq(other: T): bool
}

def int as Equate<int> {
    fun eq(other: int): bool -> this == other
}

type Vector<T> {
    buffer: @ptr int,
    capacity: int,
    length: int, 
}

@extern fun rtAlloc(size: int): @ptr int {}
@extern fun rtRealloc(buf: @ptr int, size: int): @ptr int {}
@extern fun rtCopy(size: int, buf: @ptr int): @ptr int {}
@extern fun rtFree(buf: @ptr int) {}
@extern fun rtSlice(buf: @ptr int, offset: int, len: int) {}
@extern fun rtMove(offset: int, buf: @ptr int, src: @ptr int, len: int) {}
@extern fun dbgPrint(): int {}

fun createVector<T>(size: int) {
    Vector:<T> {
        buffer: rtAlloc(T.size * size),
        capacity: size,
        length: size,
    };
}

fun createVector<T>(size: int): Vector<T> {
    Vector:<T> {
        buffer: rtAlloc(T.size * size),
        capacity: size,
        length: size,
    }
}
fun push<T>(vector: Vector<T>, item: T): Vector<T> {
    T.size;
    var vec = vector;
    if (vec.length == vec.capacity) {
        vec.buffer = rtRealloc(vec.buffer, T.size * vec.capacity);
        vec.capacity = 2 * vec.capacity;
    };
    vec[vec.length] = item;
    vec.length = vec.length + 1;
    vec
}

trait Test {
    fun num(): int
}

type TestT {
    val : int
}

def TestT {
    fun num(): int -> this.val
}

def<T> [T] {
    fun last(): T -> this[0]
}

fun main(): int {
    let a = [1, 2, 3];
    
    other::fl::pull()
}

trait Copy {
    fun copy(): This
}

trait Drop {
    fun drop()
}

def<T> [T] as Drop {
    fun drop() {
        rtFree(this.buffer);
    }
}

def<T> [T] as Copy {
    fun copy(): This {
        var new = This {
            buffer: rtAlloc(this.length * T.size),
            length: this.length,
            capacity: this.length,
        };
        var i = 0;
        while (i != this.length) {
            new[i] = this[i];
            i = i + 1;
        };
        new
    }
}
