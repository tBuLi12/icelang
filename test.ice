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
@extern fun dbgPrint(): int {}

fun createVector<T>(size: int): Vector<T> {
    Vector:<T> {
        buffer: rtAlloc(T.size * size),
        capacity: size,
        length: size,
    }
}

fun push<T>(vector: Vector<T>, item: T): Vector<T> {
    var vec = vector;
    if (vec.length == vec.capacity) {
        vec.buffer = rtRealloc(vec.buffer, T.size * vec.capacity);
        vec.capacity = 2 * vec.capacity;
    };
    vec[vec.length] = item;
    vec.length = vec.length + 1;
    vec
}

fun main(): int {
    // let x = ;
    match ([1, 1, 0, 3],1 ) {
        ([a, b, ..rest, c], 0) => rest[0],
        // [a, b, ..] => 0,
        vc => 0,
    }

    x.while (let [a, ..b]) b;

    // let y = x;
    // 0
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
        var new = Vector:<T> {
            buffer: rtCopy(this.length * T.size, this.buffer),
            length: this.length,
            capacity: this.length,
        };
        // var i = 0;
        // while (i != this.length) {
        //     new[i] = this[i];
        //     i = i + 1;
        // };
        new
    }
}