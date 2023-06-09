#ifndef ICE_STD_H
#define ICE_STD_H

static const char* stdIce = R"(
public trait Add<T> {
    fun add(other: T): T
}

def int as Add<int> {
    fun add(other: int): int -> this + other
}

public trait Multiply<T> {
    fun multiply(other: T): T
}

def int as Multiply<int> {
    fun multiply(other: int): int -> this * other
}

public trait Equate<T> {
    fun eq(other: T): bool
}

def int as Equate<int> {
    fun eq(other: int): bool -> this == other
}

public trait Into<T> {
    fun into(): T
}

public type Vector<T> {
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
@extern fun rtPrint(buf: @ptr int, len: int) {}
@extern fun dbgPrint(): int {}

public type String public [char]

def String {
    public fun print() {
        rtPrint(this.buffer, this.length);
    }
}

def String as Add<String> {
    fun add(other: String): String -> String([..this.proto, ..other.proto])
}

def String as Equate<String> {
    fun eq(other: String): bool {
        if (this.length != other.length) {
            return (0 == 1);
        };
        var i = 0;
        while (i != this.length) {
            if (this.proto[i] != other.proto[i]) {
                return (0 == 1);
            };
            i = i + 1;
        };

        (0 == 0)
    }
}

def<T> [T] {
    public fun length(): int -> this.length
    public fun rawBuffer(): @ptr int -> this.buffer
    public mut pop(): T {
        let popped = ~this[this.length - 1];
        this.length = this.length - 1;
        popped
    }

    public mut push(item: T) {
        if (this.length == this.capacity) {
            this.buffer = rtRealloc(this.buffer, T.size * this.capacity);
            this.capacity = 2 * this.capacity;
        };
        this.length = this.length + 1;
        ~this[this.length] = item;
    }
}

trait Copy {
    fun copy(): This
}

trait Drop {
    mut drop()
}

def<T> [T] as Drop {
    mut drop() {
        while (this.length != 0) {
            this.pop();
        };
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
            ~new[i] = this[i];
            i = i + 1;
        };
        new
    }
}
    )";

#endif
