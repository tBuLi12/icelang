#include <iostream>
#include <stdlib.h>

extern "C" {
void* rtAlloc(int32_t size) {
    return malloc(static_cast<size_t>(size));
}
void* rtRealloc(void* old, int32_t size) {
    free(old);
    return malloc(static_cast<size_t>(size * 2));
}

void* rtCopy(int32_t size, void* buffer) {
    void* newBuf = malloc(static_cast<size_t>(size));
    std::memcpy(newBuf, buffer, size);
    return newBuf;
}

void rtFree(void* buffer) {
    free(buffer);
}

void rtSlice(unsigned char* buffer, int32_t offset, int32_t length) {
    std::cout << "slicerino " << offset << " " << length << std::endl;
    std::memmove(buffer, buffer + offset, length);
    std::cout << "then " << *reinterpret_cast<int32_t*>(buffer) << std::endl;
}

void rtOobError(char* message, int32_t index, int32_t length) {
    std::cout << "index out of bound: accessing index " << index
              << " on a vector of length " << length << std::endl
              << message << std::endl;
    exit(1);
}

int32_t dbgPrint() {
    std::cout << "eee" << std::endl;
    return 0;
}
}
