#include <iostream>
#include <stdlib.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" {
DLLEXPORT void* rtAlloc(int32_t size) {
    return malloc(static_cast<size_t>(size));
}
DLLEXPORT void* rtRealloc(void* old, int32_t size) {
    free(old);
    return malloc(static_cast<size_t>(size * 2));
}

DLLEXPORT void* rtCopy(int32_t size, void* buffer) {
    void* newBuf = malloc(static_cast<size_t>(size));
    std::memcpy(newBuf, buffer, size);
    return newBuf;
}

DLLEXPORT void rtFree(void* buffer) {
    free(buffer);
}

DLLEXPORT void rtSlice(unsigned char* buffer, int32_t offset, int32_t length) {
    std::memmove(buffer, buffer + offset, length);
}

DLLEXPORT void rtMove(
    int32_t offset, unsigned char* buffer, unsigned char* src, int32_t length
) {
    std::memcpy(buffer + offset, src, length);
}

DLLEXPORT void rtPrint(char* buffer, int32_t length) {
    for (size_t i = 0; i < length; ++i) {
        std::cout << *buffer;
        ++buffer;
    }
}

static constexpr auto setRedColor = "\u001b[31;1m";
static constexpr auto setBlueColor = "\u001b[36;1m";
static constexpr auto resetColor = "\u001b[0m";

DLLEXPORT void rtOobError(const char* message, int32_t index, int32_t length) {
    std::cout << setRedColor << "index out of bound" << resetColor << ": accessing index " << index
              << " on a vector of length " << length << std::endl
              << message;
    exit(1);
}

DLLEXPORT void rtZeroDivError(const char* message) {
    std::cerr << setRedColor << "arithmetic error" << resetColor << ": division by zero "
              << std::endl
              << message;
    exit(1);
}

DLLEXPORT int32_t dbgPrint() {
    std::cout << "eee" << std::endl;
    return 0;
}
}
