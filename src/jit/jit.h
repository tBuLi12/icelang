#include <string_view>

enum class Result {
    Ok,
    Failed,
    DoesntCompile,
    DoesntParse,
};

Result jit(std::string_view sourceCode);
