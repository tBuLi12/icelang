#ifndef ICY_STRING_H
#define ICY_STRING_H

#include <algorithm>
#include <ostream>
#include <ranges>

template <size_t N> struct String {
    char characters[N];

    constexpr String(std::ranges::input_range auto const& source)
        : characters{} {
        std::ranges::copy(source, characters);
    }

    constexpr size_t length() const noexcept {
        return N - 1;
    }

    constexpr bool operator==(std::string const& other) const {
        return other == characters;
    }

    template <size_t M>
    constexpr bool operator==(String<M> const& other) const {
        return std::ranges::equal(characters, other.characters);
    }

    template <size_t M>
    consteval bool startsWith(String<M> const& other) const {
        return M <= N && std::ranges::equal(
                             characters | std::views::take(M - 1),
                             other.characters | std::views::take(M - 1)
                         );
    }

    template <size_t M>
    constexpr String<N + M - 1> operator+(String<M> const& second) {
        String<N + M - 1> summed{characters};
        std::ranges::copy(
            second.characters, std::begin(summed.characters) + N - 1
        );
        return summed;
    }

    template <size_t M>
    constexpr String<N + M - 1> operator+(const char (&second)[M]) {
        return *this + String{second};
    }

    constexpr char operator[](size_t index) const {
        return characters[index];
    }
};

template <size_t M, size_t N>
constexpr String<M + 1> truncate(String<N> const& string) {
    String<M + 1> truncated{string.characters | std::views::take(M)};
    truncated.characters[M] = '\0';
    return truncated;
}

template <size_t N, size_t M>
constexpr String<N + M - 1>
operator+(const char (&first)[N], String<M> const& second) {
    return String{first} + second;
}

template <size_t N>
std::ostream& operator<<(std::ostream& stream, String<N> const& string) {
    return stream << string.characters;
}

template <size_t N> String(const char (&)[N]) -> String<N>;

#endif
