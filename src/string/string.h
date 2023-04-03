#ifndef ICY_STRING_H
#define ICY_STRING_H

#include <algorithm>
#include <ostream>
#include <ranges>

template <size_t N> struct String {
    size_t length = N - 1;
    char characters[N];

    constexpr String(std::ranges::input_range auto const& source)
        : characters{} {
        std::ranges::copy(source, characters);
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
    constexpr String<M + 1> slice() const
        requires(M < N - 1)
    {
        String<M + 1> sliced{characters | std::views::take(M)};
        sliced.characters[M] = '\0';
        return sliced;
    }

    template <size_t M> constexpr String<M + 1> slice() const {
        return String<M + 1>{characters};
    }

    // template <size_t M> constexpr String<M + 1> slice() const {
    //     String<M + 1> sliced{};
    //     std::copy_n(characters, N - 1, sliced.characters);
    //     return sliced;
    // }

    template <size_t M>
    constexpr String<N + M - 1> operator+(String<M> const& second) {
        String<N + M - 1> summed{characters};
        std::ranges::copy(
            second.characters, std::begin(summed.characters) + (N - 1)
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
