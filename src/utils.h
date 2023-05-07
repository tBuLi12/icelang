#ifndef ICY_UTILS_H
#define ICY_UTILS_H

#include <variant>

template <class... Fs> struct match : Fs... {
    using Fs::operator()...;
};

template <class... Fs> match(Fs&&...) -> match<Fs...>;

template <class T, class V> constexpr bool contains = false;

template <class T, class... Ts>
constexpr bool contains<T, std::variant<Ts...>> =
    (std::is_same_v<T, Ts> || ...);

template <class T, class V>
concept VariantElement = contains<T, V>;

template <class T> class UPtr : public std::unique_ptr<T> {
  public:
    UPtr(T&& value) : std::unique_ptr<T>(new T{std::move(value)}){};
};

template <class Out, class In> struct PushUnique;
template <class... Outs, class In, class... Ins>
struct PushUnique<std::variant<Outs...>, std::variant<In, Ins...>> {
    using type = typename PushUnique<
        std::conditional_t<
            (std::is_same_v<In, Outs> || ...), std::variant<Outs...>,
            std::variant<Outs..., In>>,
        std::variant<Ins...>>::type;
};

template <class Out> struct PushUnique<Out, std::variant<>> {
    using type = Out;
};

template <class... Ts>
using VariantWithoutDuplicates = typename PushUnique<std::variant<>, std::variant<Ts...>>::type;

#endif