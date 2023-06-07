#ifndef ICY_TYPES_H
#define ICY_TYPES_H

#include <compare>
#include "../utils.h"
#include <fmt/format.h>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

namespace llvm {
struct Value;
struct Type;
struct AllocaInst;
} // namespace llvm

struct Type;

namespace ast {
struct TypeDeclaration;
struct TraitDeclaration;
struct FunctionDeclaration;
struct TraitImplementation;
struct Implementation;
} // namespace ast

struct Trait {
    ast::TraitDeclaration* declaration;
    std::vector<Type> typeArguments;

    bool operator==(Trait const& other) const = default;
    // bool operator!=(Trait const& other) const;
};

struct Function {
    ast::FunctionDeclaration* declaration;
    std::vector<Type> typeArguments;
};

struct TraitImplRef {
    ast::TraitImplementation* declaration;
    std::vector<Type> typeArguments;
};

struct ImplRef {
    ast::Implementation* declaration;
    std::vector<Type> typeArguments;
    std::vector<Type> funTypeArguments;
};

namespace type {
struct BuiltIn {
    llvm::Type* asLLVM;
    std::string_view name;


    bool operator==(BuiltIn const& other) const {
        return name == other.name;
    };
};

static BuiltIn integer = BuiltIn{nullptr, "int"};
static BuiltIn null = BuiltIn{nullptr, "null"};
static BuiltIn boolean = BuiltIn{nullptr, "bool"};
static BuiltIn ptr = BuiltIn{nullptr, "ptr"};
static BuiltIn character = BuiltIn{nullptr, "char"};
static BuiltIn floating = BuiltIn{nullptr, "float"};

struct Struct {
    std::vector<std::pair<std::string, Type>> properties;
    std::strong_ordering operator<=>(Struct const& other) const;
    bool operator==(Struct const& other) const;
};
struct Tuple {
    std::vector<Type> fields;
    std::strong_ordering operator<=>(Tuple const& other) const;
    bool operator==(Tuple const& other) const = default;
};
struct Union {
    std::vector<Type> elements;
    std::strong_ordering operator<=>(Union const& other) const;
    bool operator==(Union const& other) const = default;
};
struct Variant {
    std::vector<std::pair<std::string, Type>> variants;
    std::strong_ordering operator<=>(Variant const& other) const;
    bool operator==(Variant const& other) const;
};
// struct Vector {
//     explicit Vector(Vector const&);
//     Vector(Vector&& other) = default;
//     Vector& operator=(Vector&& other) = default;
//     Vector(Type&& _element) : element(std::move(_element)){};

//     UPtr<Type> element;
//     bool operator==(Vector const& other) const = default;
// };

struct Parameter {
    bool isBlockParameter;
    size_t index;
    std::strong_ordering operator<=>(Parameter const& other) const = default;
    bool operator==(Parameter const& other) const = default;
};

struct Named {
    ast::TypeDeclaration* declaration;
    std::vector<Type> typeArguments;
    std::vector<std::string> promotions;
    std::strong_ordering operator<=>(Named const& other) const;
    bool operator==(Named const& other) const = default;
};

struct Never {
    std::strong_ordering operator<=>(Never const& other) const = default;
    bool operator==(Never const&) const = default;
};
} // namespace type

// namespace std {
// template <>
// inline constexpr bool is_trivially_copy_constructible_v<type::Vector> =
// false;
// }

using TypeBase = std::variant<
    type::Named, type::BuiltIn*, type::Tuple, type::Struct, type::Parameter,
    type::Never, type::Union, type::Variant>;

struct Type : TypeBase {
    template <VariantElement<TypeBase> T>
    constexpr Type(T&& type)
        requires(!std::is_trivially_copy_constructible_v<T>)
        : TypeBase{std::move(type)} {};

    template <VariantElement<TypeBase> T>
    constexpr Type(T type)
        requires(std::is_trivially_copy_constructible_v<T>)
        : TypeBase{type} {};

    constexpr Type(
        ast::TypeDeclaration* namedTypeDeclaration,
        std::vector<Type>&& typeArguments
    )
        : TypeBase{
              type::Named{
                  namedTypeDeclaration,
                  std::move(typeArguments),
              },
          } {}

    constexpr Type() = default;

    bool operator==(Type const& other) const = default;
    std::strong_ordering operator<=>(Type const& other) const;
    bool isAssignableTo(Type const& other) const;

    Type operator[](size_t index) const;
};

std::ostream& operator<<(std::ostream& stream, Type const& type);

Type with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments, Type const& type
);

Trait with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments, Trait const& trait
);

std::vector<Type> with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments, std::vector<Type> const& typeList
);

std::vector<std::pair<Type, Trait>> with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments,
    std::vector<std::pair<Type, Trait>> const& traitBounds
);

struct TraitMethodRef {
    Trait trait;
    std::vector<Type> typeArguments;
};

template <> struct fmt::formatter<Type> : formatter<std::string> {
    auto format(Type const& type, auto& ctx) const {
        std::stringstream str{};
        str << type;
        return fmt::format_to(ctx.out(), "{}", str.str());
    }
};

#endif
