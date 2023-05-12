#include "./types.h"
#include "./ast.h"

#include <algorithm>
#include <iostream>

namespace type {
// can't use default, clang literally crashes
bool Struct::operator==(Struct const& other) const {
    return std::ranges::equal(properties, other.properties);
};
} // namespace type

// bool Type::operator==(Type const& other) const {
//     std::visit(
//         match{
//             []<class T>(T const& type, T const& other) {
//                 return type == other;
//             },
//             [](auto const&, auto const&) { return false; },
//         },
//         static_cast<TypeBase const&>(*this), static_cast<TypeBase
//         const&>(other)
//     );
// };

std::ostream& operator<<(std::ostream& stream, Type const& type) {
    std::visit(
        match{
            [&](type::Named const& first) {
                stream << first.declaration->name.value;

                if (first.typeArguments.size() > 0) {
                    stream << fmt::format(
                        "<{}>", fmt::join(first.typeArguments, ",")
                    );
                } else {
                    if (first.declaration->name.value == "Vector") {
                        fmt::println("wtf");
                        throw "";
                    }
                }
            },
            [&](type::BuiltIn* const& first) { stream << first->name; },
            [&](type::Struct const& first) {
                stream << "{";
                for (auto& [field, type] : first.properties)
                    stream << field << ": " << type << ",";

                stream << "}";
            },
            [&](type::Tuple const& first) {
                stream << "(";
                for (auto& type : first.fields)
                    stream << type << ",";

                stream << ")";
            },
            [&](type::Parameter const& first) {
                stream << "param " << first.isBlockParameter << " "
                       << first.index;
            },
            [&](type::Never const& first) { stream << "never"; },
        },
        type
    );
    return stream;
}

bool Type::isAssignableTo(Type const& other) const {
    if (std::holds_alternative<type::Never>(*this)) {
        return true;
    }
    return *this == other;
}

Type Type::operator[](size_t index) const {
    return std::visit(
        match{
            [&](type::Named const& named) {
                if (index != 0) {
                    std::cout << "invalid type operator" << std::endl;
                    throw "";
                }
                return with(named.typeArguments, {}, named.declaration->proto);
            },
            [&](type::Struct const& named) {
                return named.properties[index].second;
            },
            [&](type::Tuple const& named) { return named.fields[index]; },
            [](auto const&) -> Type {
                std::cout << "invalid type operator" << std::endl;
                throw "";
            },
        },
        *this
    );
}

Type with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments, Type const& type
) {
    return std::visit(
        match{
            [&](auto const&) -> Type { return Type{type}; },
            [&](type::Named const& namedType) -> Type {
                type::Named substituted{};
                substituted.declaration = namedType.declaration;
                for (auto const& typeArgument : namedType.typeArguments) {
                    substituted.typeArguments.push_back(
                        with(blockTypeArguments, typeArguments, typeArgument)
                    );
                }
                return Type{std::move(substituted)};
            },
            [&](type::Parameter const& typeParameter) -> Type {
                if (typeParameter.isBlockParameter) {
                    return Type{blockTypeArguments[typeParameter.index]};
                }
                return Type{typeArguments[typeParameter.index]};
            },
            [&](type::Tuple const& tuple) -> Type {
                std::vector<Type> fields{};
                for (auto& field : tuple.fields) {
                    fields.push_back(
                        with(blockTypeArguments, typeArguments, field)
                    );
                }
                return Type{type::Tuple{std::move(fields)}};
            },
            [&](type::Struct const& structure) -> Type {
                std::vector<std::pair<std::string, Type>> properties{};
                for (auto& [name, property] : structure.properties) {
                    properties.push_back(
                        {name,
                         Type{
                             with(blockTypeArguments, typeArguments, property)}}
                    );
                }
                return Type{type::Struct{std::move(properties)}};
            },
            [&](type::Never const&) -> Type { return type::Never{}; },
        },
        type
    );
}

Trait with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments, Trait const& trait
) {
    Trait substituted{};
    substituted.declaration = trait.declaration;
    for (auto const& typeArgument : trait.typeArguments) {
        substituted.typeArguments.push_back(
            with(blockTypeArguments, typeArguments, typeArgument)
        );
    }
    return std::move(substituted);
}

std::vector<Type> with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments, std::vector<Type> const& typeList
) {
    std::vector<Type> substituted{};
    for (auto& type : typeList) {
        substituted.push_back(with(blockTypeArguments, typeArguments, type));
    }
    return std::move(substituted);
}

std::vector<std::pair<Type, Trait>> with(
    std::vector<Type> const& blockTypeArguments,
    std::vector<Type> const& typeArguments,
    std::vector<std::pair<Type, Trait>> const& traitBounds
) {
    std::vector<std::pair<Type, Trait>> substituted{};
    for (auto& [type, trait] : traitBounds) {
        std::cout << "trying type " << type << std::endl;
        std::cout << "given" << blockTypeArguments.size() << std::endl;
        auto tp = with(blockTypeArguments, typeArguments, type);
        std::cout << "trying trait" << std::endl;
        auto tr = with(blockTypeArguments, typeArguments, trait);
        substituted.push_back({std::move(tp), std::move(tr)});
    }
    return std::move(substituted);
}
