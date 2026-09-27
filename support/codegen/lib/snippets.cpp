/*****************************************************************************************
 *                                                                                       *
 * OpenSpace Codegen                                                                     *
 *                                                                                       *
 * Copyright (c) 2021-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include "snippets.h"

#include "types.h"
#include <cassert>
#include <format>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>

// This file is full of line length violations, but f*ck it;  the generated code would
// look a lot worse if we would break these everywhere or it would be unreadable

namespace {
    // All of these snippets can probably replaced by a std::format expression that takes
    // two parameters;  one for the "internal" type and one for the baked-to type

    constexpr std::string_view BakeFunctionBool = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, bool* val) { *val = d.value<bool>(key); }\n";
    constexpr std::string_view BakeFunctionInt = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, int* val) { if (d.hasValue<double>(key)) { *val = static_cast<int>(d.value<double>(key)); } else if (d.hasValue<int>(key)) { *val = d.value<int>(key); } else { throw std::logic_error(\"Unexpected type\"); } }\n";
    constexpr std::string_view BakeFunctionDouble = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, double* val) { *val = d.value<double>(key); }\n";
    constexpr std::string_view BakeFunctionFloat = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, float* val) { *val = static_cast<float>(d.value<double>(key)); }\n";
    constexpr std::string_view BakeFunctionString = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, std::string* val) { *val = d.value<std::string>(key); }\n";
    constexpr std::string_view BakeFunctionPath = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, std::filesystem::path* val) { *val = d.value<std::string>(key); }\n";
    constexpr std::string_view BakeFunctionIVec2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::ivec2* val) { *val = d.value<glm::dvec2>(key); }\n";
    constexpr std::string_view BakeFunctionIVec3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::ivec3* val) { *val = d.value<glm::dvec3>(key); }\n";
    constexpr std::string_view BakeFunctionIVec4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::ivec4* val) { *val = d.value<glm::dvec4>(key); }\n";
    constexpr std::string_view BakeFunctionDVec2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dvec2* val) { *val = d.value<glm::dvec2>(key); }\n";
    constexpr std::string_view BakeFunctionDVec3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dvec3* val) { *val = d.value<glm::dvec3>(key); }\n";
    constexpr std::string_view BakeFunctionDVec4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dvec4* val) { *val = d.value<glm::dvec4>(key); }\n";
    constexpr std::string_view BakeFunctionVec2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::vec2* val) { *val = d.value<glm::dvec2>(key); }\n";
    constexpr std::string_view BakeFunctionVec3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::vec3* val) { *val = d.value<glm::dvec3>(key); }\n";
    constexpr std::string_view BakeFunctionVec4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::vec4* val) { *val = d.value<glm::dvec4>(key); }\n";
    constexpr std::string_view BakeFunctionMat2x2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat2x2* val) { *val = d.value<glm::dmat2x2>(key); }\n";
    constexpr std::string_view BakeFunctionMat2x3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat2x3* val) { *val = d.value<glm::dmat2x3>(key); }\n";
    constexpr std::string_view BakeFunctionMat2x4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat2x4* val) { *val = d.value<glm::dmat2x4>(key); }\n";
    constexpr std::string_view BakeFunctionMat3x2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat3x2* val) { *val = d.value<glm::dmat3x2>(key); }\n";
    constexpr std::string_view BakeFunctionMat3x3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat3x3* val) { *val = d.value<glm::dmat3x3>(key); }\n";
    constexpr std::string_view BakeFunctionMat3x4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat3x4* val) { *val = d.value<glm::dmat3x4>(key); }\n";
    constexpr std::string_view BakeFunctionMat4x2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat4x2* val) { *val = d.value<glm::dmat4x2>(key); }\n";
    constexpr std::string_view BakeFunctionMat4x3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat4x3* val) { *val = d.value<glm::dmat4x3>(key); }\n";
    constexpr std::string_view BakeFunctionMat4x4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::mat4x4* val) { *val = d.value<glm::dmat4x4>(key); }\n";
    constexpr std::string_view BakeFunctionDMat2x2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat2x2* val) { *val = d.value<glm::dmat2x2>(key); }\n";
    constexpr std::string_view BakeFunctionDMat2x3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat2x3* val) { *val = d.value<glm::dmat2x3>(key); }\n";
    constexpr std::string_view BakeFunctionDMat2x4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat2x4* val) { *val = d.value<glm::dmat2x4>(key); }\n";
    constexpr std::string_view BakeFunctionDMat3x2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat3x2* val) { *val = d.value<glm::dmat3x2>(key); }\n";
    constexpr std::string_view BakeFunctionDMat3x3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat3x3* val) { *val = d.value<glm::dmat3x3>(key); }\n";
    constexpr std::string_view BakeFunctionDMat3x4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat3x4* val) { *val = d.value<glm::dmat3x4>(key); }\n";
    constexpr std::string_view BakeFunctionDMat4x2 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat4x2* val) { *val = d.value<glm::dmat4x2>(key); }\n";
    constexpr std::string_view BakeFunctionDMat4x3 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat4x3* val) { *val = d.value<glm::dmat4x3>(key); }\n";
    constexpr std::string_view BakeFunctionDMat4x4 = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, glm::dmat4x4* val) { *val = d.value<glm::dmat4x4>(key); }\n";
    constexpr std::string_view BakeFunctionDictionary = "[[maybe_unused]] void bakeTo(const ghoul::Dictionary& d, std::string_view key, ghoul::Dictionary* val) { *val = d.value<ghoul::Dictionary>(key); }\n";

    constexpr std::string_view VariantConverterBool = "   if (d.hasValue<bool>(key)) { bool v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterInt = "   if (d.hasValue<double>(key)) { int v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDouble = "   if (d.hasValue<double>(key)) { double v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterFloat = "   if (d.hasValue<double>(key)) { float v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterString = "   if (d.hasValue<std::string>(key)) { std::string v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterPath = "   if (d.hasValue<std::string>(key)) { std::filesystem::path v; bakeTo(d, key, &v); *val = v; return; }\n";
    constexpr std::string_view VariantConverterIVec2 = "   if (d.hasValue<glm::dvec2>(key)) { glm::ivec2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterIVec3 = "   if (d.hasValue<glm::dvec3>(key)) { glm::ivec3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterIVec4 = "   if (d.hasValue<glm::dvec4>(key)) { glm::ivec4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDVec2 = "   if (d.hasValue<glm::dvec2>(key)) { glm::dvec2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDVec3 = "   if (d.hasValue<glm::dvec3>(key)) { glm::dvec3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDVec4 = "   if (d.hasValue<glm::dvec4>(key)) { glm::dvec4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterVec2 = "   if (d.hasValue<glm::dvec2>(key)) { glm::vec2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterVec3 = "   if (d.hasValue<glm::dvec3>(key)) { glm::vec3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterVec4 = "   if (d.hasValue<glm::dvec4>(key)) { glm::vec4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat2x2 = "   if (d.hasValue<glm::dmat2x2>(key)) { glm::mat2x2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat2x3 = "   if (d.hasValue<glm::dmat2x3>(key)) { glm::mat2x3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat2x4 = "   if (d.hasValue<glm::dmat2x4>(key)) { glm::mat2x4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat3x2 = "   if (d.hasValue<glm::dmat3x2>(key)) { glm::mat3x2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat3x3 = "   if (d.hasValue<glm::dmat3x3>(key)) { glm::mat3x3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat3x4 = "   if (d.hasValue<glm::dmat3x4>(key)) { glm::mat3x4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat4x2 = "   if (d.hasValue<glm::dmat4x2>(key)) { glm::mat4x2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat4x3 = "   if (d.hasValue<glm::dmat4x3>(key)) { glm::mat4x3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterMat4x4 = "   if (d.hasValue<glm::dmat4x4>(key)) { glm::mat4x4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat2x2 = "   if (d.hasValue<glm::dmat2x2>(key)) { glm::dmat2x2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat2x3 = "   if (d.hasValue<glm::dmat2x3>(key)) { glm::dmat2x3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat2x4 = "   if (d.hasValue<glm::dmat2x4>(key)) { glm::dmat2x4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat3x2 = "   if (d.hasValue<glm::dmat3x2>(key)) { glm::dmat3x2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat3x3 = "   if (d.hasValue<glm::dmat3x3>(key)) { glm::dmat3x3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat3x4 = "   if (d.hasValue<glm::dmat3x4>(key)) { glm::dmat3x4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat4x2 = "   if (d.hasValue<glm::dmat4x2>(key)) { glm::dmat4x2 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat4x3 = "   if (d.hasValue<glm::dmat4x3>(key)) { glm::dmat4x3 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDMat4x4 = "   if (d.hasValue<glm::dmat4x4>(key)) { glm::dmat4x4 v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
    constexpr std::string_view VariantConverterDictionary = "   if (d.hasValue<ghoul::Dictionary>(key)) { ghoul::Dictionary v; bakeTo(d, key, &v); *val = std::move(v); return; }\n";
} // namespace

std::string_view bakeFunctionForType(BasicType::Type type) {
    switch (type) {
        case BasicType::Type::Bool:       return BakeFunctionBool;
        case BasicType::Type::Int:        return BakeFunctionInt;
        case BasicType::Type::Double:     return BakeFunctionDouble;
        case BasicType::Type::Float:      return BakeFunctionFloat;
        case BasicType::Type::String:     return BakeFunctionString;
        case BasicType::Type::Path:       return BakeFunctionPath;
        case BasicType::Type::Ivec2:      return BakeFunctionIVec2;
        case BasicType::Type::Ivec3:      return BakeFunctionIVec3;
        case BasicType::Type::Ivec4:      return BakeFunctionIVec4;
        case BasicType::Type::Dvec2:      return BakeFunctionDVec2;
        case BasicType::Type::Dvec3:      return BakeFunctionDVec3;
        case BasicType::Type::Dvec4:      return BakeFunctionDVec4;
        case BasicType::Type::Vec2:       return BakeFunctionVec2;
        case BasicType::Type::Vec3:       return BakeFunctionVec3;
        case BasicType::Type::Vec4:       return BakeFunctionVec4;
        case BasicType::Type::Mat2x2:     return BakeFunctionMat2x2;
        case BasicType::Type::Mat2x3:     return BakeFunctionMat2x3;
        case BasicType::Type::Mat2x4:     return BakeFunctionMat2x4;
        case BasicType::Type::Mat3x2:     return BakeFunctionMat3x2;
        case BasicType::Type::Mat3x3:     return BakeFunctionMat3x3;
        case BasicType::Type::Mat3x4:     return BakeFunctionMat3x4;
        case BasicType::Type::Mat4x2:     return BakeFunctionMat4x2;
        case BasicType::Type::Mat4x3:     return BakeFunctionMat4x3;
        case BasicType::Type::Mat4x4:     return BakeFunctionMat4x4;
        case BasicType::Type::DMat2x2:    return BakeFunctionDMat2x2;
        case BasicType::Type::DMat2x3:    return BakeFunctionDMat2x3;
        case BasicType::Type::DMat2x4:    return BakeFunctionDMat2x4;
        case BasicType::Type::DMat3x2:    return BakeFunctionDMat3x2;
        case BasicType::Type::DMat3x3:    return BakeFunctionDMat3x3;
        case BasicType::Type::DMat3x4:    return BakeFunctionDMat3x4;
        case BasicType::Type::DMat4x2:    return BakeFunctionDMat4x2;
        case BasicType::Type::DMat4x3:    return BakeFunctionDMat4x3;
        case BasicType::Type::DMat4x4:    return BakeFunctionDMat4x4;
        case BasicType::Type::Dictionary: return BakeFunctionDictionary;
        default:                          throw std::logic_error("Missing case label");
    }
}

std::string vectorBakeFunctionForType(std::string_view type) {
    return std::format(
        "   if (d.hasValue<ghoul::Dictionary>(key)) {{ {} v; bakeTo(d, key, &v); *val = std::move(v); return; }}\n",
        type
    );
}

std::string enumBakeFunctionForType(std::string_view type) {
    return std::format(
        "   if (d.hasValue<std::string>(key)) {{ {} v; bakeTo(d, key, &v); *val = std::move(v); return; }}\n",
        type
    );
}

std::string_view variantConversionFunctionForType(std::string_view type) {
    assert(!type.empty());
    static std::unordered_map<std::string_view, std::string_view> ConvertFunctions = {
        { "bool",                  VariantConverterBool },
        { "int",                   VariantConverterInt },
        { "double",                VariantConverterDouble },
        { "float",                 VariantConverterFloat },
        { "std::string",           VariantConverterString },
        { "std::filesystem::path", VariantConverterPath },
        { "glm::ivec2",            VariantConverterIVec2 },
        { "glm::ivec3",            VariantConverterIVec3 },
        { "glm::ivec4",            VariantConverterIVec4 },
        { "glm::dvec2",            VariantConverterDVec2 },
        { "glm::dvec3",            VariantConverterDVec3 },
        { "glm::dvec4",            VariantConverterDVec4 },
        { "glm::vec2",             VariantConverterVec2 },
        { "glm::vec3",             VariantConverterVec3 },
        { "glm::vec4",             VariantConverterVec4 },
        { "glm::mat2x2",           VariantConverterMat2x2 },
        { "glm::mat2x3",           VariantConverterMat2x3 },
        { "glm::mat2x4",           VariantConverterMat2x4 },
        { "glm::mat3x2",           VariantConverterMat3x2 },
        { "glm::mat3x3",           VariantConverterMat3x3 },
        { "glm::mat3x4",           VariantConverterMat3x4 },
        { "glm::mat4x2",           VariantConverterMat4x2 },
        { "glm::mat4x3",           VariantConverterMat4x3 },
        { "glm::mat4x4",           VariantConverterMat4x4 },
        { "glm::dmat2x2",          VariantConverterDMat2x2 },
        { "glm::dmat2x3",          VariantConverterDMat2x3 },
        { "glm::dmat2x4",          VariantConverterDMat2x4 },
        { "glm::dmat3x2",          VariantConverterDMat3x2 },
        { "glm::dmat3x3",          VariantConverterDMat3x3 },
        { "glm::dmat3x4",          VariantConverterDMat3x4 },
        { "glm::dmat4x2",          VariantConverterDMat4x2 },
        { "glm::dmat4x3",          VariantConverterDMat4x3 },
        { "glm::dmat4x4",          VariantConverterDMat4x4 },
        { "ghoul::Dictionary",     VariantConverterDictionary }
    };

    const auto it = ConvertFunctions.find(type);
    assert(it != ConvertFunctions.end());
    return it->second;
}

std::string enumToEnumMapping(Enum* e) {
    assert(e);
    assert(!e->attributes.mappedTo.empty());
    std::string mappedTo = e->attributes.mappedTo;
    std::string fullyQualifiedName = fqn(e, "::");
    std::string result = std::format(R"(
template <> [[maybe_unused]] {0} map<{0}, {1}>({1} value) {{
    switch (value) {{
        // If you end up here following a compiler error saying something about
        // 'illegal qualified name in member declaration' or such nonsense, then you tried
        // to map an enum A to another enum B and A has an enum value that B does not
        // have. For example enum class A {{ Value1, Value2 }}; enum class B {{ Value1 }};
        // would trigger that error on trying to access B::Value2 wich is an illegal
        // qualified name. Make the enums match each other and run codegen again)",
        mappedTo, fullyQualifiedName
    );

    for (EnumElement* ee : e->elements) {
        result += std::format(R"(
        case {0}::{2}:  return {1}::{2};)",
            fullyQualifiedName, e->attributes.mappedTo, ee->name
        );
    }

    result += std::format(R"(
        default:
            throw "This default label is not necessary since the case labels are "
                  "exhaustive, but not having it makes Visual Studio cranky";
    }}
}}
)"
    );

    return result;
}

std::string enumArrayify(Enum* e) {
    assert(e);
    assert(e->attributes.arrayify);
    assert(!e->elements.empty());

    std::string fullyQualifiedName = fqn(e, "::");
    std::string result = std::format(R"(
template <> [[maybe_unused]] std::vector<{0}> arrayify() {{
    return {{
)", fullyQualifiedName);

    for (EnumElement* elem : e->elements) {
        result += std::format("        {0}::{1},\n", fullyQualifiedName, elem->name);
    }

    // Remove the terminating ,  We first remove the \n, but have to put it back after
    result.pop_back();
    result.pop_back();
    result += '\n';
    result += "    };\n}";

    return result;
}
