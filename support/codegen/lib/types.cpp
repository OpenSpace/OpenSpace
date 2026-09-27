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

#include "types.h"

#include "util.h"
#include <algorithm>
#include <cassert>
#include <charconv>
#include <format>
#include <functional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

namespace {
    std::string join(const std::vector<std::string_view>& list, std::string_view sep) {
        assert(
            std::none_of(list.begin(), list.end(), std::mem_fn(&std::string_view::empty))
        );
        assert(!sep.empty());

        size_t size = 0;
        for (const std::string_view l : list) {
            size += l.size();
        }
        // This allocates space for one sep more than needed, but it simplifies the loop
        size += sep.size() * (list.size() - 1);

        std::string res;
        res.reserve(size);
        for (const std::string_view l : list) {
            res.append(l.data(), l.size());
            res.append(sep.data(), sep.size());
        }
        // Remove the last separator
        for (size_t i = 0; i < sep.size(); i++) {
            res.pop_back();
        }
        return res;
    }

    bool operator==(const StackElement& lhs, const StackElement& rhs) {
        return lhs.type == rhs.type && lhs.name == rhs.name &&
            lhs.comment == rhs.comment && lhs.parent == rhs.parent;
    }
} // namespace

CodegenError::CodegenError(const std::string& e) : std::runtime_error(e) {}
CodegenError::~CodegenError() noexcept = default;
CodegenError::operator std::string() const noexcept { return what(); }

bool VariableType::isBasicType() const { return tag == Tag::BasicType; }
bool VariableType::isPointerType() const { return tag == Tag::PointerType; }
bool VariableType::isMapType() const { return tag == Tag::MapType; }
bool VariableType::isOptionalType() const { return tag == Tag::OptionalType; }
bool VariableType::isVariantType() const { return tag == Tag::VariantType; }
bool VariableType::isTupleType() const { return tag == Tag::TupleType; }
bool VariableType::isArrayType() const { return tag == Tag::ArrayType; }
bool VariableType::isVectorType() const { return tag == Tag::VectorType; }
bool VariableType::isCustomType() const { return tag == Tag::CustomType; }

bool VariableType::containsCustomType() const {
    switch (tag) {
        case Tag::BasicType:
            return false;
        case Tag::PointerType:
            return false;
        case Tag::MapType:
            return static_cast<const MapType*>(this)->keyType->containsCustomType() ||
                   static_cast<const MapType*>(this)->valueType->containsCustomType();
        case Tag::OptionalType:
            return static_cast<const OptionalType*>(this)->type->containsCustomType();
        case Tag::VariantType:
            return std::any_of(
                static_cast<const VariantType*>(this)->types.begin(),
                static_cast<const VariantType*>(this)->types.end(),
                std::mem_fn(&VariableType::containsCustomType)
            );
        case Tag::TupleType:
            return std::any_of(
                static_cast<const TupleType*>(this)->types.begin(),
                static_cast<const TupleType*>(this)->types.end(),
                std::mem_fn(&VariableType::containsCustomType)
            );
        case Tag::ArrayType:
            return static_cast<const ArrayType*>(this)->type->containsCustomType();
        case Tag::VectorType:
            return static_cast<const VectorType*>(this)->type->containsCustomType();
        case Tag::CustomType:
            return true;
        default:
            throw std::logic_error("Missing case label");
    }
}

bool operator==(const VariableType& lhs, const VariableType& rhs) {
    if (lhs.tag != rhs.tag) {
        return false;
    }

    using Tag = VariableType::Tag;

    switch (lhs.tag) {
        case Tag::BasicType:
            return static_cast<const BasicType&>(lhs) ==
                   static_cast<const BasicType&>(rhs);
        case Tag::PointerType:
            return static_cast<const PointerType&>(lhs) ==
                   static_cast<const PointerType&>(rhs);
        case Tag::MapType:
            return static_cast<const MapType&>(lhs) ==
                   static_cast<const MapType&>(rhs);
        case Tag::OptionalType:
            return static_cast<const OptionalType&>(lhs) ==
                   static_cast<const OptionalType&>(rhs);
        case Tag::VariantType:
            return static_cast<const VariantType&>(lhs) ==
                   static_cast<const VariantType&>(rhs);
        case Tag::TupleType:
            return static_cast<const TupleType&>(lhs) ==
                   static_cast<const TupleType&>(rhs);
        case Tag::ArrayType:
            return static_cast<const ArrayType&>(lhs) ==
                   static_cast<const ArrayType&>(rhs);
        case Tag::VectorType:
            return static_cast<const VectorType&>(lhs) ==
                   static_cast<const VectorType&>(rhs);
        case Tag::CustomType:
            return static_cast<const CustomType&>(lhs) ==
                   static_cast<const CustomType&>(rhs);
        default:
            throw std::logic_error("Missing case label");
    }
}

bool operator==(const BasicType& lhs, const BasicType& rhs) {
    return lhs.type == rhs.type;
}

bool operator==(const PointerType& lhs, const PointerType& rhs) {
    return lhs.type == rhs.type;
}

bool operator==(const MapType& lhs, const MapType& rhs) {
    return (*lhs.keyType == *rhs.keyType) && (*lhs.valueType == *rhs.valueType);
}

bool operator==(const OptionalType& lhs, const OptionalType& rhs) {
    return *lhs.type == *rhs.type;
}

bool operator==(const VariantType& lhs, const VariantType& rhs) {
    if (lhs.types.size() != rhs.types.size()) {
        return false;
    }

    for (size_t i = 0; i < lhs.types.size(); i++) {
        if (!(*lhs.types[i] == *rhs.types[i])) {
            return false;
        }
    }
    return true;
}

bool operator==(const TupleType& lhs, const TupleType& rhs) {
    if (lhs.types.size() != rhs.types.size()) {
        return false;
    }

    for (size_t i = 0; i < lhs.types.size(); i++) {
        if (!(*lhs.types[i] == *rhs.types[i])) {
            return false;
        }
    }
    return true;
}

bool operator==(const ArrayType& lhs, const ArrayType& rhs) {
    return *lhs.type == *rhs.type && lhs.size == rhs.size;
}

bool operator==(const VectorType& lhs, const VectorType& rhs) {
    return *lhs.type == *rhs.type;
}

bool operator==(const CustomType& lhs, const CustomType& rhs) {
    return *lhs.type == *rhs.type;
}

const Struct* rootStruct(const Struct* s) {
    assert(s);

    while (s->parent) {
        s = s->parent;
    }
    return s;
}

const StackElement* resolveType(const Struct* context, std::string_view type) {
    assert(context);
    assert(!type.empty());

    if (context->name == type) {
        return context;
    }

    // Check the children of the context first
    const auto it = std::find_if(
        context->children.cbegin(), context->children.cend(),
        [type](StackElement* e) { return e->name == type; }
    );
    if (it != context->children.end()) {
        return *it;
    }

    // Then check the parent
    if (context->parent) {
        const StackElement* s = resolveType(context->parent, type);
        if (s) {
            return s;
        }
    }

    // If we got this far, noone knew what to do with the type
    return nullptr;
}

std::string fqn(const StackElement* s, std::string_view separator) {
    assert(s);
    assert(!separator.empty());

    std::vector<std::string_view> names;
    names.push_back(s->name);
    while (s->parent) {
        s = s->parent;
        names.push_back(s->name);
    }
    std::reverse(names.begin(), names.end());

    assert(
        std::none_of(names.cbegin(), names.cend(), std::mem_fn(&std::string_view::empty))
    );

    return join(names, separator);
}

VariableType* parseType(std::string_view type, Struct* context) {
    using namespace std::literals;

    if (type == "void") {
        return nullptr;
    }

    assert(!type.empty());

    if (type[type.size() - 1] == '&') {
        throw CodegenError(std::format("Illegal reference type found: {}", type));
    }

    auto newType = [](BasicType::Type t) -> BasicType* {
        BasicType* bt = new BasicType;
        bt->tag = VariableType::Tag::BasicType;
        bt->type = t;
        return bt;
    };

    VariableType* t = nullptr;
    // NOLINTBEGIN(bugprone-branch-clone)
    if (type == "bool")                       { t = newType(BasicType::Type::Bool); }
    else if (type == "int")                   { t = newType(BasicType::Type::Int); }
    else if (type == "double")                { t = newType(BasicType::Type::Double); }
    else if (type == "float")                 { t = newType(BasicType::Type::Float); }
    else if (type == "std::string")           { t = newType(BasicType::Type::String); }
    else if (type == "std::filesystem::path") { t = newType(BasicType::Type::Path); }
    else if (type == "glm::ivec2")            { t = newType(BasicType::Type::Ivec2); }
    else if (type == "glm::ivec3")            { t = newType(BasicType::Type::Ivec3); }
    else if (type == "glm::ivec4")            { t = newType(BasicType::Type::Ivec4); }
    else if (type == "glm::dvec2")            { t = newType(BasicType::Type::Dvec2); }
    else if (type == "glm::dvec3")            { t = newType(BasicType::Type::Dvec3); }
    else if (type == "glm::dvec4")            { t = newType(BasicType::Type::Dvec4); }
    else if (type == "glm::vec2")             { t = newType(BasicType::Type::Vec2); }
    else if (type == "glm::vec3")             { t = newType(BasicType::Type::Vec3); }
    else if (type == "glm::vec4")             { t = newType(BasicType::Type::Vec4); }
    else if (type == "glm::mat2x2")           { t = newType(BasicType::Type::Mat2x2); }
    else if (type == "glm::mat2")             { t = newType(BasicType::Type::Mat2x2); }
    else if (type == "glm::mat2x3")           { t = newType(BasicType::Type::Mat2x3); }
    else if (type == "glm::mat2x4")           { t = newType(BasicType::Type::Mat2x4); }
    else if (type == "glm::mat3x2")           { t = newType(BasicType::Type::Mat3x2); }
    else if (type == "glm::mat3x3")           { t = newType(BasicType::Type::Mat3x3); }
    else if (type == "glm::mat3")             { t = newType(BasicType::Type::Mat3x3); }
    else if (type == "glm::mat3x4")           { t = newType(BasicType::Type::Mat3x4); }
    else if (type == "glm::mat4x2")           { t = newType(BasicType::Type::Mat4x2); }
    else if (type == "glm::mat4x3")           { t = newType(BasicType::Type::Mat4x3); }
    else if (type == "glm::mat4x4")           { t = newType(BasicType::Type::Mat4x4); }
    else if (type == "glm::mat4")             { t = newType(BasicType::Type::Mat4x4); }
    else if (type == "glm::dmat2x2")          { t = newType(BasicType::Type::DMat2x2); }
    else if (type == "glm::dmat2")            { t = newType(BasicType::Type::DMat2x2); }
    else if (type == "glm::dmat2x3")          { t = newType(BasicType::Type::DMat2x3); }
    else if (type == "glm::dmat2x4")          { t = newType(BasicType::Type::DMat2x4); }
    else if (type == "glm::dmat3x2")          { t = newType(BasicType::Type::DMat3x2); }
    else if (type == "glm::dmat3x3")          { t = newType(BasicType::Type::DMat3x3); }
    else if (type == "glm::dmat3")            { t = newType(BasicType::Type::DMat3x3); }
    else if (type == "glm::dmat3x4")          { t = newType(BasicType::Type::DMat3x4); }
    else if (type == "glm::dmat4x2")          { t = newType(BasicType::Type::DMat4x2); }
    else if (type == "glm::dmat4x3")          { t = newType(BasicType::Type::DMat4x3); }
    else if (type == "glm::dmat4x4")          { t = newType(BasicType::Type::DMat4x4); }
    else if (type == "glm::dmat4")            { t = newType(BasicType::Type::DMat4x4); }
    else if (type == "ghoul::Dictionary")    { t = newType(BasicType::Type::Dictionary); }
    // NOLINTEND(bugprone-branch-clone)
    else if (startsWith(type, "std::vector<")) {
        type.remove_prefix("std::vector<"sv.size());
        type.remove_suffix(">"sv.size());
        type = strip(type);

        VectorType* vt = new VectorType;
        vt->tag = VariableType::Tag::VectorType;
        vt->type = parseType(type, context);
        assert(vt->type);
        t = vt;
    }
    else if (startsWith(type, "std::array<")) {
        type.remove_prefix("std::array<"sv.size());
        type.remove_suffix(">"sv.size());

        const size_t separator = type.rfind(',');
        if (separator == std::string_view::npos) {
            throw CodegenError(std::format(
                "Invalid array specification, missing comma: {}", type
            ));

        }

        std::string_view count = type.substr(separator + 1);
        count = strip(count);
        type = type.substr(0, separator);
        type = strip(type);

        ArrayType* at = new ArrayType;
        at->tag = VariableType::Tag::ArrayType;
        at->type = parseType(type, context);
        assert(at->type);
        int size = -1;
        auto result = std::from_chars(count.data(), count.data() + count.size(), size);
        if (result.ec == std::errc::invalid_argument) {
            throw CodegenError(std::format(
                "Invalid array specification, could not convert size to integer: {}", type
            ));
        }
        at->size = size;
        t = at;
    }
    else if (startsWith(type, "std::optional<")) {
        type.remove_prefix("std::optional<"sv.size());
        type.remove_suffix(">"sv.size());
        type = strip(type);

        OptionalType* op = new OptionalType;
        op->tag = VariableType::Tag::OptionalType;
        op->type = parseType(type, context);
        assert(op->type);
        t = op;
    }
    else if (startsWith(type, "std::map<")) {
        type.remove_prefix("std::map<"sv.size());

        std::vector<std::string_view> list = extractTemplateTypeList(type);
        assert(list.size() == 2);

        MapType* mp = new MapType;
        mp->tag = VariableType::Tag::MapType;
        mp->keyType = parseType(list[0], context);
        assert(mp->keyType);
        mp->valueType = parseType(list[1], context);
        assert(mp->valueType);

        const bool isValidKey = mp->hasStringKey() || mp->hasEnumKey();
        if (!isValidKey) {
            throw CodegenError(std::format(
                "Currently only std::string or enums as key is supported\n{}", type
            ));
        }

        t = mp;
    }
    else if (startsWith(type, "std::variant<")) {
        type.remove_prefix("std::variant<"sv.size());

        const std::vector<std::string_view> list = extractTemplateTypeList(type);

        VariantType* vt = new VariantType;
        vt->tag = VariableType::Tag::VariantType;

        for (const std::string_view elem : list) {
            VariableType* listType = parseType(elem, context);
            assert(listType);
            vt->types.push_back(listType);
        }

        // Check for multiple vector types
        int nVectorTypes = 0;
        for (VariableType* listType : vt->types) {
            if (listType->tag == VariableType::Tag::VectorType) {
                nVectorTypes += 1;
            }
        }
        if (nVectorTypes > 1) {
            throw CodegenError(std::format(
                "We can't have a variant containing multiple vector types, try a "
                "vector of variants instead\n{}", type
            ));
        }

        // Check for illegal types
        for (VariableType* lt : vt->types) {
            const bool isEnum =
                lt->isCustomType() && static_cast<CustomType*>(lt)->type &&
                static_cast<CustomType*>(lt)->type->type == StackElement::Type::Enum;
            if (!lt->isBasicType() && !lt->isVectorType() && !isEnum) {
                throw CodegenError(std::format(
                    "Unsupported type '{}' found in variant list\n{}",
                    generateTypename(lt), type
                ));
            }
        }

        t = vt;
    }
    else if (startsWith(type, "std::tuple<")) {
        type.remove_prefix("std::tuple<"sv.size());

        const std::vector<std::string_view> list = extractTemplateTypeList(type);

        TupleType* tt = new TupleType;
        tt->tag = VariableType::Tag::TupleType;

        for (const std::string_view elem : list) {
            VariableType* listType = parseType(elem, context);
            assert(listType);
            tt->types.push_back(listType);
        }

        t = tt;
    }
    else if (type[type.size() - 1] == '*') {
        PointerType* tt = new PointerType;
        tt->tag = VariableType::Tag::PointerType;
        // Remove the trailing *
        tt->type = type.substr(0, type.size() - 1);
        t = tt;
    }


    if (!t) {
        // We don't have a standard type, so 'type' must now be a struct or an enum
        // visible in the variable's context
        if (!context) {
            throw CodegenError(std::format(
                "Type detected that codegen doesn't know how to handle: '{}'", type
            ));
        }

        CustomType* ct = new CustomType;
        ct->tag = VariableType::Tag::CustomType;
        ct->name = std::string(type);
        const StackElement* el = resolveType(context, type);
        if (!el) {
            throw CodegenError(std::format(
                "Type detected that codegen doesn't know how to handle: '{}'", type
            ));
        }
        ct->type = el;
        t = ct;
    }
    assert(t);
    return t;
}

bool MapType::hasStringKey() const {
    return (keyType->tag == VariableType::Tag::BasicType) &&
           (static_cast<BasicType*>(keyType)->type == BasicType::Type::String);
}

bool MapType::hasEnumKey() const {
    return (keyType->tag == VariableType::Tag::CustomType) &&
           (static_cast<CustomType*>(keyType)->type->type == StackElement::Type::Enum);
}

std::string generateTypename(BasicType::Type type) {
    switch (type) {
        case BasicType::Type::Bool:    return "bool";
        case BasicType::Type::Int:     return "int";
        case BasicType::Type::Double:  return "double";
        case BasicType::Type::Float:   return "float";
        case BasicType::Type::String:  return "std::string";
        case BasicType::Type::Path:    return "std::filesystem::path";
        case BasicType::Type::Ivec2:   return "glm::ivec2";
        case BasicType::Type::Ivec3:   return "glm::ivec3";
        case BasicType::Type::Ivec4:   return "glm::ivec4";
        case BasicType::Type::Dvec2:   return "glm::dvec2";
        case BasicType::Type::Dvec3:   return "glm::dvec3";
        case BasicType::Type::Dvec4:   return "glm::dvec4";
        case BasicType::Type::Vec2:    return "glm::vec2";
        case BasicType::Type::Vec3:    return "glm::vec3";
        case BasicType::Type::Vec4:    return "glm::vec4";
        case BasicType::Type::Mat2x2:  return "glm::mat2x2";
        case BasicType::Type::Mat2x3:  return "glm::mat2x3";
        case BasicType::Type::Mat2x4:  return "glm::mat2x4";
        case BasicType::Type::Mat3x2:  return "glm::mat3x2";
        case BasicType::Type::Mat3x3:  return "glm::mat3x3";
        case BasicType::Type::Mat3x4:  return "glm::mat3x4";
        case BasicType::Type::Mat4x2:  return "glm::mat4x2";
        case BasicType::Type::Mat4x3:  return "glm::mat4x3";
        case BasicType::Type::Mat4x4:  return "glm::mat4x4";
        case BasicType::Type::DMat2x2: return "glm::dmat2x2";
        case BasicType::Type::DMat2x3: return "glm::dmat2x3";
        case BasicType::Type::DMat2x4: return "glm::dmat2x4";
        case BasicType::Type::DMat3x2: return "glm::dmat3x2";
        case BasicType::Type::DMat3x3: return "glm::dmat3x3";
        case BasicType::Type::DMat3x4: return "glm::dmat3x4";
        case BasicType::Type::DMat4x2: return "glm::dmat4x2";
        case BasicType::Type::DMat4x3: return "glm::dmat4x3";
        case BasicType::Type::DMat4x4: return "glm::dmat4x4";
        case BasicType::Type::Dictionary: return "ghoul::Dictionary";
        default:                       throw std::logic_error("Missing case label");
    }
}

namespace {

    std::string generateLuaExtractionTypename(BasicType::Type type) {
        return generateTypename(type);
    }

    std::string generateDescriptiveTypename(BasicType::Type type) {
        switch (type) {
            case BasicType::Type::Bool:       return "Boolean";
            case BasicType::Type::Int:        return "Integer";
            case BasicType::Type::Double:     return "Number";
            case BasicType::Type::Float:      return "Number";
            case BasicType::Type::String:     return "String";
            case BasicType::Type::Path:       return "Path";
            case BasicType::Type::Ivec2:      return "ivec2";
            case BasicType::Type::Ivec3:      return "ivec3";
            case BasicType::Type::Ivec4:      return "ivec4";
            case BasicType::Type::Dvec2:      return "vec2";
            case BasicType::Type::Dvec3:      return "vec3";
            case BasicType::Type::Dvec4:      return "vec4";
            case BasicType::Type::Vec2:       return "vec2";
            case BasicType::Type::Vec3:       return "vec3";
            case BasicType::Type::Vec4:       return "vec4";
            case BasicType::Type::Mat2x2:     return "mat2x2";
            case BasicType::Type::Mat2x3:     return "mat2x3";
            case BasicType::Type::Mat2x4:     return "mat2x4";
            case BasicType::Type::Mat3x2:     return "mat3x2";
            case BasicType::Type::Mat3x3:     return "mat3x3";
            case BasicType::Type::Mat3x4:     return "mat3x4";
            case BasicType::Type::Mat4x2:     return "mat4x2";
            case BasicType::Type::Mat4x3:     return "mat4x3";
            case BasicType::Type::Mat4x4:     return "mat4x4";
            case BasicType::Type::DMat2x2:    return "mat2x2";
            case BasicType::Type::DMat2x3:    return "mat2x3";
            case BasicType::Type::DMat2x4:    return "mat2x4";
            case BasicType::Type::DMat3x2:    return "mat3x2";
            case BasicType::Type::DMat3x3:    return "mat3x3";
            case BasicType::Type::DMat3x4:    return "mat3x4";
            case BasicType::Type::DMat4x2:    return "mat4x2";
            case BasicType::Type::DMat4x3:    return "mat4x3";
            case BasicType::Type::DMat4x4:    return "mat4x4";
            case BasicType::Type::Dictionary: return "Table";
            default:                         throw std::logic_error("Missing case label");
        }
    }

    std::string generateTypename(const BasicType* type) {
        return generateTypename(type->type);
    }

    std::string generateLuaExtractionTypename(const BasicType* type) {
        return generateLuaExtractionTypename(type->type);
    }

    std::string generateDescriptiveTypename(const BasicType* type) {
        return generateDescriptiveTypename(type->type);
    }

    std::string generateTypename(const PointerType* type) {
        return std::format("{}*", type->type);
    }

    std::string generateLuaExtractionTypename(const PointerType* type) {
        return std::format("{}*", type->type);
    }

    std::string generateDescriptiveTypename(const PointerType* type) {
        return std::format("{}*", type->type);
    }

    std::string generateTypename(const MapType* type, bool fullyQualified) {
        std::string t1 = generateTypename(type->keyType, fullyQualified);
        std::string t2 = generateTypename(type->valueType, fullyQualified);
        return std::format("std::map<{}, {}>", t1, t2);
    }

    std::string generateLuaExtractionTypename(const MapType* type) {
        std::string t1 = generateLuaExtractionTypename(type->keyType);
        std::string t2 = generateLuaExtractionTypename(type->valueType);
        return std::format("std::map<{}, {}>", t1, t2);
    }

    std::string generateDescriptiveTypename(const MapType* type) {
        std::string t1 = generateDescriptiveTypename(type->keyType);
        std::string t2 = generateDescriptiveTypename(type->valueType);
        return std::format("{} -> {}", t1, t2);
    }

    std::string generateTypename(const OptionalType* type, bool fullyQualified) {
        std::string t1 = generateTypename(type->type, fullyQualified);
        return std::format("std::optional<{}>", t1);
    }

    std::string generateLuaExtractionTypename(const OptionalType* type) {
        std::string t1 = generateLuaExtractionTypename(type->type);
        return std::format("std::optional<{}>", t1);
    }

    std::string generateDescriptiveTypename(const OptionalType* type) {
        std::string t1 = generateDescriptiveTypename(type->type);
        return std::format("{}?", t1);
    }

    std::string generateTypename(const VariantType* type, bool fullyQualified) {
        std::string res = "std::variant<";
        for (VariableType* v : type->types) {
            res += generateTypename(v, fullyQualified);
            res += ", ";
        }

        res.pop_back();   // Remove the final ' '
        res.back() = '>'; // Replace the final , with the >

        return res;
    }

    std::string generateLuaExtractionTypename(const VariantType* type) {
        std::string res = "std::variant<";
        for (VariableType* v : type->types) {
            res += generateLuaExtractionTypename(v);
            res += ", ";
        }

        res.pop_back();   // Remove the final ' '
        res.back() = '>'; // Replace the final , with the >

        return res;
    }

    std::string generateDescriptiveTypename(const VariantType* type) {
        std::string res;
        for (size_t i = 0; i < type->types.size(); i += 1) {
            res += generateDescriptiveTypename(type->types[i]);

            if (i != type->types.size() - 1) {
                res += " | ";
            }
        }
        return res;
    }

    std::string generateTypename(const TupleType* type, bool fullyQualified) {
        std::string res = "std::tuple<";
        for (VariableType* v : type->types) {
            res += generateTypename(v, fullyQualified);
            res += ", ";
        }

        res.pop_back();   // Remove the final ' '
        res.back() = '>'; // Replace the final , with the >

        return res;
    }

    std::string generateLuaExtractionTypename(const TupleType* type) {
        std::string res = "std::tuple<";
        for (VariableType* v : type->types) {
            res += generateLuaExtractionTypename(v);
            res += ", ";
        }

        res.pop_back();   // Remove the final ' '
        res.back() = '>'; // Replace the final , with the >

        return res;
    }

    std::string generateDescriptiveTypename(const TupleType* type) {
        std::string res;
        for (size_t i = 0; i < type->types.size(); i += 1) {
            res += generateDescriptiveTypename(type->types[i]);

            if (i != type->types.size() - 1) {
                res += ", ";
            }
        }
        return std::format("({})", res);
    }

    std::string generateTypename(const ArrayType* type, bool fullyQualified) {
        std::string t1 = generateTypename(type->type, fullyQualified);
        return std::format("std::array<{}, {}>", t1, type->size);
    }

    std::string generateLuaExtractionTypename(const ArrayType* type) {
        std::string t1 = generateLuaExtractionTypename(type->type);
        return std::format("std::array<{}, {}>", t1, type->size);
    }

    std::string generateDescriptiveTypename(const ArrayType* type) {
        return std::format("{}[{}]", generateDescriptiveTypename(type->type), type->size);
    }

    std::string generateTypename(const VectorType* type, bool fullyQualified) {
        std::string t1 = generateTypename(type->type, fullyQualified);
        return std::format("std::vector<{}>", t1);
    }

    std::string generateLuaExtractionTypename(const VectorType* type) {
        std::string t1 = generateLuaExtractionTypename(type->type);
        return std::format("std::vector<{}>", t1);
    }

    std::string generateDescriptiveTypename(const VectorType* type) {
        return generateDescriptiveTypename(type->type) + "[]";
    }

    std::string generateTypename(const CustomType* type, bool fullyQualified) {
        if (fullyQualified) {
            return fqn(type->type, "::");
        }
        else {
            return
                type->type ?
                type->type->name : // If we have a type, we can return its proper name
                type->name;        // If we don't have a type, we failed where no context
                                   // for this custom type was available
        }
    }

    std::string generateLuaExtractionTypename(const CustomType* type) {
        // Yo dawg, I heard you like types
        switch (type->type->type) {
            case StackElement::Type::Enum:   return "std::string";
            case StackElement::Type::Struct: return "ghoul::Dictionary";
            default:                         throw std::logic_error("Missing case label");
        }
    }

    std::string generateDescriptiveTypename(const CustomType* type) {
        return type->type->name;
    }
} // namespace

std::string generateTypename(const VariableType* type, bool fullyQualified) {
    assert(type);

    const bool fq = fullyQualified;
    switch (type->tag) {
        case VariableType::Tag::BasicType:
            return generateTypename(static_cast<const BasicType*>(type));
        case VariableType::Tag::PointerType:
            return generateTypename(static_cast<const PointerType*>(type));
        case VariableType::Tag::MapType:
            return generateTypename(static_cast<const MapType*>(type), fq);
        case VariableType::Tag::OptionalType:
            return generateTypename(static_cast<const OptionalType*>(type), fq);
        case VariableType::Tag::VariantType:
            return generateTypename(static_cast<const VariantType*>(type), fq);
        case VariableType::Tag::TupleType:
            return generateTypename(static_cast<const TupleType*>(type), fq);
        case VariableType::Tag::ArrayType:
            return generateTypename(static_cast<const ArrayType*>(type), fq);
        case VariableType::Tag::VectorType:
            return generateTypename(static_cast<const VectorType*>(type), fq);
        case VariableType::Tag::CustomType:
            return generateTypename(static_cast<const CustomType*>(type), fq);
        default:
            throw std::logic_error("Missing case label");
    }
}

std::string generateLuaExtractionTypename(const VariableType* type) {
    assert(type);

    switch (type->tag) {
        case VariableType::Tag::BasicType:
            return generateLuaExtractionTypename(static_cast<const BasicType*>(type));
        case VariableType::Tag::PointerType:
            return generateLuaExtractionTypename(static_cast<const PointerType*>(type));
        case VariableType::Tag::MapType:
            return generateLuaExtractionTypename(static_cast<const MapType*>(type));
        case VariableType::Tag::OptionalType:
            return generateLuaExtractionTypename(static_cast<const OptionalType*>(type));
        case VariableType::Tag::VariantType:
            return generateLuaExtractionTypename(static_cast<const VariantType*>(type));
        case VariableType::Tag::TupleType:
            return generateLuaExtractionTypename(static_cast<const TupleType*>(type));
        case VariableType::Tag::ArrayType:
            return generateLuaExtractionTypename(static_cast<const ArrayType*>(type));
        case VariableType::Tag::VectorType:
            return generateLuaExtractionTypename(static_cast<const VectorType*>(type));
        case VariableType::Tag::CustomType:
            return generateLuaExtractionTypename(static_cast<const CustomType*>(type));
        default:
            throw std::logic_error("Missing case label");
    }
}

std::string generateDescriptiveTypename(const VariableType* type) {
    assert(type);

    switch (type->tag) {
        case VariableType::Tag::BasicType:
            return generateDescriptiveTypename(static_cast<const BasicType*>(type));
        case VariableType::Tag::PointerType:
            return generateDescriptiveTypename(static_cast<const PointerType*>(type));
        case VariableType::Tag::MapType:
            return generateDescriptiveTypename(static_cast<const MapType*>(type));
        case VariableType::Tag::OptionalType:
            return generateDescriptiveTypename(static_cast<const OptionalType*>(type));
        case VariableType::Tag::VariantType:
            return generateDescriptiveTypename(static_cast<const VariantType*>(type));
        case VariableType::Tag::TupleType:
            return generateDescriptiveTypename(static_cast<const TupleType*>(type));
        case VariableType::Tag::ArrayType:
            return generateDescriptiveTypename(static_cast<const ArrayType*>(type));
        case VariableType::Tag::VectorType:
            return generateDescriptiveTypename(static_cast<const VectorType*>(type));
        case VariableType::Tag::CustomType:
            return generateDescriptiveTypename(static_cast<const CustomType*>(type));
        default:
            throw std::logic_error("Missing case label");
    }
}
