/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/invariants.h>
#include <algorithm>
#include <array>
#include <charconv>
#include <optional>
#include <string_view>
#include <type_traits>
#include <variant>
#include <vector>

namespace ghoul::lua {

namespace internal {

template <typename T> struct is_optional : std::false_type {};
template <typename T> struct is_optional<std::optional<T>> : std::true_type {};

template <typename... Ts> struct is_variant : std::false_type {};
template <typename... Ts> struct is_variant<std::variant<Ts...>> : std::true_type {};

template <typename T> struct is_string_map : std::false_type {};
template <typename T> struct is_string_map<std::map<std::string, T>> : std::true_type {};

template <typename T> struct is_vector : std::false_type {};
template <typename T> struct is_vector<std::vector<T>> : std::true_type {};

template <typename T> struct is_array : std::false_type {};
template <typename T, size_t N>
struct is_array<std::array<T, N>> : std::true_type {};

template <typename T> struct is_tuple : std::false_type {};
template <typename... Ts> struct is_tuple<std::tuple<Ts...>> : std::true_type {};

// Boolean constant used to check whether a value T is part of a parameter pack Ts
template <typename T, typename U> struct is_one_of;
template <typename T, typename... Ts>
struct is_one_of<T, std::variant<Ts...>> :
    std::bool_constant<(std::is_same_v<T, Ts> || ...)>
{};

template <size_t I = 0, typename... Ts>
constexpr void extractValues(lua_State* L, std::tuple<Ts...>& tuple, int baseLocation,
                             int nArguments, int& argumentsFound)
{
    using T = std::tuple_element_t<I, std::tuple<Ts...>>;

    if constexpr (is_optional<T>::value) {
        if (baseLocation + I > static_cast<size_t>(nArguments)) {
            // We have reached the end of the arguments and only have optional now, so we
            // can bail out
            return;
        }
        else {
            // We are looking at an optional parameter that is provided
            std::get<I>(tuple) = value<typename T::value_type>(
                L,
                baseLocation + I,
                PopValue::No
            );
            argumentsFound += 1;
        }
    }
    else {
        if (baseLocation + I > static_cast<size_t>(nArguments)) {
            throw LuaExecutionException("Too few arguments to Lua function call");
        }

        std::get<I>(tuple) = value<std::tuple_element_t<I, std::tuple<Ts...>>>(
            L,
            baseLocation + I,
            PopValue::No
        );
        argumentsFound += 1;
    }

    if constexpr (I+1 != sizeof...(Ts)) {
        extractValues<I+1>(L, tuple, baseLocation, nArguments, argumentsFound);
    }
}

template <size_t I = 0, typename... Ts>
constexpr void pushTupleValues(lua_State* L, const std::tuple<Ts...>& tuple) {
    ghoul::lua::push(L, static_cast<int>(I + 1), std::get<I>(tuple));
    lua_settable(L, -3);
    if constexpr (I + 1 != sizeof...(Ts)) {
        pushTupleValues<I + 1, Ts...>(L, tuple);
    }
}

enum class Phase {
    Mandatory,
    Optional
};

template <size_t I = 0, typename... Ts>
constexpr bool verifyParameters(Phase phase = Phase::Mandatory) {
    using T = std::tuple_element_t<I, std::tuple<Ts...>>;

    if constexpr (is_optional<T>::value) {
        phase = Phase::Optional;
    }
    else {
        if (phase == Phase::Optional) {
            // We are currently in the optional phase, but we have a non-optional type,
            // which is not allowed
            return false;
        }
    }

    if constexpr (I+1 != sizeof...(Ts)) {
        return verifyParameters<I+1, Ts...>(phase);
    }
    else {
        // We have reached the end without a single failure -> success
        return true;
    }
}

template <size_t I = 0, typename Variant>
constexpr bool hasVariantValue(lua_State* L, int location) {
    using T = std::variant_alternative_t<I, Variant>;

    bool res = hasValue<T>(L, location);
    if (res) {
        // We found the type, so we succeeded
        return true;
    }

    if constexpr (I+1 != std::variant_size_v<Variant>) {
        // We haven't finished with all of the types yes, so we got to continue
        return hasVariantValue<I+1, Variant>(L, location);
    }
    else {
        // We have reached the end of the variant list and haven't found the type in here,
        // so we're done
        return false;
    }
}

template <size_t I = 0, typename Variant>
constexpr Variant variantValue(lua_State* L, int location) {
    using T = std::variant_alternative_t<I, Variant>;

    if (hasValue<T>(L, location)) {
        return valueInner<T>(L, location);
    }

    if constexpr (I+1 != std::variant_size_v<Variant>) {
        // We haven't finished with all of the types yes, so we got to continue
        return variantValue<I+1, Variant>(L, location);
    }
    else {
        // We have reached the end of the variant list and haven't found the type in here,
        // so we're done
        throw LuaFormatException(std::format(
            "Unable to extract requested value '{}' out of the variant with type '{}' at "
            "parameter {}", typeid(T).name(), typeid(Variant).name(), location
        ));
    }
}

template <size_t I = 0>
void pushDictionaryHelper(lua_State* L, const Dictionary& d, std::string_view key) {
    if constexpr (I < std::variant_size_v<Dictionary::Types>) {
        using T = std::variant_alternative_t<I, Dictionary::Types>;
        bool has = d.hasValue<T>(key);
        if (has) {
            ghoul::lua::push(L, d.value<T>(key));
            return;
        }
        else {
            pushDictionaryHelper<I + 1>(L, d, key);
        }
    }
}

template <size_t I = 0, typename... Ts>
void extractValues(const Dictionary& dict, std::tuple<Ts...>& tuple) {
    using T = std::tuple_element_t<I, std::tuple<Ts...>>;

    std::vector<std::string_view> keys = dict.keys();
    std::sort(
        keys.begin(),
        keys.end(),
        [](std::string_view lhs, std::string_view rhs) {
            int lhsVal = 0;
            int rhsVal = 0;
            auto [p1, e1] = std::from_chars(lhs.data(), lhs.data() + lhs.size(), lhsVal);
            auto [p2, e2] = std::from_chars(rhs.data(), rhs.data() + rhs.size(), rhsVal);
            if (e1 == std::errc() && e2 == std::errc()) {
                return lhsVal < rhsVal;
            }
            else {
                return lhs < rhs;
            }
        }
    );

    if constexpr (std::is_same_v<T, float>) {
        std::get<I>(tuple) = static_cast<float>(dict.value<double>(keys[I]));
    }
    else if constexpr (std::is_same_v<T, int>) {
        std::get<I>(tuple) = static_cast<int>(dict.value<double>(keys[I]));
    }
    else {
        std::get<I>(tuple) = dict.value<T>(keys[I]);
    }
    if constexpr (I + 1 != sizeof...(Ts)) {
        extractValues<I + 1, Ts...>(dict, tuple);
    }
}

template <typename T, bool NumericKeys = false>
std::vector<T> dictionaryToVector(const Dictionary& d) {
    std::vector<std::string_view> keys = d.keys();
    if constexpr (NumericKeys) {
        std::sort(
            keys.begin(),
            keys.end(),
            [](std::string_view lhs, std::string_view rhs) {
                int lhsVal = 0;
                int rhsVal = 0;
                auto [p1, e1] = std::from_chars(
                    lhs.data(),
                    lhs.data() + lhs.size(),
                    lhsVal
                );
                auto [p2, e2] = std::from_chars(
                    rhs.data(),
                    rhs.data() + rhs.size(),
                    rhsVal
                );
                if (e1 == std::errc() && e2 == std::errc()) {
                    return lhsVal < rhsVal;
                }
                else {
                    return lhs < rhs;
                }
            }
        );
    }

    std::vector<T> res;
    res.reserve(keys.size());
    for (std::string_view k : keys) {
        if constexpr (std::is_same_v<T, float>) {
            T value = static_cast<float>(d.value<double>(k));
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, std::filesystem::path>) {
            T value = d.value<std::string>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::vec2>) {
            T value = d.value<glm::dvec2>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::vec3>) {
            T value = d.value<glm::dvec3>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::vec4>) {
            T value = d.value<glm::dvec4>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::ivec2>) {
            T value = d.value<glm::dvec2>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::ivec3>) {
            T value = d.value<glm::dvec3>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::ivec4>) {
            T value = d.value<glm::dvec4>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::uvec2>) {
            T value = d.value<glm::dvec2>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::uvec3>) {
            T value = d.value<glm::dvec3>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::uvec4>) {
            T value = d.value<glm::dvec4>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat2x2>) {
            T value = d.value<glm::dmat2x2>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat2x3>) {
            T value = d.value<glm::dmat2x3>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat2x4>) {
            T value = d.value<glm::dmat2x4>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat3x2>) {
            T value = d.value<glm::dmat3x2>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat3x3>) {
            T value = d.value<glm::dmat3x3>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat3x4>) {
            T value = d.value<glm::dmat3x4>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat4x2>) {
            T value = d.value<glm::dmat4x2>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat4x3>) {
            T value = d.value<glm::dmat4x3>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, glm::mat4x4>) {
            T value = d.value<glm::dmat4x4>(k);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_same_v<T, int>) {
            T value = static_cast<int>(d.value<double>(k));
            res.push_back(std::move(value));
        }
        else if constexpr (is_tuple<T>::value) {
            Dictionary dictVal = d.value<Dictionary>(k);
            T value;
            extractValues(dictVal, value);
            res.push_back(std::move(value));
        }
        else if constexpr (std::is_pointer_v<T>) {
            void* value = d.value<void*>(k);
            res.push_back(reinterpret_cast<T>(value));
        }
        else {
            T value = d.value<T>(k);
            res.push_back(std::move(value));
        }
    }
    return res;
}

template <typename T>
void push(lua_State* L, T value) {
    // We have to handle the floating point types first in this as floats are able to be
    // converted to ints, which would remove their fractional part. Same goes for the
    // const char* check before the std::is_pointer, since we want to handle the
    // specialized case first
    if constexpr (std::is_same_v<T, bool>) {
        lua_pushboolean(L, value ? 1 : 0);
    }
    else if constexpr (std::is_same_v<T, lua_Number>) {
        lua_pushnumber(L, std::move(value));
    }
    else if constexpr (std::is_same_v<T, lua_Integer>) {
        lua_pushinteger(L, std::move(value));
    }
    else if constexpr (std::is_floating_point_v<T>) {
        lua_pushnumber(L, static_cast<lua_Number&&>(std::move(value)));
    }
    else if constexpr (std::is_integral_v<T>) {
        lua_pushinteger(L, static_cast<lua_Integer&&>(std::move(value)));
    }
    else if constexpr (std::is_same_v<T, nil_t>) {
        (void)value; // Suppress an unused variable warning
        lua_pushnil(L);
    }
    else if constexpr (std::is_same_v<T, const char*>) {
        lua_pushstring(L, value);
    }
    else if constexpr (std::is_same_v<T, std::string>) {
        lua_pushlstring(L, value.c_str(), value.size());
    }
    else if constexpr (std::is_same_v<T, std::string_view>) {
        lua_pushlstring(L, value.data(), value.size());
    }
    else if constexpr (std::is_same_v<T, std::filesystem::path>) {
        const std::string s = value.string();
        lua_pushlstring(L, s.c_str(), s.size());
    }
    else if constexpr (std::is_same_v<T, std::vector<double>> ||
                       std::is_same_v<T, std::vector<float>>)
    {
        lua_newtable(L);
        for (size_t i = 0; i < value.size(); i++) {
            lua_pushinteger(L, i + 1);
            lua_pushnumber(L, std::move(value[i]));
            lua_settable(L, -3);
        }
    }
    else if constexpr (std::is_same_v<T, std::vector<int>>) {
        lua_newtable(L);
        for (size_t i = 0; i < value.size(); i++) {
            lua_pushinteger(L, i + 1);
            lua_pushinteger(L, std::move(value[i]));
            lua_settable(L, -3);
        }
    }
    else if constexpr (std::is_same_v<T, std::vector<const char*>>) {
        lua_newtable(L);
        for (size_t i = 0; i < value.size(); i++) {
            lua_pushinteger(L, i + 1);
            lua_pushstring(L, value[i]);
            lua_settable(L, -3);
        }
    }
    else if constexpr (std::is_same_v<T, std::vector<std::string>>) {
        lua_newtable(L);
        for (size_t i = 0; i < value.size(); i++) {
            lua_pushinteger(L, i + 1);
            lua_pushstring(L, value[i].c_str());
            lua_settable(L, -3);
        }
    }
    else if constexpr (std::is_pointer_v<T>) {
        lua_pushlightuserdata(L, reinterpret_cast<void*>(value));
    }
    else if constexpr (isGlmVector<T>()) {
        lua_newtable(L);
        int number = 1;
        for (glm::length_t i = 0; i < glm_components<T>::value; i++) {
            lua_pushnumber(L, static_cast<lua_Number>(value[i]));
            lua_rawseti(L, -2, number);
            number++;
        }
    }
    else if constexpr (isGlmMatrix<T>()) {
        lua_newtable(L);
        int number = 1;
        for (glm::length_t i = 0; i < T::type::row_type::length(); i++) {
            for (glm::length_t j = 0; j < T::type::col_type::length(); j++) {
                lua_pushnumber(L, static_cast<lua_Number>(value[i][j]));
                lua_rawseti(L, -2, number);
                number++;
            }
        }
    }
    else if constexpr (is_string_map<T>::value) {
        lua_newtable(L);
        for (const std::pair<const std::string, typename T::mapped_type>& p : value) {
            ghoul::lua::push(L, p.first, p.second);
            lua_settable(L, -3);
        }
    }
    else if constexpr (is_vector<T>::value) {
        lua_newtable(L);
        for (size_t i = 1; i <= value.size(); i++) {
            if constexpr (std::is_same_v<typename T::value_type, bool>) {
                ghoul::lua::push(L, static_cast<int>(i), static_cast<bool>(value[i - 1]));
            }
            else {
                ghoul::lua::push(L, static_cast<int>(i), value[i - 1]);
            }
            lua_settable(L, -3);
        }
    }
    else if constexpr (is_array<T>::value) {
        lua_newtable(L);
        for (size_t i = 1; i <= value.size(); i++) {
            if constexpr (std::is_same_v<typename T::value_type, bool>) {
                ghoul::lua::push(L, static_cast<int>(i), static_cast<bool>(value[i - 1]));
            }
            else {
                ghoul::lua::push(L, static_cast<int>(i), value[i - 1]);
            }
            lua_settable(L, -3);
        }
    }
    else if constexpr (is_tuple<T>::value) {
        lua_newtable(L);
        pushTupleValues(L, value);
    }
    else if constexpr (std::is_same_v<T, Dictionary>) {
        lua_newtable(L);
        for (std::string_view key : value.keys()) {
            ghoul::lua::push(L, key);
            pushDictionaryHelper(L, value, key);
            lua_settable(L, -3);
        }
    }
    else {
        (void) value; // Suppress an unused variable warning
        static_assert(sizeof(T) == 0, "Unable to push type T onto the Lua stack");
    }
}

template <typename T>
std::string Name();

template <size_t I = 0, typename Variant>
std::string VariantName() {
    using T = std::variant_alternative_t<I, Variant>;

    if constexpr (I + 1 != std::variant_size_v<Variant>) {
        return std::format("{} or {}", Name<T>(), VariantName<I + 1, Variant>());
    }
    else {
        return Name<T>();
    }
}

template <typename T>
std::string Name() {
    if constexpr (std::is_same_v<T, bool>) {
        return "Boolean";
    }
    else if constexpr (std::is_floating_point_v<T> || std::is_integral_v<T>) {
        return "Number";
    }
    else if constexpr (std::is_same_v<T, Dictionary> ||
                       internal::is_string_map<T>::value ||
                       internal::is_vector<T>::value ||
                       internal::is_array<T>::value || internal::is_tuple<T>::value ||
                       std::is_same_v<T, glm::ivec2> || std::is_same_v<T, glm::ivec3> ||
                       std::is_same_v<T, glm::ivec4> || std::is_same_v<T, glm::uvec2> ||
                       std::is_same_v<T, glm::uvec3> || std::is_same_v<T, glm::uvec4> ||
                       std::is_same_v<T, glm::dvec2> || std::is_same_v<T, glm::dvec3> ||
                       std::is_same_v<T, glm::dvec4> || std::is_same_v<T, glm::vec2> ||
                       std::is_same_v<T, glm::vec3>  || std::is_same_v<T, glm::vec4> ||
                       std::is_same_v<T, glm::dmat2x2> ||
                       std::is_same_v<T, glm::dmat2x3> ||
                       std::is_same_v<T, glm::dmat2x4> ||
                       std::is_same_v<T, glm::dmat3x2> ||
                       std::is_same_v<T, glm::dmat3x3> ||
                       std::is_same_v<T, glm::dmat3x4> ||
                       std::is_same_v<T, glm::dmat4x2> ||
                       std::is_same_v<T, glm::dmat4x3> ||
                       std::is_same_v<T, glm::dmat4x4> ||
                       std::is_same_v<T, glm::mat2x2> || std::is_same_v<T, glm::mat2x3> ||
                       std::is_same_v<T, glm::mat2x4> || std::is_same_v<T, glm::mat3x2> ||
                       std::is_same_v<T, glm::mat3x3> || std::is_same_v<T, glm::mat3x4> ||
                       std::is_same_v<T, glm::mat4x2> || std::is_same_v<T, glm::mat4x3> ||
                       std::is_same_v<T, glm::mat4x4>)
    {
        return "Table";
    }
    else if constexpr (is_variant<T>::value) {
        return internal::VariantName<0, T>();
    }
    else if constexpr (std::is_same_v<T, const char*> || std::is_same_v<T, std::string> ||
                       std::is_same_v<T, std::filesystem::path> ||
                       std::is_same_v<T, std::string_view>)
    {
        return "String";
    }
    else if constexpr (is_optional<T>::value) {
        return "[" + Name<typename T::value_type>() + "]";
    }
    else if constexpr (std::is_pointer_v<T>) {
        return "user pointer";
    }
    else {
        static_assert(sizeof(T) == 0, "Missing case for T");
    }
}

template <typename T>
T valueInner(lua_State* L, int location) {
    if (!hasValue<T>(L, location)) {
        std::string name = Name<T>();
        // If we get this far, none of the previous return statements were hit
        std::string error = std::format(
            "Expected type '{}' for parameter {} but got wrong type '{}' instead",
            name, location, luaTypeToString(lua_type(L, location))
        );
        // This function won't actually return, but C++ doesn't know that, so
        luaError(L, error.c_str());
        throw LuaFormatException(std::move(error));
    }

    if constexpr (std::is_same_v<T, bool>) {
        return lua_toboolean(L, location) == 1;
    }
    else if constexpr (std::is_same_v<T, lua_Number>) {
        return lua_tonumber(L, location);
    }
    else if constexpr (std::is_same_v<T, lua_Integer>) {
        return lua_tointeger(L, location);
    }
    else if constexpr (std::is_integral_v<T>) {
        return static_cast<T>(lua_tointeger(L, location));
    }
    else if constexpr (std::is_floating_point_v<T>) {
        return static_cast<T>(lua_tonumber(L, location));
    }
    else if constexpr (std::is_same_v<T, const char*> || std::is_same_v<T, std::string> ||
                       std::is_same_v<T, std::filesystem::path> ||
                       std::is_same_v<T, std::string_view>)
    {
        return lua_tostring(L, location);
    }
    else if constexpr (std::is_pointer_v<T>) {
        return reinterpret_cast<T>(lua_touserdata(L, location));
    }
    else if constexpr (std::is_same_v<T, Dictionary>) {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary d = luaDictionaryFromState(L);
        return d;
    }
    else if constexpr (std::is_same_v<T, glm::dvec2> || std::is_same_v<T, glm::dvec3> ||
                       std::is_same_v<T, glm::dvec4> || std::is_same_v<T, glm::dmat2x2> ||
                       std::is_same_v<T, glm::dmat2x3> ||
                       std::is_same_v<T, glm::dmat2x4> ||
                       std::is_same_v<T, glm::dmat3x2> ||
                       std::is_same_v<T, glm::dmat3x3> ||
                       std::is_same_v<T, glm::dmat3x4> ||
                       std::is_same_v<T, glm::dmat4x2> ||
                       std::is_same_v<T, glm::dmat4x3> || std::is_same_v<T, glm::dmat4x4>)
    {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary value = luaDictionaryFromState(L);
        Dictionary holder;
        holder.setValue("value", value);
        return holder.value<T>("value");
    }
    else if constexpr (std::is_same_v<T, glm::ivec2>) {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary value = luaDictionaryFromState(L);
        Dictionary holder;
        holder.setValue("value", value);
        return holder.value<glm::dvec2>("value");
    }
    else if constexpr (std::is_same_v<T, glm::ivec3>) {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary value = luaDictionaryFromState(L);
        Dictionary holder;
        holder.setValue("value", value);
        return holder.value<glm::dvec3>("value");
    }
    else if constexpr (std::is_same_v<T, glm::ivec4>) {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary value = luaDictionaryFromState(L);
        Dictionary holder;
        holder.setValue("value", value);
        return holder.value<glm::dvec4>("value");
    }
    else if constexpr (std::is_same_v<T, glm::uvec2>) {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary value = luaDictionaryFromState(L);
        Dictionary holder;
        holder.setValue("value", value);
        return holder.value<glm::dvec2>("value");
    }
    else if constexpr (std::is_same_v<T, glm::uvec3>) {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary value = luaDictionaryFromState(L);
        Dictionary holder;
        holder.setValue("value", value);
        return holder.value<glm::dvec3>("value");
    }
    else if constexpr (std::is_same_v<T, glm::uvec4>) {
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary value = luaDictionaryFromState(L);
        Dictionary holder;
        holder.setValue("value", value);
        return holder.value<glm::dvec4>("value");
    }
    else if constexpr (std::is_same_v<T, glm::vec2>) {
        return valueInner<glm::dvec2>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::vec3>) {
        return valueInner<glm::dvec3>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::vec4>) {
        return valueInner<glm::dvec4>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat2x2>) {
        return valueInner<glm::dmat2x2>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat2x3>) {
        return valueInner<glm::dmat2x3>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat2x4>) {
        return valueInner<glm::dmat2x4>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat3x2>) {
        return valueInner<glm::dmat3x2>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat3x3>) {
        return valueInner<glm::dmat3x3>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat3x4>) {
        return valueInner<glm::dmat3x4>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat4x2>) {
        return valueInner<glm::dmat4x2>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat4x3>) {
        return valueInner<glm::dmat4x3>(L, location);
    }
    else if constexpr (std::is_same_v<T, glm::mat4x4>) {
        return valueInner<glm::dmat4x4>(L, location);
    }
    else if constexpr (is_variant<T>::value) {
        return variantValue<0, T>(L, location);
    }
    else if constexpr (is_string_map<T>::value) {
        using value_type = typename T::mapped_type;
        if constexpr (std::is_same_v<value_type, float>) {
            auto res = valueInner<std::map<std::string, double>>(L, location);
            std::map<std::string, float> res2;
            for (const std::pair<const std::string, double>& p : res) {
                res2[p.first] = static_cast<float>(p.second);
            }
            return res2;
        }
        else {
            // First get the state as a Dictionary which is then converted into the map
            lua_pushvalue(L, location);
            defer { lua_pop(L, 1); };

            Dictionary d = luaDictionaryFromState(L);
            std::vector<std::string_view> keys = d.keys();
            T res;
            for (std::string_view k : keys) {
                if constexpr (std::is_same_v<
                                typename T::mapped_type, std::filesystem::path
                              >)
                {
                    typename T::mapped_type value = d.value<std::string>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::vec2>) {
                    typename T::mapped_type value = d.value<glm::dvec2>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::vec3>) {
                    typename T::mapped_type value = d.value<glm::dvec3>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::vec4>) {
                    typename T::mapped_type value = d.value<glm::dvec4>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::ivec2>) {
                    typename T::mapped_type value = d.value<glm::dvec2>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::ivec3>) {
                    typename T::mapped_type value = d.value<glm::dvec3>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::ivec4>) {
                    typename T::mapped_type value = d.value<glm::dvec4>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat2x2>) {
                    typename T::mapped_type value = d.value<glm::dmat2x2>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat2x3>) {
                    typename T::mapped_type value = d.value<glm::dmat2x3>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat2x4>) {
                    typename T::mapped_type value = d.value<glm::dmat2x4>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat3x2>) {
                    typename T::mapped_type value = d.value<glm::dmat3x2>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat3x3>) {
                    typename T::mapped_type value = d.value<glm::dmat3x3>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat3x4>) {
                    typename T::mapped_type value = d.value<glm::dmat3x4>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat4x2>) {
                    typename T::mapped_type value = d.value<glm::dmat4x2>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat4x3>) {
                    typename T::mapped_type value = d.value<glm::dmat4x3>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, glm::mat4x4>) {
                    typename T::mapped_type value = d.value<glm::dmat4x4>(k);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (std::is_same_v<typename T::mapped_type, int>) {
                    typename T::mapped_type value = static_cast<int>(d.value<double>(k));
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (is_vector<typename T::mapped_type>::value) {
                    // A vector is going to be stored as a Dictionary
                    Dictionary dictVal = d.value<Dictionary>(k);
                    typename T::mapped_type value =
                        dictionaryToVector<typename T::mapped_type::value_type>(dictVal);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (is_array<typename T::mapped_type>::value) {
                    // A vector is going to be stored as a Dictionary
                    Dictionary dictVal = d.value<Dictionary>(k);
                    typename T::mapped_type value =
                        dictionaryToVector<typename T::mapped_type::value_type>(dictVal);
                    res[std::string(k)] = std::move(value);
                }
                else if constexpr (is_tuple<typename T::mapped_type>::value) {
                    Dictionary dictVal = d.value<Dictionary>(k);
                    extractValues(dictVal, res[std::string(k)]);
                }
                else if constexpr (std::is_pointer_v<typename T::mapped_type>) {
                    void* val = d.value<void*>(k);
                    res[std::string(k)] = reinterpret_cast<typename T::mapped_type>(val);
                }
                else {
                    typename T::mapped_type value = d.value<typename T::mapped_type>(k);
                    res[std::string(k)] = std::move(value);
                }
            }
            return res;
        }
    }
    else if constexpr (is_vector<T>::value) {
        // First get the state as a Dictionary which is then converted into the vector
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary d = luaDictionaryFromState(L);
        T res = dictionaryToVector<typename T::value_type, true>(d);
        return res;
    }
    else if constexpr (is_array<T>::value) {
        // First get the state as a Dictionary which is then converted into the vector
        lua_pushvalue(L, location);
        defer { lua_pop(L, 1); };

        Dictionary d = luaDictionaryFromState(L);
        std::vector<typename T::value_type> r =
            dictionaryToVector<typename T::value_type, true>(d);

        T res;

        if (r.size() != res.size()) {
            // The list of values that was passed in was of different length then what was
            // requested out of it
            std::string name = Name<T>();
            std::string error = std::format(
                "Expected '{}' values for '{}' but got '{}' instead",
                res.size(), name, r.size()
            );
            // This function won't actually return, but C++ doesn't know that, so
            luaError(L, error.c_str());
            throw LuaFormatException(std::move(error));
        }


        std::copy_n(r.begin(), r.size(), res.begin());
        return res;
    }
    else if constexpr (is_tuple<T>::value) {
        // The values for the tuple are currently inside of a table, but we need to lift
        // them to the top level for the `extractValues` function to work. For now, we'll
        // use a new state for this to simplify the code. This could be made more
        // efficient if we use the same stack but copy values around in a smart way
        lua_State* newL = luaL_newstate();

        lua_pushnil(L);
        while (lua_next(L, -2) != 0) {
            lua_xmove(L, newL, 1);
        }

        int n = lua_gettop(newL);
        T result;
        int argumentsFound = 0;
        // Location is 1 since we copied over the values into a new Lua state
        internal::extractValues(newL, result, 1, n, argumentsFound);
        lua_close(newL);
        return result;
    }
    else {
        static_assert(sizeof(T) == 0, "Unhandled type T");
    }
}

} // namespace internal

template <typename T>
bool hasValue(lua_State* L, int location) {
    if constexpr (std::is_same_v<T, bool>) {
        return lua_isboolean(L, location);
    }
    else if constexpr (std::is_same_v<T, lua_Number>) {
        return lua_isnumber(L, location) == 1;
    }
    else if constexpr (std::is_same_v<T, lua_Integer>) {
        return lua_isinteger(L, location) == 1;
    }
    else if constexpr (std::is_integral_v<T>) {
        return lua_isinteger(L, location) == 1;
    }
    else if constexpr (std::is_floating_point_v<T>) {
        return lua_isnumber(L, location) == 1;
    }
    else if constexpr (std::is_same_v<T, const char*> || std::is_same_v<T, std::string> ||
                       std::is_same_v<T, std::filesystem::path> ||
                       std::is_same_v<T, std::string_view>)
    {
        return lua_type(L, location) == LUA_TSTRING;
    }
    else if constexpr (std::is_pointer_v<T>) {
        return lua_isuserdata(L, location) == 1;
    }
    else if constexpr (std::is_same_v<T, Dictionary> ||
                       internal::is_string_map<T>::value ||
                       internal::is_vector<T>::value ||
                       internal::is_array<T>::value || internal::is_tuple<T>::value ||
                       std::is_same_v<T, glm::ivec2> || std::is_same_v<T, glm::ivec3> ||
                       std::is_same_v<T, glm::ivec4> || std::is_same_v<T, glm::uvec2> ||
                       std::is_same_v<T, glm::uvec3> || std::is_same_v<T, glm::uvec4> ||
                       std::is_same_v<T, glm::dvec2> || std::is_same_v<T, glm::dvec3> ||
                       std::is_same_v<T, glm::dvec4> || std::is_same_v<T, glm::vec2>  ||
                       std::is_same_v<T, glm::vec3>  || std::is_same_v<T, glm::vec4>  ||
                       std::is_same_v<T, glm::dmat2x2> ||
                       std::is_same_v<T, glm::dmat2x3> ||
                       std::is_same_v<T, glm::dmat2x4> ||
                       std::is_same_v<T, glm::dmat3x2> ||
                       std::is_same_v<T, glm::dmat3x3> ||
                       std::is_same_v<T, glm::dmat3x4> ||
                       std::is_same_v<T, glm::dmat4x2> ||
                       std::is_same_v<T, glm::dmat4x3> ||
                       std::is_same_v<T, glm::dmat4x4> ||
                       std::is_same_v<T, glm::mat2x2> || std::is_same_v<T, glm::mat2x3> ||
                       std::is_same_v<T, glm::mat2x4> || std::is_same_v<T, glm::mat3x2> ||
                       std::is_same_v<T, glm::mat3x3> || std::is_same_v<T, glm::mat3x4> ||
                       std::is_same_v<T, glm::mat4x2> || std::is_same_v<T, glm::mat4x3> ||
                       std::is_same_v<T, glm::mat4x4>)
    {
        return lua_istable(L, location) == 1;
    }
    else if constexpr (internal::is_variant<T>::value) {
        return internal::hasVariantValue<0, T>(L, location);
    }
    else if constexpr (internal::is_optional<T>::value) {
        const int n = lua_gettop(L);
        if (n < location || n == 0) {
            // We tried to access an optional value for which no parameter was provided
            return true;
        }
        else {
            return hasValue<typename T::value_type>(L, location);
        }
    }
    else {
        static_assert(sizeof(T) == 0, "Unhandled type T");
    }
}

template <typename T>
T value(lua_State* L, int location, PopValue shouldPopValue) {
    ghoul_precondition(L != nullptr, "L must not be nullptr");

    if constexpr (internal::is_optional<T>::value) {
        const int n = lua_gettop(L);
        if (n < location || n == 0) {
            // We tried to access an optional value for which no parameter was provided
            return std::nullopt;
        }
        else {
            T res = internal::valueInner<typename T::value_type>(L, location);
            if (shouldPopValue) {
                lua_remove(L, location);
            }
            return res;
        }
    }
    else {
        T res = internal::valueInner<T>(L, location);
        if (shouldPopValue) {
            lua_remove(L, location);
        }
        return res;
    }
}

template <typename... Ts>
constexpr std::tuple<Ts...> values(lua_State* L, int location, PopValue shouldPopValue) {
    ghoul_precondition(L != nullptr, "L must not be nullptr");

    // Verify that we don't have a situation where we have non-optional parameters, then
    // optional parameters, and then non-optional parameters again as that will bork up
    // the argument calculations
    static_assert(
        internal::verifyParameters<0, Ts...>(),
        "Tuple parameters have to list all non-optional parameters first, then the "
        "optional ones and cannot get back to non-optional parameters"
    );

    int n = lua_gettop(L);
    std::tuple<Ts...> result;
    int argumentsFound = 0;
    internal::extractValues(L, result, location, n, argumentsFound);

    if (shouldPopValue) {
        for (int i = 0; i < argumentsFound; i++) {
            lua_remove(L, location);
        }
    }
    return result;
}

template <typename T>
T* userData(lua_State* L, int location) {
    ghoul_precondition(L != nullptr, "L must not be nullptr");
    return reinterpret_cast<T*>(lua_touserdata(L, lua_upvalueindex(location)));
}

template <typename... Ts>
void push(lua_State* L, Ts... arguments) {
    ghoul_precondition(L != nullptr, "L must not be nullptr");

    (internal::push(L, arguments), ...);
}

} // namespace ghoul::lua
