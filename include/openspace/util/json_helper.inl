/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <type_traits>

namespace openspace {

namespace internal {

template <class T, class... Ts>
struct is_any : std::disjunction<std::is_same<T, Ts>...> {};

template <typename T>
constexpr bool isGlmMatrix() {
    return is_any<T,
        glm::mat2x2, glm::mat2x3, glm::mat2x4,
        glm::mat3x2, glm::mat3x3, glm::mat3x4,
        glm::mat4x2, glm::mat4x3, glm::mat4x4,
        glm::dmat2x2, glm::dmat2x3, glm::dmat2x4,
        glm::dmat3x2, glm::dmat3x3, glm::dmat3x4,
        glm::dmat4x2, glm::dmat4x3, glm::dmat4x4>::value;
}

template <typename T>
constexpr bool isGlmVector() {
    return is_any<T,
        glm::vec2, glm::vec3, glm::vec4,
        glm::ivec2, glm::ivec3, glm::ivec4,
        glm::dvec2, glm::dvec3, glm::dvec4,
        glm::uvec2, glm::uvec3, glm::uvec4>::value;
}

} // namespace internal

template <typename T>
std::string formatJson(T value) {
    if constexpr (std::is_same_v<T, bool>) {
        return value ? "true" : "false";
    }
    else if constexpr (std::is_arithmetic_v<T>) {
        return formatJsonNumber(static_cast<double>(value));
    }
    else if constexpr (std::is_same_v<T, std::string>) {
        return escapedJson(value);
    }
    else if constexpr (std::is_same_v<T, ghoul::Dictionary>) {
        return ghoul::formatJson(value);
    }
    else if constexpr (internal::isGlmVector<T>()) {
        std::string values;
        for (glm::length_t i = 0; i < ghoul::glm_components<T>::value; i++) {
            values += std::to_string(value[i]) + ',';
        }
        values.pop_back();
        return std::format("[{}]", values);
    }
    else if constexpr (internal::isGlmMatrix<T>()) {
        std::string values;
        for (glm::length_t i = 0; i < T::type::row_type::length(); i++) {
            for (glm::length_t j = 0; j < T::type::col_type::length(); j++) {
                values += std::to_string(value[i][j]) + ',';
            }
        }
        values.pop_back();
        return std::format("[{}]", values);
    }
    else {
        static_assert(sizeof(T) == 0, "JSON formatting of type T not implemented");
    }
}

}  // namespace openspace
