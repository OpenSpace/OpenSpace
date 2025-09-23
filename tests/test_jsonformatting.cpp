/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>

#include <openspace/json.h>
#include <openspace/util/json_helper.h>
#include <vector>

// Note: Dictionary formatting is tested in Ghoul

TEMPLATE_TEST_CASE("FormatJson", "[formatjson]", glm::vec2, glm::vec3,
    glm::vec4, glm::dvec2, glm::dvec3, glm::dvec4, glm::ivec2, glm::ivec3, glm::ivec4,
    glm::uvec2, glm::uvec3, glm::uvec4, glm::mat2x2, glm::mat2x3, glm::mat2x4,
    glm::mat3x2, glm::mat3x3, glm::mat3x4, glm::mat4x2, glm::mat4x3, glm::mat4x4,
    glm::dmat2x2, glm::dmat2x3, glm::dmat2x4, glm::dmat3x2, glm::dmat3x3, glm::dmat3x4,
    glm::dmat4x2, glm::dmat4x3, glm::dmat4x4)
{
    using T = TestType;

    const T val(1);

    std::string json = openspace::formatJson(val);

    // Compare with Ghoul's Lua conversions. Note that Lua uses '{' for arrays,
    // while we here expect '[' for all glm types
    std::string luaValue = ghoul::to_string(val);
    luaValue.front() = '[';
    luaValue.back() = ']';
    CHECK(json == luaValue);
}

TEST_CASE("FormatJson - Bool", "[formatjson]") {
    constexpr bool TrueVal = true;
    constexpr bool FalseVal = false;

    CHECK(openspace::formatJson(TrueVal) == "true");
    CHECK(openspace::formatJson(FalseVal) == "false");
}

TEST_CASE("FormatJson - Infinity & Nan", "[formatjson]") {
    CHECK(openspace::formatJson(std::numeric_limits<double>::infinity()) == "null");
    CHECK(openspace::formatJson(std::numeric_limits<double>::quiet_NaN()) == "null");
}

// @TODO(emmbr 2021-04-29) Add more tests at some point, if we find it necessary
