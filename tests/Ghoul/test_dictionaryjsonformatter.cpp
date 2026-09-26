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

#include <catch2/catch_test_macros.hpp>

#include <ghoul/misc/dictionaryjsonformatter.h>
#include <ghoul/misc/dictionary.h>
#include <string>

TEST_CASE("DictionaryJsonFormatter: Empty Dictionary", "[dictionaryjsonformatter]") {
    const ghoul::Dictionary d;
    std::string res = ghoul::formatJson(d);
    CHECK(res == "{}");
}

TEST_CASE("DictionaryJsonFormatter: Simple Dictionary", "[dictionaryjsonformatter]") {
    using namespace std::string_literals;
    ghoul::Dictionary d;
    d.setValue("boolFalse", false);
    d.setValue("boolTrue", true);
    d.setValue("int", 1);
    d.setValue("double", 2.2);
    d.setValue("vec2", glm::dvec2(0.0));
    d.setValue("vec3", glm::dvec3(0.0));
    d.setValue("vec4", glm::dvec4(0.0));
    d.setValue("string", ""s);

    std::string res = ghoul::formatJson(d);
    CHECK(
        res ==
        "{\"boolFalse\":false,\"boolTrue\":true,"
        "\"double\":2.2,\"int\":1,\"string\":\"\","
        "\"vec2\":[0,0],\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]}"
    );
}

TEST_CASE("DictionaryJsonFormatter: Dictionary with Ivec", "[dictionaryjsonformatter]") {
    using namespace std::string_literals;
    ghoul::Dictionary d;
    d.setValue("ivec2", glm::ivec2(0));
    d.setValue("ivec3", glm::ivec3(0));
    d.setValue("ivec4", glm::ivec4(0));

    std::string res = ghoul::formatJson(d);
    CHECK(
        res ==
        "{\"ivec2\":[0,0],\"ivec3\":[0,0,0],"
        "\"ivec4\":[0,0,0,0]}"
    );
}

TEST_CASE("DictionaryJsonFormatter: std::vectors", "[dictionaryjsonformatter]") {
    using namespace std::string_literals;
    ghoul::Dictionary d;
    d.setValue("iVector", std::vector<int>{ 1, 2, 3, 4, 5 });
    d.setValue("dVector", std::vector<double>{ 0.1, 0.2, 0.3, 0.4, 0.5 });
    d.setValue("empty", std::vector<double>{});

    std::string res = ghoul::formatJson(d);
    CHECK(
        res ==
        "{\"dVector\":[0.1,0.2,0.3,0.4,0.5],"
        "\"empty\":[],"
        "\"iVector\":[1,2,3,4,5]}"
    );
}

TEST_CASE("DictionaryJsonFormatter: Matrices", "[dictionaryjsonformatter]") {
    using namespace std::string_literals;
    ghoul::Dictionary d;
    d.setValue("dmat2x2", glm::dmat2(0.0));
    d.setValue("dmat2x3", glm::dmat2x3(0.0));
    d.setValue("dmat2x4", glm::dmat2x4(0.0));

    d.setValue("dmat3x3", glm::dmat3(0.0));
    d.setValue("dmat3x2", glm::dmat3x2(0.0));
    d.setValue("dmat3x4", glm::dmat3x4(0.0));

    d.setValue("dmat4x4", glm::dmat4(0.0));
    d.setValue("dmat4x2", glm::dmat4x2(0.0));
    d.setValue("dmat4x3", glm::dmat4x3(0.0));

    std::string res = ghoul::formatJson(d);
    CHECK(
        res ==
        "{\"dmat2x2\":[0,0,0,0],\"dmat2x3\":[0,0,0,0,0,0],\"dmat2x4\":[0,0,0,0,0,0,0,0],"
        "\"dmat3x2\":[0,0,0,0,0,0],\"dmat3x3\":[0,0,0,0,0,0,0,0,0],"
        "\"dmat3x4\":[0,0,0,0,0,0,0,0,0,0,0,0],\"dmat4x2\":[0,0,0,0,0,0,0,0],"
        "\"dmat4x3\":[0,0,0,0,0,0,0,0,0,0,0,0],"
        "\"dmat4x4\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}"
    );
}

TEST_CASE("DictionaryJsonFormatter: Dictionary with nan", "[dictionaryjsonformatter]") {
    ghoul::Dictionary d;
    d.setValue("nanValue", std::numeric_limits<double>::quiet_NaN());

    std::string res = ghoul::formatJson(d);
    CHECK(res == "{\"nanValue\":null}");
}

TEST_CASE("DictionaryJsonFormatter: Dictionary with infinity",
          "[dictionaryjsonformatter]")
{
    ghoul::Dictionary d;
    d.setValue("infinity", std::numeric_limits<double>::infinity());

    std::string res = ghoul::formatJson(d);
    CHECK(res == "{\"infinity\":null}");
}

TEST_CASE("DictionaryJsonFormatter: Nested Dictionary", "[dictionaryjsonformatter]") {
    using namespace std::string_literals;

    ghoul::Dictionary d;
    d.setValue("int", 1);
    d.setValue("double", 2.2);
    d.setValue("vec2", glm::dvec2(0.0));
    d.setValue("vec3", glm::dvec3(0.0));
    d.setValue("vec4", glm::dvec4(0.0));
    d.setValue("string", ""s);

    ghoul::Dictionary e;
    e.setValue("int", 1);
    e.setValue("double", 2.2);
    e.setValue("vec2", glm::dvec2(0.0));
    e.setValue("vec3", glm::dvec3(0.0));
    e.setValue("vec4", glm::dvec4(0.0));
    e.setValue("string", ""s);
    e.setValue("dict", d);

    ghoul::Dictionary f;
    f.setValue("int", 1);
    f.setValue("double", 2.2);
    f.setValue("vec2", glm::dvec2(0.0));
    f.setValue("vec3", glm::dvec3(0.0));
    f.setValue("vec4", glm::dvec4(0.0));
    f.setValue("string", ""s);
    f.setValue("dict", e);

    ghoul::Dictionary g;
    g.setValue("int", 1);
    g.setValue("double", 2.2);
    g.setValue("vec2", glm::dvec2(0.0));
    g.setValue("vec3", glm::dvec3(0.0));
    g.setValue("vec4", glm::dvec4(0.0));
    g.setValue("string", ""s);
    g.setValue("dict", f);
    g.setValue("dict2", f);
    g.setValue("dict3", f);

    std::string res = ghoul::formatJson(g);

    CHECK(
        res ==
        "{\"dict\":{\"dict\":{\"dict\":{\"double\":2.2,\"int\":1,"
        "\"string\":\"\",\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},"
        "\"double\":2.2,\"int\":1,\"string\":\"\","
        "\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},"
        "\"double\":2.2,\"int\":1,\"string\":\"\","
        "\"vec2\":[0,0],\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},"
        "\"dict2\":{\"dict\":{\"dict\":{\"double\":2.2,\"int\":1,\"string\":\"\","
        "\"vec2\":[0,0],\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},\"double\":2.2,\"int\":1,"
        "\"string\":\"\",\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},\"double\":2.2,\"int\":1,"
        "\"string\":\"\",\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},\"dict3\":{\"dict\":{\"dict\":{"
        "\"double\":2.2,\"int\":1,\"string\":\"\",\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},\"double\":2.2,\"int\":1,"
        "\"string\":\"\",\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},\"double\":2.2,\"int\":1,"
        "\"string\":\"\",\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]},\"double\":2.2,\"int\":1,"
        "\"string\":\"\",\"vec2\":[0,0],"
        "\"vec3\":[0,0,0],"
        "\"vec4\":[0,0,0,0]}"
    );
}
