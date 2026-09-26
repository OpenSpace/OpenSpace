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

#include <ghoul/misc/dictionary.h>
#include <ghoul/glm.h>

TEST_CASE("Dictionary: bool", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", true);
    d.setValue("b", false);

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<bool>("a"));
    CHECK(d.value<bool>("a") == true);

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<bool>("b"));
    CHECK(d.value<bool>("b") == false);
}

TEST_CASE("Dictionary: int", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", 1);
    d.setValue("b", 2);
    d.setValue("c", 3);

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<int>("a"));
    CHECK(d.value<int>("a") == 1);

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<int>("b"));
    CHECK(d.value<int>("b") == 2);

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<int>("c"));
    CHECK(d.value<int>("c") == 3);
}

TEST_CASE("Dictionary: double", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", 1.1);
    d.setValue("b", 2.2);
    d.setValue("c", 3.3);

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<double>("a"));
    CHECK(d.value<double>("a") == 1.1);

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<double>("b"));
    CHECK(d.value<double>("b") == 2.2);

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<double>("c"));
    CHECK(d.value<double>("c") == 3.3);
}

TEST_CASE("Dictionary: std::string", "[dictionary]") {
    using namespace std::string_literals;
    using namespace std::string_view_literals;

    ghoul::Dictionary d;
    d.setValue("a", "abc"s);
    d.setValue("b", "def"s);
    d.setValue("c", "ghi"s);

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<std::string>("a"));
    CHECK(d.value<std::string>("a") == "abc"sv);

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<std::string>("b"));
    CHECK(d.value<std::string>("b") == "def"sv);

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<std::string>("c"));
    CHECK(d.value<std::string>("c") == "ghi"sv);
}

TEST_CASE("Dictionary: Dictionary", "[dictionary]") {
    using namespace std::string_literals;
    using namespace std::string_view_literals;

    ghoul::Dictionary d;
    {
        ghoul::Dictionary e;
        e.setValue("foo", 1);
        e.setValue("bar", 2.2);
        e.setValue("foobar", "abc"s);
        d.setValue("a", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("foo", 3);
        e.setValue("bar", 4.4);
        e.setValue("foobar", "def"s);
        d.setValue("b", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("foo", 5);
        e.setValue("bar", 6.6);
        e.setValue("foobar", "ghi"s);
        d.setValue("c", e);
    }

    {
        REQUIRE(d.hasKey("a"));
        REQUIRE(d.hasValue<ghoul::Dictionary>("a"));
        const ghoul::Dictionary e = d.value<ghoul::Dictionary>("a");
        REQUIRE(e.hasKey("foo"));
        REQUIRE(e.hasValue<int>("foo"));
        CHECK(e.value<int>("foo") == 1);
        REQUIRE(e.hasKey("bar"));
        REQUIRE(e.hasValue<double>("bar"));
        CHECK(e.value<double>("bar") == 2.2);
        REQUIRE(e.hasKey("foobar"));
        REQUIRE(e.hasValue<std::string>("foobar"));
        CHECK(e.value<std::string>("foobar") == "abc"sv);
    }
    {
        REQUIRE(d.hasKey("b"));
        REQUIRE(d.hasValue<ghoul::Dictionary>("b"));
        const ghoul::Dictionary e = d.value<ghoul::Dictionary>("b");
        REQUIRE(e.hasKey("foo"));
        REQUIRE(e.hasValue<int>("foo"));
        CHECK(e.value<int>("foo") == 3);
        REQUIRE(e.hasKey("bar"));
        REQUIRE(e.hasValue<double>("bar"));
        CHECK(e.value<double>("bar") == 4.4);
        REQUIRE(e.hasKey("foobar"));
        REQUIRE(e.hasValue<std::string>("foobar"));
        CHECK(e.value<std::string>("foobar") == "def"sv);
    }
    {
        REQUIRE(d.hasKey("c"));
        REQUIRE(d.hasValue<ghoul::Dictionary>("c"));
        const ghoul::Dictionary e = d.value<ghoul::Dictionary>("c");
        REQUIRE(e.hasKey("foo"));
        REQUIRE(e.hasValue<int>("foo"));
        CHECK(e.value<int>("foo") == 5);
        REQUIRE(e.hasKey("bar"));
        REQUIRE(e.hasValue<double>("bar"));
        CHECK(e.value<double>("bar") == 6.6);
        REQUIRE(e.hasKey("foobar"));
        REQUIRE(e.hasValue<std::string>("foobar"));
        CHECK(e.value<std::string>("foobar") == "ghi"sv);
    }
}

TEST_CASE("Dictionary: std::vector<int>", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", std::vector<int>{ 1, 2, 3 });
    d.setValue("b", std::vector<int>{ 4, 5, 6 });
    d.setValue("c", std::vector<int>{ 7, 8, 9 });

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<std::vector<int>>("a"));
    CHECK(d.value<std::vector<int>>("a") == std::vector<int>{ 1, 2, 3 });

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<std::vector<int>>("b"));
    CHECK(d.value<std::vector<int>>("b") == std::vector<int>{ 4, 5, 6 });

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<std::vector<int>>("c"));
    CHECK(d.value<std::vector<int>>("c") == std::vector<int>{ 7, 8, 9 });
}

TEST_CASE("Dictionary: std::vector<double>", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", std::vector<double>{ 1.1, 2.2, 3.3 });
    d.setValue("b", std::vector<double>{ 4.4, 5.5, 6.6 });
    d.setValue("c", std::vector<double>{ 7.7, 8.8, 9.9 });

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<std::vector<double>>("a"));
    CHECK(d.value<std::vector<double>>("a") == std::vector<double>{ 1.1, 2.2, 3.3 });

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<std::vector<double>>("b"));
    CHECK(d.value<std::vector<double>>("b") == std::vector<double>{ 4.4, 5.5, 6.6 });

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<std::vector<double>>("c"));
    CHECK(d.value<std::vector<double>>("c") == std::vector<double>{ 7.7, 8.8, 9.9 });
}

TEST_CASE("Dictionary: std::vector<std::string>", "[dictionary]") {
    using namespace std::string_literals;

    ghoul::Dictionary d;
    d.setValue("a", std::vector<std::string>{ "abc"s, "def"s, "ghi"s });
    d.setValue("b", std::vector<std::string>{ "jkl"s, "mno"s, "pqr"s });
    d.setValue("c", std::vector<std::string>{ "stu"s, "vwx"s, "yzz" });

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<std::vector<std::string>>("a"));
    CHECK(
        d.value<std::vector<std::string>>("a") ==
        std::vector<std::string>{"abc"s, "def"s, "ghi"s }
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<std::vector<std::string>>("b"));
    CHECK(
        d.value<std::vector<std::string>>("b") ==
        std::vector<std::string>{ "jkl"s, "mno"s, "pqr"s }
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<std::vector<std::string>>("c"));
    CHECK(
        d.value<std::vector<std::string>>("c") ==
        std::vector<std::string>{ "stu"s, "vwx"s, "yzz" }
    );
}

TEST_CASE("Dictionary: glm::ivec2", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::ivec2(1, 2));
    d.setValue("b", glm::ivec2(3, 4));
    d.setValue("c", glm::ivec2(5, 6));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::ivec2>("a"));
    CHECK(d.value<glm::ivec2>("a") == glm::ivec2(1, 2));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::ivec2>("b"));
    CHECK(d.value<glm::ivec2>("b") == glm::ivec2(3, 4));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::ivec2>("c"));
    CHECK(d.value<glm::ivec2>("c") == glm::ivec2(5, 6));
}

TEST_CASE("Dictionary: glm::ivec3", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::ivec3(1, 2, 3));
    d.setValue("b", glm::ivec3(4, 5, 6));
    d.setValue("c", glm::ivec3(7, 8, 9));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::ivec3>("a"));
    CHECK(d.value<glm::ivec3>("a") == glm::ivec3(1, 2, 3));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::ivec3>("b"));
    CHECK(d.value<glm::ivec3>("b") == glm::ivec3(4, 5, 6));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::ivec3>("c"));
    CHECK(d.value<glm::ivec3>("c") == glm::ivec3(7, 8, 9));
}

TEST_CASE("Dictionary: glm::ivec4", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::ivec4(1, 2, 3, 4));
    d.setValue("b", glm::ivec4(5, 6, 7, 8));
    d.setValue("c", glm::ivec4(9, 10, 11, 12));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::ivec4>("a"));
    CHECK(d.value<glm::ivec4>("a") == glm::ivec4(1, 2, 3, 4));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::ivec4>("b"));
    CHECK(d.value<glm::ivec4>("b") == glm::ivec4(5, 6, 7, 8));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::ivec4>("c"));
    CHECK(d.value<glm::ivec4>("c") == glm::ivec4(9, 10, 11, 12));
}

TEST_CASE("Dictionary: glm::dvec2", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dvec2(1.1, 2.2));
    d.setValue("b", glm::dvec2(3.3, 4.4));
    d.setValue("c", glm::dvec2(5.5, 6.6));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dvec2>("a"));
    CHECK(d.value<glm::dvec2>("a") == glm::dvec2(1.1, 2.2));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dvec2>("b"));
    CHECK(d.value<glm::dvec2>("b") == glm::dvec2(3.3, 4.4));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dvec2>("c"));
    CHECK(d.value<glm::dvec2>("c") == glm::dvec2(5.5, 6.6));
}

TEST_CASE("Dictionary: glm::dvec3", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dvec3(1.1, 2.2, 3.3));
    d.setValue("b", glm::dvec3(4.4, 5.5, 6.6));
    d.setValue("c", glm::dvec3(7.7, 8.8, 9.9));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dvec3>("a"));
    CHECK(d.value<glm::dvec3>("a") == glm::dvec3(1.1, 2.2, 3.3));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dvec3>("b"));
    CHECK(d.value<glm::dvec3>("b") == glm::dvec3(4.4, 5.5, 6.6));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dvec3>("c"));
    CHECK(d.value<glm::dvec3>("c") == glm::dvec3(7.7, 8.8, 9.9));
}

TEST_CASE("Dictionary: glm::dvec4", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dvec4(1.1, 2.2, 3.3, 4.4));
    d.setValue("b", glm::dvec4(5.5, 6.6, 7.7, 8.8));
    d.setValue("c", glm::dvec4(9.9, 10.10, 11.11, 12.12));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dvec4>("a"));
    CHECK(d.value<glm::dvec4>("a") == glm::dvec4(1.1, 2.2, 3.3, 4.4));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dvec4>("b"));
    CHECK(d.value<glm::dvec4>("b") == glm::dvec4(5.5, 6.6, 7.7, 8.8));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dvec4>("c"));
    CHECK(d.value<glm::dvec4>("c") == glm::dvec4(9.9, 10.10, 11.11, 12.12));
}

TEST_CASE("Dictionary: glm::dmat2x2", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    d.setValue("b", glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    d.setValue("c", glm::dmat2x2(9.9, 10.10, 11.11, 12.12));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat2x2>("a"));
    CHECK(d.value<glm::dmat2x2>("a") == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat2x2>("b"));
    CHECK(d.value<glm::dmat2x2>("b") == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat2x2>("c"));
    CHECK(d.value<glm::dmat2x2>("c") == glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
}

TEST_CASE("Dictionary: glm::dmat2x3", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    d.setValue("b", glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    d.setValue("c", glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat2x3>("a"));
    CHECK(d.value<glm::dmat2x3>("a") == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat2x3>("b"));
    CHECK(d.value<glm::dmat2x3>("b") == glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat2x3>("c"));
    CHECK(
        d.value<glm::dmat2x3>("c") ==
        glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );
}

TEST_CASE("Dictionary: glm::dmat2x4", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dmat2x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    d.setValue(
        "b",
        glm::dmat2x4(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    d.setValue(
        "c",
        glm::dmat2x4(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat2x4>("a"));
    CHECK(
        d.value<glm::dmat2x4>("a") ==
        glm::dmat2x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat2x4>("b"));
    CHECK(
        d.value<glm::dmat2x4>("b") ==
        glm::dmat2x4(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat2x4>("c"));
    CHECK(
        d.value<glm::dmat2x4>("c") ==
        glm::dmat2x4(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
}

TEST_CASE("Dictionary: glm::dmat3x2", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dmat3x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    d.setValue("b", glm::dmat3x2(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    d.setValue("c", glm::dmat3x2(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat3x2>("a"));
    CHECK(
        d.value<glm::dmat3x2>("a") == glm::dmat3x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat3x2>("b"));
    CHECK(
        d.value<glm::dmat3x2>("b") == glm::dmat3x2(7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat3x2>("c"));
    CHECK(
        d.value<glm::dmat3x2>("c") ==
        glm::dmat3x2(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );
}

TEST_CASE("Dictionary: glm::dmat3x3", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dmat3x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9));
    d.setValue(
        "b",
        glm::dmat3x3(10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );
    d.setValue(
        "c",
        glm::dmat3x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24, 25.25, 26.26, 27.27)
    );

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat3x3>("a"));
    CHECK(
        d.value<glm::dmat3x3>("a") ==
        glm::dmat3x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9)
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat3x3>("b"));
    CHECK(
        d.value<glm::dmat3x3>("b") ==
        glm::dmat3x3(10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat3x3>("c"));
    CHECK(
        d.value<glm::dmat3x3>("c") ==
        glm::dmat3x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24, 25.25, 26.26, 27.27)
    );
}

TEST_CASE("Dictionary: glm::dmat3x4", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue(
        "a",
        glm::dmat3x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
    );
    d.setValue(
        "b",
        glm::dmat3x4(
            13.13, 14.14, 15.15,
            16.16, 17.17, 18.18,
            19.19, 20.20, 21.21,
            22.22, 23.23, 24.24
        )
    );
    d.setValue(
        "c",
        glm::dmat3x4(
            25.25, 26.26, 27.27,
            28.28, 29.29, 30.30,
            31.31, 32.32, 33.33,
            34.34, 35.35, 36.36
        )
    );

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat3x4>("a"));
    CHECK(
        d.value<glm::dmat3x4>("a") ==
        glm::dmat3x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat3x4>("b"));
    CHECK(
        d.value<glm::dmat3x4>("b") ==
        glm::dmat3x4(
            13.13, 14.14, 15.15,
            16.16, 17.17, 18.18,
            19.19, 20.20, 21.21,
            22.22, 23.23, 24.24
        )
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat3x4>("c"));
    CHECK(
        d.value<glm::dmat3x4>("c") ==
        glm::dmat3x4(
            25.25, 26.26, 27.27,
            28.28, 29.29, 30.30,
            31.31, 32.32, 33.33,
            34.34, 35.35, 36.36
        )
    );
}

TEST_CASE("Dictionary: glm::dmat4x2", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue("a", glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    d.setValue(
        "b",
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    d.setValue(
        "c",
        glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat4x2>("a"));
    CHECK(
        d.value<glm::dmat4x2>("a") == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat4x2>("b"));
    CHECK(
        d.value<glm::dmat4x2>("b") ==
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat4x2>("c"));
    CHECK(
        d.value<glm::dmat4x2>("c") ==
        glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
}

TEST_CASE("Dictionary: glm::dmat4x3", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue(
        "a",
        glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
    );
    d.setValue(
        "b",
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16,
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24
        )
    );
    d.setValue(
        "c",
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32,
            33.33, 34.34, 35.35, 36.36
        )
    );

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat4x3>("a"));
    CHECK(
        d.value<glm::dmat4x3>("a") ==
        glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat4x3>("b"));
    CHECK(
        d.value<glm::dmat4x3>("b") ==
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16,
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24
        )
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat4x3>("c"));
    CHECK(
        d.value<glm::dmat4x3>("c") ==
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32,
            33.33, 34.34, 35.35, 36.36
        )
    );
}

TEST_CASE("Dictionary: glm::dmat4x4", "[dictionary]") {
    ghoul::Dictionary d;
    d.setValue(
        "a",
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        )
    );
    d.setValue(
        "b",
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32
        )
    );
    d.setValue(
        "c",
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36,
            37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44,
            45.45, 46.46, 47.47, 48.48
        )
    );

    REQUIRE(d.hasKey("a"));
    REQUIRE(d.hasValue<glm::dmat4x4>("a"));
    CHECK(
        d.value<glm::dmat4x4>("a") ==
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        )
    );

    REQUIRE(d.hasKey("b"));
    REQUIRE(d.hasValue<glm::dmat4x4>("b"));
    CHECK(
        d.value<glm::dmat4x4>("b") ==
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32
        )
    );

    REQUIRE(d.hasKey("c"));
    REQUIRE(d.hasValue<glm::dmat4x4>("c"));
    CHECK(
        d.value<glm::dmat4x4>("c") ==
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36,
            37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44,
            45.45, 46.46, 47.47, 48.48
        )
    );
}
