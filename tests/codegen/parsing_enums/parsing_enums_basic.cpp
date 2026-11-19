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

#include <catch2/catch_test_macros.hpp>

#include "codegen.h"
#include "parsing.h"
#include "types.h"

TEST_CASE("Parsing/Enums/Basic:  Basic setup", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class [[codegen::stringify()]] Enum {
        Value1,
        value2,
        Value3
    };
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
    CHECK(e->attributes.stringify);
    CHECK(!e->attributes.arrayify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Stringify/1", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class [[codegen::stringify()]] Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
    CHECK(e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Stringify/2", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class
    [[codegen::stringify()]] Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
    CHECK(e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Stringify/3", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class
    [[codegen::stringify()]]
    Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
    CHECK(e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Stringify/4", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class
    [[codegen::stringify()]] Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
    CHECK(e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Stringify/5", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class
    [[codegen::stringify()]]
    Enum
    {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
    CHECK(e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Map/1", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class E {
        Value1,
        Value2,
        Value3
    };

    enum class [[codegen::map(E)]] Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo == "E");
    CHECK(!e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Map/2", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class E {
        Value1,
        Value2,
        Value3
    };

    enum class
    [[codegen::map(E)]] Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo == "E");
    CHECK(!e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Map/3", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class E {
        Value1,
        Value2,
        Value3
    };

    enum class
    [[codegen::map(E)]]
    Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo == "E");
    CHECK(!e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Map/4", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class E {
        Value1,
        Value2,
        Value3
    };

    enum class
    [[codegen::map(E)]] Enum {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo == "E");
    CHECK(!e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Map/5", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    enum class E {
        Value1,
        Value2,
        Value3
    };

    enum class
    [[codegen::map(E)]]
    Enum
    {
        Value1,
        Value2,
        Value3
    };
    )";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo == "E");
    CHECK(!e->attributes.stringify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Enums/Basic:  Line breaks/Map/6", "[Parsing][Enums]") {
    constexpr std::string_view Source = R"(
    namespace abc::def::ghi {
        enum class E {
            Value5,
            Value4,
            Value3,
            Value2,
            Value1,
            Value0
        };
    } // abc::def::ghi

    enum class [[codegen::map(abc::def::ghi::E)]]
    Enum
    {
        Value1 = 0,
        Value2,
        Value3,
        Value4
    };
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.luaWrapperFunctions.empty());
    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);

    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo == "abc::def::ghi::E");
    CHECK(!e->attributes.stringify);
    REQUIRE(e->elements.size() == 4);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Value1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Value2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Value3");
    }
    {
        EnumElement* ee = e->elements[3];
        REQUIRE(ee);
        CHECK(ee->name == "Value4");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
