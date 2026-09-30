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

TEST_CASE("Parsing/MultipleDictionaries", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(StructA)]] StructA {
        int value;
        double value2;
    };

    struct [[codegen::Dictionary(StructB)]] StructB {
        std::string value;
        glm::vec2 value2;
    };
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 2);

    {
        Struct* s = code.structs[0];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 2);
        {
            Variable* var = s->variables[0];
            REQUIRE(var);
            CHECK(var->name == "value");
            CHECK(var->key == "\"Value\"");
            CHECK(generateTypename(var->type) == "int");

            CHECK(var->attributes.annotation.empty());
            CHECK(var->attributes.greater.empty());
            CHECK(var->attributes.greaterequal.empty());
            CHECK(var->attributes.inlist.empty());
            CHECK(var->attributes.inrange.empty());
            CHECK(var->attributes.key.empty());
            CHECK(var->attributes.less.empty());
            CHECK(var->attributes.lessequal.empty());
            CHECK(var->attributes.notinrange.empty());
            CHECK(var->attributes.reference.empty());
            CHECK(var->attributes.unequal.empty());
            CHECK(!var->attributes.isColor);
            CHECK(!var->attributes.isDirectory);
            CHECK(!var->attributes.isDateTime);
            CHECK(!var->attributes.isIdentifier);
            CHECK(!var->attributes.mustBeNotEmpty);
            CHECK(!var->attributes.isPrivate);
        }
        {
            Variable* var = s->variables[1];
            REQUIRE(var);
            CHECK(var->name == "value2");
            CHECK(var->key == "\"Value2\"");
            CHECK(generateTypename(var->type) == "double");

            CHECK(var->attributes.annotation.empty());
            CHECK(var->attributes.greater.empty());
            CHECK(var->attributes.greaterequal.empty());
            CHECK(var->attributes.inlist.empty());
            CHECK(var->attributes.inrange.empty());
            CHECK(var->attributes.key.empty());
            CHECK(var->attributes.less.empty());
            CHECK(var->attributes.lessequal.empty());
            CHECK(var->attributes.notinrange.empty());
            CHECK(var->attributes.reference.empty());
            CHECK(var->attributes.unequal.empty());
            CHECK(!var->attributes.isColor);
            CHECK(!var->attributes.isDirectory);
            CHECK(!var->attributes.isDateTime);
            CHECK(!var->attributes.isIdentifier);
            CHECK(!var->attributes.mustBeNotEmpty);
            CHECK(!var->attributes.isPrivate);
        }
    }

    {
        Struct* s = code.structs[1];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 2);
        {
            Variable* var = s->variables[0];
            REQUIRE(var);
            CHECK(var->name == "value");
            CHECK(var->key == "\"Value\"");
            CHECK(generateTypename(var->type) == "std::string");

            CHECK(var->attributes.annotation.empty());
            CHECK(var->attributes.greater.empty());
            CHECK(var->attributes.greaterequal.empty());
            CHECK(var->attributes.inlist.empty());
            CHECK(var->attributes.inrange.empty());
            CHECK(var->attributes.key.empty());
            CHECK(var->attributes.less.empty());
            CHECK(var->attributes.lessequal.empty());
            CHECK(var->attributes.notinrange.empty());
            CHECK(var->attributes.reference.empty());
            CHECK(var->attributes.unequal.empty());
            CHECK(!var->attributes.isColor);
            CHECK(!var->attributes.isDirectory);
            CHECK(!var->attributes.isDateTime);
            CHECK(!var->attributes.isIdentifier);
            CHECK(!var->attributes.mustBeNotEmpty);
            CHECK(!var->attributes.isPrivate);
        }
        {
            Variable* var = s->variables[1];
            REQUIRE(var);
            CHECK(var->name == "value2");
            CHECK(var->key == "\"Value2\"");
            CHECK(generateTypename(var->type) == "glm::vec2");

            CHECK(var->attributes.annotation.empty());
            CHECK(var->attributes.greater.empty());
            CHECK(var->attributes.greaterequal.empty());
            CHECK(var->attributes.inlist.empty());
            CHECK(var->attributes.inrange.empty());
            CHECK(var->attributes.key.empty());
            CHECK(var->attributes.less.empty());
            CHECK(var->attributes.lessequal.empty());
            CHECK(var->attributes.notinrange.empty());
            CHECK(var->attributes.reference.empty());
            CHECK(var->attributes.unequal.empty());
            CHECK(!var->attributes.isColor);
            CHECK(!var->attributes.isDirectory);
            CHECK(!var->attributes.isDateTime);
            CHECK(!var->attributes.isIdentifier);
            CHECK(!var->attributes.mustBeNotEmpty);
            CHECK(!var->attributes.isPrivate);
        }
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}


//TEST_CASE("Parsing/Mixed/Basic:  Mixing structs and enum", "[Parsing][Misc]") {
//    constexpr std::string_view Source = R"(
//    struct [[codegen::Dictionary(MixedStruct)]] Struct {
//        int value;
//        double value2;
//};
//
//    enum class [[codegen::stringify()]] Enum {
//        Value1,
//        value2,
//        Value3
//    };
//)";
//
//    Code code = parse(Source);
//    REQUIRE(code.structs.size() == 1);
//    Struct* s = code.structs.front();
//    REQUIRE(s);
//    REQUIRE(s->variables.size() == 2);
//    {
//        Variable* var = s->variables[0];
//        REQUIRE(var);
//        CHECK(var->name == "value");
//        CHECK(var->key == "\"Value\"");
//        CHECK(generateTypename(var->type) == "int");
//
//        CHECK(var->attributes.annotation.empty());
//        CHECK(var->attributes.greater.empty());
//        CHECK(var->attributes.greaterequal.empty());
//        CHECK(var->attributes.inlist.empty());
//        CHECK(var->attributes.inrange.empty());
//        CHECK(var->attributes.key.empty());
//        CHECK(var->attributes.less.empty());
//        CHECK(var->attributes.lessequal.empty());
//        CHECK(var->attributes.notinrange.empty());
//        CHECK(var->attributes.reference.empty());
//        CHECK(var->attributes.unequal.empty());
//    }
//    {
//        Variable* var = s->variables[1];
//        REQUIRE(var);
//        CHECK(var->name == "value2");
//        CHECK(var->key == "\"Value2\"");
//        CHECK(generateTypename(var->type) == "double");
//
//        CHECK(var->attributes.annotation.empty());
//        CHECK(var->attributes.greater.empty());
//        CHECK(var->attributes.greaterequal.empty());
//        CHECK(var->attributes.inlist.empty());
//        CHECK(var->attributes.inrange.empty());
//        CHECK(var->attributes.key.empty());
//        CHECK(var->attributes.less.empty());
//        CHECK(var->attributes.lessequal.empty());
//        CHECK(var->attributes.notinrange.empty());
//        CHECK(var->attributes.reference.empty());
//        CHECK(var->attributes.unequal.empty());
//    }
//
//    REQUIRE(code.enums.size() == 1);
//    Enum* e = code.enums.front();
//    REQUIRE(e);
//    CHECK(e->parent == nullptr);
//    CHECK(e->mappedTo.empty());
//    REQUIRE(e->elements.size() == 3);
//    {
//        EnumElement* ee = e->elements[0];
//        REQUIRE(ee);
//        CHECK(ee->name == "Value1");
//    }
//    {
//        EnumElement* ee = e->elements[1];
//        REQUIRE(ee);
//        CHECK(ee->name == "value2");
//    }
//    {
//        EnumElement* ee = e->elements[2];
//        REQUIRE(ee);
//        CHECK(ee->name == "Value3");
//    }
//
//    std::string r = generateResult(code);
//    CHECK(!r.empty());
//}

