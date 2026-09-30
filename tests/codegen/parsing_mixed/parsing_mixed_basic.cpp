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

TEST_CASE("Parsing/Mixed/Basic:  Mixing structs and enum", "[Parsing][Mixed]") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(MixedStruct)]] Struct {
        int value;
        double value2;
};

    enum class [[codegen::stringify()]] Enum {
        Value1,
        value2,
        Value3
    };
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    Struct* s = code.structs.front();
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

    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);
    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
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

TEST_CASE("Parsing/Mixed/Basic:  Mixing enums and structs", "[Parsing][Mixed]") {
    constexpr std::string_view Source = R"(
    enum class [[codegen::stringify()]] Enum {
        Value1,
        value2,
        Value3
    };

    struct [[codegen::Dictionary(MixedStruct)]] Struct {
        int value;
        double value2;
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    Struct* s = code.structs.front();
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

    REQUIRE(code.enums.size() == 1);
    Enum* e = code.enums.front();
    REQUIRE(e);
    CHECK(e->parent == nullptr);
    CHECK(e->attributes.mappedTo.empty());
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

TEST_CASE("Parsing/Mixed/Basic:  Enums in Structs", "[Parsing][Mixed]") {
    constexpr std::string_view Source = R"(
    enum class Enum1 {
        Value1,
        Value2,
        Value3
    };

    enum class [[codegen::stringify()]] Enum2 {
        Value1,
        Value2,
        Value3
    };

    enum class [[codegen::map(Enum1)]] Enum3 {
        Value1,
        Value2,
        Value3
    };

    enum class [[codegen::map(Enum1), codegen::stringify()]] Enum4 {
        Value1,
        Value2,
        Value3
    };

    struct [[codegen::Dictionary(MixedStruct)]] Struct {
        int value;
        double value2;

        // enum5 documentation
        enum class Enum5 {
            Value1,
            Value2,
            Value3
        };
        Enum5 enum5;

        // enum6 documentation
        enum class [[codegen::stringify()]] Enum6 {
            Value1,
            Value2,
            Value3
        };
        Enum6 enum6;

        // enum7 documentation
        enum class [[codegen::map(Enum1)]] Enum7 {
            Value1,
            Value2,
            Value3
        };
        Enum7 enum7;

        // enum8 documentation
        enum class [[codegen::map(Enum1), codegen::stringify()]] Enum8 {
            Value1,
            Value2,
            Value3
        };
        Enum8 enum8;
    };
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    {
        Struct* s = code.structs.front();
        REQUIRE(s);
        REQUIRE(s->variables.size() == 6);
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
        {
            Variable* var = s->variables[2];
            REQUIRE(var);
            CHECK(var->name == "enum5");
            CHECK(var->key == "\"Enum5\"");
            CHECK(generateTypename(var->type) == "Enum5");

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
            Variable* var = s->variables[3];
            REQUIRE(var);
            CHECK(var->name == "enum6");
            CHECK(var->key == "\"Enum6\"");
            CHECK(generateTypename(var->type) == "Enum6");

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
            Variable* var = s->variables[4];
            REQUIRE(var);
            CHECK(var->name == "enum7");
            CHECK(var->key == "\"Enum7\"");
            CHECK(generateTypename(var->type) == "Enum7");

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
            Variable* var = s->variables[5];
            REQUIRE(var);
            CHECK(var->name == "enum8");
            CHECK(var->key == "\"Enum8\"");
            CHECK(generateTypename(var->type) == "Enum8");

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

        REQUIRE(s->children.size() == 4);
        {
            REQUIRE(s->children[0]->type == StackElement::Type::Enum);
            Enum* e = static_cast<Enum*>(s->children[0]);
            CHECK(e->name == "Enum5");
            CHECK(e->attributes.mappedTo.empty());
            CHECK(!e->attributes.stringify);
            CHECK(e->comment == "enum5 documentation");
            REQUIRE(e->elements.size() == 3);
            CHECK(e->elements[0]->name == "Value1");
            CHECK(e->elements[1]->name == "Value2");
            CHECK(e->elements[2]->name == "Value3");
        }
        {
            REQUIRE(s->children[1]->type == StackElement::Type::Enum);
            Enum* e = static_cast<Enum*>(s->children[1]);
            CHECK(e->name == "Enum6");
            CHECK(e->attributes.mappedTo.empty());
            CHECK(e->attributes.stringify);
            CHECK(e->comment == "enum6 documentation");
            REQUIRE(e->elements.size() == 3);
            CHECK(e->elements[0]->name == "Value1");
            CHECK(e->elements[1]->name == "Value2");
            CHECK(e->elements[2]->name == "Value3");
        }
        {
            REQUIRE(s->children[2]->type == StackElement::Type::Enum);
            Enum* e = static_cast<Enum*>(s->children[2]);
            CHECK(e->name == "Enum7");
            CHECK(e->attributes.mappedTo == "Enum1");
            CHECK(!e->attributes.stringify);
            CHECK(e->comment == "enum7 documentation");
            REQUIRE(e->elements.size() == 3);
            CHECK(e->elements[0]->name == "Value1");
            CHECK(e->elements[1]->name == "Value2");
            CHECK(e->elements[2]->name == "Value3");
        }
        {
            REQUIRE(s->children[3]->type == StackElement::Type::Enum);
            Enum* e = static_cast<Enum*>(s->children[3]);
            CHECK(e->name == "Enum8");
            CHECK(e->attributes.mappedTo == "Enum1");
            CHECK(e->attributes.stringify);
            CHECK(e->comment == "enum8 documentation");
            REQUIRE(e->elements.size() == 3);
            CHECK(e->elements[0]->name == "Value1");
            CHECK(e->elements[1]->name == "Value2");
            CHECK(e->elements[2]->name == "Value3");
        }
    }


    REQUIRE(code.enums.size() == 3);
    {
        {
            Enum* e = code.enums[0];
            REQUIRE(e);
            CHECK(e->name == "Enum2");
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
        }
        {
            Enum* e = code.enums[1];
            REQUIRE(e);
            CHECK(e->name == "Enum3");
            CHECK(e->parent == nullptr);
            CHECK(e->attributes.mappedTo == "Enum1");
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
        }
        {
            Enum* e = code.enums[2];
            REQUIRE(e);
            CHECK(e->name == "Enum4");
            CHECK(e->parent == nullptr);
            CHECK(e->attributes.mappedTo == "Enum1");
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
        }
    }
    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
