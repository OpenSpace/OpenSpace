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

TEST_CASE("Parsing/Structs/Map: Minimal", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    std::map<std::string, std::string> p;
};)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    CHECK(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->children.empty());
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());

    REQUIRE(s->variables.size() == 1);
    CHECK(s->variables[0]->name == "p");
    CHECK(s->variables[0]->key == "\"P\"");

    CHECK(s->variables[0]->attributes.annotation.empty());
    CHECK(s->variables[0]->attributes.greater.empty());
    CHECK(s->variables[0]->attributes.greaterequal.empty());
    CHECK(s->variables[0]->attributes.inlist.empty());
    CHECK(s->variables[0]->attributes.less.empty());
    CHECK(s->variables[0]->attributes.lessequal.empty());
    CHECK(s->variables[0]->attributes.notinrange.empty());
    CHECK(s->variables[0]->attributes.reference.empty());
    CHECK(s->variables[0]->attributes.unequal.empty());
    CHECK(!s->variables[0]->attributes.isColor);
    CHECK(!s->variables[0]->attributes.isDirectory);
    CHECK(!s->variables[0]->attributes.isDateTime);
    CHECK(!s->variables[0]->attributes.isIdentifier);
    CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
    CHECK(!s->variables[0]->attributes.isPrivate);

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Map: Enum Key", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    enum class KeyType {
        Key1,
        Key2,
        Key3
    };

    std::map<KeyType, std::string> p;
};)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    REQUIRE(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());

    Struct* s = code.structs.front();
    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 1);
    REQUIRE(s->children.front()->type == StackElement::Type::Enum);
    Enum* e = static_cast<Enum*>(s->children.front());
    REQUIRE(e);

    CHECK(e->attributes.mappedTo.empty());
    CHECK(!e->attributes.stringify);
    CHECK(!e->attributes.arrayify);
    REQUIRE(e->elements.size() == 3);
    {
        EnumElement* ee = e->elements[0];
        REQUIRE(ee);
        CHECK(ee->name == "Key1");
    }
    {
        EnumElement* ee = e->elements[1];
        REQUIRE(ee);
        CHECK(ee->name == "Key2");
    }
    {
        EnumElement* ee = e->elements[2];
        REQUIRE(ee);
        CHECK(ee->name == "Key3");
    }


    REQUIRE(s->variables.size() == 1);
    CHECK(s->variables[0]->name == "p");
    CHECK(s->variables[0]->key == "\"P\"");

    CHECK(s->variables[0]->attributes.annotation.empty());
    CHECK(s->variables[0]->attributes.greater.empty());
    CHECK(s->variables[0]->attributes.greaterequal.empty());
    CHECK(s->variables[0]->attributes.inlist.empty());
    CHECK(s->variables[0]->attributes.less.empty());
    CHECK(s->variables[0]->attributes.lessequal.empty());
    CHECK(s->variables[0]->attributes.notinrange.empty());
    CHECK(s->variables[0]->attributes.reference.empty());
    CHECK(s->variables[0]->attributes.unequal.empty());
    CHECK(!s->variables[0]->attributes.isColor);
    CHECK(!s->variables[0]->attributes.isDirectory);
    CHECK(!s->variables[0]->attributes.isDateTime);
    CHECK(!s->variables[0]->attributes.isIdentifier);
    CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
    CHECK(!s->variables[0]->attributes.isPrivate);

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Map: Enum Key Multiple", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    enum class KeyType {
        Key1,
        Key2,
        Key3
    };

    enum class KeyType2 {
        Key21,
        Key22,
        Key23
    };

    std::map<KeyType, std::string> p;
    std::map<KeyType, std::string> p2;
    std::map<KeyType, std::string> q;
    std::map<KeyType, std::string> q2;
};)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    REQUIRE(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());

    Struct* s = code.structs.front();
    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 2);
    {
        REQUIRE(s->children[0]->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(s->children[0]);
        REQUIRE(e);

        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        {
            EnumElement* ee = e->elements[0];
            REQUIRE(ee);
            CHECK(ee->name == "Key1");
        }
        {
            EnumElement* ee = e->elements[1];
            REQUIRE(ee);
            CHECK(ee->name == "Key2");
        }
        {
            EnumElement* ee = e->elements[2];
            REQUIRE(ee);
            CHECK(ee->name == "Key3");
        }
    }
    {
        REQUIRE(s->children[1]->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(s->children[1]);
        REQUIRE(e);

        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        {
            EnumElement* ee = e->elements[0];
            REQUIRE(ee);
            CHECK(ee->name == "Key21");
        }
        {
            EnumElement* ee = e->elements[1];
            REQUIRE(ee);
            CHECK(ee->name == "Key22");
        }
        {
            EnumElement* ee = e->elements[2];
            REQUIRE(ee);
            CHECK(ee->name == "Key23");
        }
    }

    REQUIRE(s->variables.size() == 4);
    {
        Variable* v = s->variables[0];
        CHECK(v->name == "p");
        CHECK(v->key == "\"P\"");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.notinrange.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
        CHECK(!v->attributes.isColor);
        CHECK(!v->attributes.isDirectory);
        CHECK(!v->attributes.isDateTime);
        CHECK(!v->attributes.isIdentifier);
        CHECK(!v->attributes.mustBeNotEmpty);
        CHECK(!v->attributes.isPrivate);
    }
    {
        Variable* v = s->variables[1];
        CHECK(v->name == "p2");
        CHECK(v->key == "\"P2\"");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.notinrange.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
        CHECK(!v->attributes.isColor);
        CHECK(!v->attributes.isDirectory);
        CHECK(!v->attributes.isDateTime);
        CHECK(!v->attributes.isIdentifier);
        CHECK(!v->attributes.mustBeNotEmpty);
        CHECK(!v->attributes.isPrivate);
    }
    {
        Variable* v = s->variables[2];
        CHECK(v->name == "q");
        CHECK(v->key == "\"Q\"");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.notinrange.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
        CHECK(!v->attributes.isColor);
        CHECK(!v->attributes.isDirectory);
        CHECK(!v->attributes.isDateTime);
        CHECK(!v->attributes.isIdentifier);
        CHECK(!v->attributes.mustBeNotEmpty);
        CHECK(!v->attributes.isPrivate);
    }
    {
        Variable* v = s->variables[3];
        CHECK(v->name == "q2");
        CHECK(v->key == "\"Q2\"");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.notinrange.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
        CHECK(!v->attributes.isColor);
        CHECK(!v->attributes.isDirectory);
        CHECK(!v->attributes.isDateTime);
        CHECK(!v->attributes.isIdentifier);
        CHECK(!v->attributes.mustBeNotEmpty);
        CHECK(!v->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Map: Enum Key (Outer)", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    enum class KeyType {
        Key1,
        Key2,
        Key3
    };

    struct Inner {
        std::map<KeyType, std::string> p;
    };
};)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    REQUIRE(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());

    Struct* s = code.structs.front();
    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->variables.empty());

    REQUIRE(s->children.size() == 2);
    {
        REQUIRE(s->children[0]->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(s->children[0]);
        REQUIRE(e);

        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        {
            EnumElement* ee = e->elements[0];
            REQUIRE(ee);
            CHECK(ee->name == "Key1");
        }
        {
            EnumElement* ee = e->elements[1];
            REQUIRE(ee);
            CHECK(ee->name == "Key2");
        }
        {
            EnumElement* ee = e->elements[2];
            REQUIRE(ee);
            CHECK(ee->name == "Key3");
        }
    }
    {
        REQUIRE(s->children[1]->type == StackElement::Type::Struct);
        Struct* ss = static_cast<Struct*>(s->children[1]);
        REQUIRE(ss);

        REQUIRE(ss->variables.size() == 1);
        CHECK(ss->variables[0]->name == "p");
        CHECK(ss->variables[0]->key == "\"P\"");

        CHECK(ss->variables[0]->attributes.annotation.empty());
        CHECK(ss->variables[0]->attributes.greater.empty());
        CHECK(ss->variables[0]->attributes.greaterequal.empty());
        CHECK(ss->variables[0]->attributes.inlist.empty());
        CHECK(ss->variables[0]->attributes.less.empty());
        CHECK(ss->variables[0]->attributes.lessequal.empty());
        CHECK(ss->variables[0]->attributes.notinrange.empty());
        CHECK(ss->variables[0]->attributes.reference.empty());
        CHECK(ss->variables[0]->attributes.unequal.empty());
        CHECK(!ss->variables[0]->attributes.isColor);
        CHECK(!ss->variables[0]->attributes.isDirectory);
        CHECK(!ss->variables[0]->attributes.isDateTime);
        CHECK(!ss->variables[0]->attributes.isIdentifier);
        CHECK(!ss->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!ss->variables[0]->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
