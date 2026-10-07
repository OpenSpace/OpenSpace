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

TEST_CASE("Parsing/Structs/Enums") {
    constexpr std::string_view Source = R"(

enum class Base {
    Value1,
    Value2,
    Value3
};

namespace abc::def::ghi {
    enum class OtherBase {
        Value5,
        Value4,
        Value3,
        Value2,
        Value1,
        Value0
    };
} // abc::def::ghi

struct [[codegen::Dictionary(Multiline)]] Parameters {

    // enum A documentation
    enum class A {
        Value1,
        value2,
        Value3
    };
    // variable enumAValue documentation
    A enumAValue;


    // enum B documentation
    enum class B {
        Value1,
        value2,
        Value3
    };
    // variable enumBValue documentation
    std::optional<B> enumBValue;


    // enum C documentation
    enum class C {
        Value1,
        value2,
        Value3
    };
    // variable enumCValue documentation
    std::vector<C> enumCValue;


    enum class D {
        VeryLongValueThatIsSoLongWithAnEvenLongerKeyWhichNeedsToBe
            [[codegen::key("On a second line or else one line would be too long")]],
        ValueB,
        SecondVeryLongValueThatWillCheckIfWeDetectPropertyThatWeHave
            [[codegen::key("a continuation line as the last element of the enum")]]
    };
    // variable enumDValue documentation
    D enumDValue;


    enum class [[codegen::stringify()]] E {
        Value1,
        value2,
        Value3
    };


    enum class
    [[codegen::stringify()]] F {
        Value1,
        Value2,
        Value3
    };


    enum class
    [[codegen::stringify()]]
    G {
        Value1,
        Value2,
        Value3
    };


    enum class
    [[codegen::stringify()]] H {
        Value1,
        Value2,
        Value3
    };


    enum class
    [[codegen::stringify()]]
    I
    {
        Value1,
        Value2,
        Value3
    };


    enum class [[codegen::map(Base)]] J {
        Value1,
        Value2,
        Value3
    };


    enum class
    [[codegen::map(Base)]] K {
        Value1,
        Value2,
        Value3
    };


    enum class
    [[codegen::map(Base)]]
    L {
        Value1,
        Value2,
        Value3
    };


    enum class
    [[codegen::map(Base)]] M {
        Value1,
        Value2,
        Value3
    };


    enum class
    [[codegen::map(Base)]]
    N
    {
        Value1,
        Value2,
        Value3
    };


    enum class [[codegen::map(abc::def::ghi::OtherBase)]]
    O2
    {
        Value1 = 0,
        Value2,
        Value3,
        Value4
    };

    enum class [[codegen::arrayify()]] P {
        Value1,
        value2,
        Value3
    };
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    CHECK(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());
    Struct* s = code.structs.front();
    REQUIRE(s);

    CHECK(s->name == "Parameters");
    CHECK(s->comment.empty());
    CHECK(s->attributes.dictionary == "Multiline");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 16);
    {
        StackElement* se = s->children[0];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "A");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[1];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "B");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[2];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "C");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[3];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "D");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "VeryLongValueThatIsSoLongWithAnEvenLongerKeyWhichNeedsToBe");
        CHECK(e->elements[1]->name == "ValueB");
        CHECK(e->elements[2]->name == "SecondVeryLongValueThatWillCheckIfWeDetectPropertyThatWeHave");
    }

    {
        StackElement* se = s->children[4];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "E");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[5];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "F");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[6];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "G");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[7];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "H");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[8];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "I");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[9];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "J");
        CHECK(e->attributes.mappedTo == "Base");
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[10];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "K");
        CHECK(e->attributes.mappedTo == "Base");
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[11];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "L");
        CHECK(e->attributes.mappedTo == "Base");
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[12];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "M");
        CHECK(e->attributes.mappedTo == "Base");
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[13];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "N");
        CHECK(e->attributes.mappedTo == "Base");
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    {
        StackElement* se = s->children[14];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "O2");
        CHECK(e->attributes.mappedTo == "abc::def::ghi::OtherBase");
        CHECK(!e->attributes.stringify);
        CHECK(!e->attributes.arrayify);
        REQUIRE(e->elements.size() == 4);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "Value2");
        CHECK(e->elements[2]->name == "Value3");
        CHECK(e->elements[3]->name == "Value4");
    }

    {
        StackElement* se = s->children[15];
        REQUIRE(se);
        REQUIRE(se->type == StackElement::Type::Enum);
        Enum* e = static_cast<Enum*>(se);
        CHECK(e->name == "P");
        CHECK(e->attributes.mappedTo.empty());
        CHECK(!e->attributes.stringify);
        CHECK(e->attributes.arrayify);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "Value1");
        CHECK(e->elements[1]->name == "value2");
        CHECK(e->elements[2]->name == "Value3");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
