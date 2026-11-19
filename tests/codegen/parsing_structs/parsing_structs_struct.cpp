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

TEST_CASE("Parsing/Structs/Struct:  Minimal", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
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
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    CHECK(s->variables.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  NoExhaustive no parameter", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name), codegen::noexhaustive()]] Parameters {
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
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    CHECK(s->variables.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  NoExhaustive true parameter", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name), codegen::noexhaustive(true)]] Parameters {
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    CHECK(s->variables.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  NoExhaustive false parameter", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name), codegen::noexhaustive(false)]] Parameters {
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(!s->attributes.noExhaustive);
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    CHECK(s->variables.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Substruct", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
struct A {
};
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->variables.empty());
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);


    REQUIRE(s->children.size() == 1);
    {
        StackElement* e = s->children[0];
        REQUIRE(e);
        CHECK(e->type == StackElement::Type::Struct);
        CHECK(e->name == "A");
        Struct* a = static_cast<Struct*>(e);
        CHECK(a->comment.empty());
        CHECK(a->attributes.dictionary.empty());
        CHECK(a->attributes.noExhaustive);
    }

    const std::string r = generateResult(code);
    REQUIRE(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Double Substruct", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
    struct A {
    };
    struct B {
    };
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->variables.empty());
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 2);
    {
        StackElement* e = s->children[0];
        CHECK(e->type == StackElement::Type::Struct);
        CHECK(e->name == "A");
        Struct* a = static_cast<Struct*>(e);
        CHECK(a->comment.empty());
        CHECK(a->attributes.dictionary.empty());
        CHECK(a->attributes.noExhaustive);
        CHECK(a->children.empty());
        CHECK(a->variables.empty());
    }
    {
        StackElement* e = s->children[1];
        CHECK(e->type == StackElement::Type::Struct);
        CHECK(e->name == "B");
        Struct* b = static_cast<Struct*>(e);
        CHECK(b->comment.empty());
        CHECK(b->attributes.dictionary.empty());
        CHECK(b->attributes.noExhaustive);
        CHECK(b->children.empty());
        CHECK(b->variables.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Variable", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
    // variable documentation
    int variable;
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->children.empty());
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->variables.size() == 1);
    {

        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "variable");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "variable documentation");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Double Variable", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
    // variable1 documentation
    int variable;

    // variable2 documentation
    float variable2;
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->children.empty());
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->variables.size() == 2);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "variable");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "variable1 documentation");
    }
    {
        Variable* var = s->variables[1];
        REQUIRE(var);
        CHECK(var->name == "variable2");
        CHECK(generateTypename(var->type) == "float");
        CHECK(var->comment == "variable2 documentation");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Empty Enum", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
    // enum documentation
    enum class Name {
    };
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->variables.empty());
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 1);
    {
        StackElement* e = s->children[0];
        REQUIRE(e);
        CHECK(e->type == StackElement::Type::Enum);
        CHECK(e->name == "Name");
        CHECK(e->comment == "enum documentation");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Enum", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
    // enum documentation
    enum class Name {
        Value,
        Value2
    };
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->variables.empty());
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 1);
    {
        StackElement* e = s->children[0];
        REQUIRE(e);
        CHECK(e->type == StackElement::Type::Enum);
        CHECK(e->name == "Name");
        CHECK(e->comment == "enum documentation");
        Enum* ee = static_cast<Enum*>(e);
        CHECK(ee->elements.size() == 2);
        CHECK(ee->elements[0]->name == "Value");
        CHECK(ee->elements[1]->name == "Value2");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Enum Key Attribute", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters {
    // enum documentation
    enum class Name {
        Value [[codegen::key(VK)]],
        Value2
    };
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->variables.empty());
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 1);
    {
        StackElement* e = s->children[0];
        REQUIRE(e);
        CHECK(e->type == StackElement::Type::Enum);
        CHECK(e->name == "Name");
        CHECK(e->comment == "enum documentation");
        Enum* ee = static_cast<Enum*>(e);
        REQUIRE(ee->elements.size() == 2);
        CHECK(ee->elements[0]->name == "Value");
        CHECK(ee->elements[0]->attributes.key == "VK");
        CHECK(ee->elements[1]->name == "Value2");
        CHECK(ee->elements[1]->attributes.key == "\"Value2\"");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  New lines", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct
[[codegen::Dictionary(Dictionary),
codegen::noexhaustive(false)]]
Parameters
{
    int value;
};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Dictionary");
    CHECK(!s->attributes.noExhaustive);
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->variables.size() == 1);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "value");
        CHECK(var->key == "\"Value\"");
        CHECK(generateTypename(var->type) == "int");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  New lines go nuts", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(

struct



[[codegen::Dictionary(Dictionary),



codegen::noexhaustive(false)]]

Parameters

{

    int value;



};)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    REQUIRE(s);
    CHECK(s->name == "Parameters");
    CHECK(s->attributes.dictionary == "Dictionary");
    CHECK(!s->attributes.noExhaustive);
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->variables.size() == 1);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "value");
        CHECK(var->key == "\"Value\"");
        CHECK(generateTypename(var->type) == "int");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Struct:  Substruct vector documentation", "[Parsing][Structs]")
{
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(D)]] P {
    // struct a documentation
    struct A {
        int a;
    };
    // vector documentation
    std::vector<A> aa;
};
)";

    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();

    CHECK(s->name == "P");
    CHECK(s->attributes.dictionary == "D");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->comment.empty());
    CHECK(s->parent == nullptr);

    REQUIRE(s->children.size() == 1);
    CHECK(s->children[0]->type == StackElement::Type::Struct);
    CHECK(s->children[0]->name == "A");
    CHECK(s->children[0]->comment == "struct a documentation");

    REQUIRE(s->variables.size() == 1);
    CHECK(s->variables[0]->type->tag == VariableType::Tag::VectorType);
    CHECK(
        static_cast<VectorType*>(s->variables[0]->type)->type->tag ==
        VariableType::Tag::CustomType
    );
    CHECK(s->variables[0]->name == "aa");
    CHECK(s->variables[0]->comment == "vector documentation");

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
