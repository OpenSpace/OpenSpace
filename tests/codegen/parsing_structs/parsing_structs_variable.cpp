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

TEST_CASE("Parsing/Structs/Variable:  Basic Types", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    bool boolVariable;
    int intVariable;
    double doubleValue;
    float floatValue;
    std::string stringValue;
    glm::ivec2 ivec2Value;
    glm::ivec3 ivec3Value;
    glm::ivec4 ivec4Value;
    glm::dvec2 dvec2Value;
    glm::dvec3 dvec3Value;
    glm::dvec4 dvec4Value;
    glm::vec2 vec2Value;
    glm::vec3 vec3Value;
    glm::vec4 vec4Value;
    glm::mat2x2 mat2x2Value;
    glm::mat2 mat2Value;
    glm::mat2x3 mat2x3Value;
    glm::mat2x4 mat2x4Value;
    glm::mat3x2 mat3x2Value;
    glm::mat3x3 mat3x3Value;
    glm::mat3 mat3Value;
    glm::mat3x4 mat3x4Value;
    glm::mat4x2 mat4x2Value;
    glm::mat4x3 mat4x3Value;
    glm::mat4x4 mat4x4Value;
    glm::mat4 mat4Value;
    glm::dmat2x2 dmat2x2Value;
    glm::dmat2 dmat2Value;
    glm::dmat2x3 dmat2x3Value;
    glm::dmat2x4 dmat2x4Value;
    glm::dmat3x2 dmat3x2Value;
    glm::dmat3x3 dmat3x3Value;
    glm::dmat3 dmat3Value;
    glm::dmat3x4 dmat3x4Value;
    glm::dmat4x2 dmat4x2Value;
    glm::dmat4x3 dmat4x3Value;
    glm::dmat4x4 dmat4x4Value;
    glm::dmat4 dmat4Value;
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
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 38);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "boolVariable");
        CHECK(var->key == "\"BoolVariable\"");
        CHECK(generateTypename(var->type) == "bool");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "intVariable");
        CHECK(var->key == "\"IntVariable\"");
        CHECK(generateTypename(var->type) == "int");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "doubleValue");
        CHECK(var->key == "\"DoubleValue\"");
        CHECK(generateTypename(var->type) == "double");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "floatValue");
        CHECK(var->key == "\"FloatValue\"");
        CHECK(generateTypename(var->type) == "float");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "stringValue");
        CHECK(var->key == "\"StringValue\"");
        CHECK(generateTypename(var->type) == "std::string");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "ivec2Value");
        CHECK(var->key == "\"Ivec2Value\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "ivec3Value");
        CHECK(var->key == "\"Ivec3Value\"");
        CHECK(generateTypename(var->type) == "glm::ivec3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "ivec4Value");
        CHECK(var->key == "\"Ivec4Value\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "dvec2Value");
        CHECK(var->key == "\"Dvec2Value\"");
        CHECK(generateTypename(var->type) == "glm::dvec2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "dvec3Value");
        CHECK(var->key == "\"Dvec3Value\"");
        CHECK(generateTypename(var->type) == "glm::dvec3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "dvec4Value");
        CHECK(var->key == "\"Dvec4Value\"");
        CHECK(generateTypename(var->type) == "glm::dvec4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "vec2Value");
        CHECK(var->key == "\"Vec2Value\"");
        CHECK(generateTypename(var->type) == "glm::vec2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "vec3Value");
        CHECK(var->key == "\"Vec3Value\"");
        CHECK(generateTypename(var->type) == "glm::vec3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "vec4Value");
        CHECK(var->key == "\"Vec4Value\"");
        CHECK(generateTypename(var->type) == "glm::vec4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "mat2x2Value");
        CHECK(var->key == "\"Mat2x2Value\"");
        CHECK(generateTypename(var->type) == "glm::mat2x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "mat2Value");
        CHECK(var->key == "\"Mat2Value\"");
        CHECK(generateTypename(var->type) == "glm::mat2x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "mat2x3Value");
        CHECK(var->key == "\"Mat2x3Value\"");
        CHECK(generateTypename(var->type) == "glm::mat2x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "mat2x4Value");
        CHECK(var->key == "\"Mat2x4Value\"");
        CHECK(generateTypename(var->type) == "glm::mat2x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "mat3x2Value");
        CHECK(var->key == "\"Mat3x2Value\"");
        CHECK(generateTypename(var->type) == "glm::mat3x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "mat3x3Value");
        CHECK(var->key == "\"Mat3x3Value\"");
        CHECK(generateTypename(var->type) == "glm::mat3x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "mat3Value");
        CHECK(var->key == "\"Mat3Value\"");
        CHECK(generateTypename(var->type) == "glm::mat3x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "mat3x4Value");
        CHECK(var->key == "\"Mat3x4Value\"");
        CHECK(generateTypename(var->type) == "glm::mat3x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "mat4x2Value");
        CHECK(var->key == "\"Mat4x2Value\"");
        CHECK(generateTypename(var->type) == "glm::mat4x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "mat4x3Value");
        CHECK(var->key == "\"Mat4x3Value\"");
        CHECK(generateTypename(var->type) == "glm::mat4x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "mat4x4Value");
        CHECK(var->key == "\"Mat4x4Value\"");
        CHECK(generateTypename(var->type) == "glm::mat4x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "mat4Value");
        CHECK(var->key == "\"Mat4Value\"");
        CHECK(generateTypename(var->type) == "glm::mat4x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2Value");
        CHECK(var->key == "\"Dmat2x2Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat2x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "dmat2Value");
        CHECK(var->key == "\"Dmat2Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat2x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3Value");
        CHECK(var->key == "\"Dmat2x3Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat2x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4Value");
        CHECK(var->key == "\"Dmat2x4Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat2x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2Value");
        CHECK(var->key == "\"Dmat3x2Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat3x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3Value");
        CHECK(var->key == "\"Dmat3x3Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat3x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "dmat3Value");
        CHECK(var->key == "\"Dmat3Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat3x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4Value");
        CHECK(var->key == "\"Dmat3x4Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat3x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2Value");
        CHECK(var->key == "\"Dmat4x2Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat4x2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3Value");
        CHECK(var->key == "\"Dmat4x3Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat4x3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4Value");
        CHECK(var->key == "\"Dmat4x4Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat4x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "dmat4Value");
        CHECK(var->key == "\"Dmat4Value\"");
        CHECK(generateTypename(var->type) == "glm::dmat4x4");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing Variable: Vector Base Types", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    std::vector<bool> boolVariable;
    std::vector<int> intVariable;
    std::vector<double> doubleValue;
    std::vector<float> floatValue;
    std::vector<std::string> stringValue;
    std::vector<glm::ivec2> ivec2Value;
    std::vector<glm::ivec3> ivec3Value;
    std::vector<glm::ivec4> ivec4Value;
    std::vector<glm::dvec2> dvec2Value;
    std::vector<glm::dvec3> dvec3Value;
    std::vector<glm::dvec4> dvec4Value;
    std::vector<glm::vec2> vec2Value;
    std::vector<glm::vec3> vec3Value;
    std::vector<glm::vec4> vec4Value;
    std::vector<glm::mat2x2> mat2x2Value;
    std::vector<glm::mat2> mat2Value;
    std::vector<glm::mat2x3> mat2x3Value;
    std::vector<glm::mat2x4> mat2x4Value;
    std::vector<glm::mat3x2> mat3x2Value;
    std::vector<glm::mat3x3> mat3x3Value;
    std::vector<glm::mat3> mat3Value;
    std::vector<glm::mat3x4> mat3x4Value;
    std::vector<glm::mat4x2> mat4x2Value;
    std::vector<glm::mat4x3> mat4x3Value;
    std::vector<glm::mat4x4> mat4x4Value;
    std::vector<glm::mat4> mat4Value;
    std::vector<glm::dmat2x2> dmat2x2Value;
    std::vector<glm::dmat2> dmat2Value;
    std::vector<glm::dmat2x3> dmat2x3Value;
    std::vector<glm::dmat2x4> dmat2x4Value;
    std::vector<glm::dmat3x2> dmat3x2Value;
    std::vector<glm::dmat3x3> dmat3x3Value;
    std::vector<glm::dmat3> dmat3Value;
    std::vector<glm::dmat3x4> dmat3x4Value;
    std::vector<glm::dmat4x2> dmat4x2Value;
    std::vector<glm::dmat4x3> dmat4x3Value;
    std::vector<glm::dmat4x4> dmat4x4Value;
    std::vector<glm::dmat4> dmat4Value;
};
)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();
    REQUIRE(s);

    CHECK(s->name == "Parameters");
    CHECK(s->comment.empty());
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 38);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "boolVariable");
        CHECK(var->key == "\"BoolVariable\"");
        CHECK(generateTypename(var->type) == "std::vector<bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "intVariable");
        CHECK(var->key == "\"IntVariable\"");
        CHECK(generateTypename(var->type) == "std::vector<int>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "doubleValue");
        CHECK(var->key == "\"DoubleValue\"");
        CHECK(generateTypename(var->type) == "std::vector<double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "floatValue");
        CHECK(var->key == "\"FloatValue\"");
        CHECK(generateTypename(var->type) == "std::vector<float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "stringValue");
        CHECK(var->key == "\"StringValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "ivec2Value");
        CHECK(var->key == "\"Ivec2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "ivec3Value");
        CHECK(var->key == "\"Ivec3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "ivec4Value");
        CHECK(var->key == "\"Ivec4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "dvec2Value");
        CHECK(var->key == "\"Dvec2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dvec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "dvec3Value");
        CHECK(var->key == "\"Dvec3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dvec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "dvec4Value");
        CHECK(var->key == "\"Dvec4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dvec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "vec2Value");
        CHECK(var->key == "\"Vec2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "vec3Value");
        CHECK(var->key == "\"Vec3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "vec4Value");
        CHECK(var->key == "\"Vec4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "mat2x2Value");
        CHECK(var->key == "\"Mat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "mat2Value");
        CHECK(var->key == "\"Mat2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "mat2x3Value");
        CHECK(var->key == "\"Mat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "mat2x4Value");
        CHECK(var->key == "\"Mat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "mat3x2Value");
        CHECK(var->key == "\"Mat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "mat3x3Value");
        CHECK(var->key == "\"Mat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "mat3Value");
        CHECK(var->key == "\"Mat3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "mat3x4Value");
        CHECK(var->key == "\"Mat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "mat4x2Value");
        CHECK(var->key == "\"Mat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "mat4x3Value");
        CHECK(var->key == "\"Mat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "mat4x4Value");
        CHECK(var->key == "\"Mat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "mat4Value");
        CHECK(var->key == "\"Mat4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2Value");
        CHECK(var->key == "\"Dmat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "dmat2Value");
        CHECK(var->key == "\"Dmat2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3Value");
        CHECK(var->key == "\"Dmat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4Value");
        CHECK(var->key == "\"Dmat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2Value");
        CHECK(var->key == "\"Dmat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3Value");
        CHECK(var->key == "\"Dmat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "dmat3Value");
        CHECK(var->key == "\"Dmat3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4Value");
        CHECK(var->key == "\"Dmat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2Value");
        CHECK(var->key == "\"Dmat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3Value");
        CHECK(var->key == "\"Dmat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4Value");
        CHECK(var->key == "\"Dmat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "dmat4Value");
        CHECK(var->key == "\"Dmat4Value\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Variable:  Optional Base Types", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    std::optional<bool> boolVariable;
    std::optional<int> intVariable;
    std::optional<double> doubleValue;
    std::optional<float> floatValue;
    std::optional<std::string> stringValue;
    std::optional<glm::ivec2> ivec2Value;
    std::optional<glm::ivec3> ivec3Value;
    std::optional<glm::ivec4> ivec4Value;
    std::optional<glm::dvec2> dvec2Value;
    std::optional<glm::dvec3> dvec3Value;
    std::optional<glm::dvec4> dvec4Value;
    std::optional<glm::vec2> vec2Value;
    std::optional<glm::vec3> vec3Value;
    std::optional<glm::vec4> vec4Value;
    std::optional<glm::mat2x2> mat2x2Value;
    std::optional<glm::mat2> mat2Value;
    std::optional<glm::mat2x3> mat2x3Value;
    std::optional<glm::mat2x4> mat2x4Value;
    std::optional<glm::mat3x2> mat3x2Value;
    std::optional<glm::mat3x3> mat3x3Value;
    std::optional<glm::mat3> mat3Value;
    std::optional<glm::mat3x4> mat3x4Value;
    std::optional<glm::mat4x2> mat4x2Value;
    std::optional<glm::mat4x3> mat4x3Value;
    std::optional<glm::mat4x4> mat4x4Value;
    std::optional<glm::mat4> mat4Value;
    std::optional<glm::dmat2x2> dmat2x2Value;
    std::optional<glm::dmat2> dmat2Value;
    std::optional<glm::dmat2x3> dmat2x3Value;
    std::optional<glm::dmat2x4> dmat2x4Value;
    std::optional<glm::dmat3x2> dmat3x2Value;
    std::optional<glm::dmat3x3> dmat3x3Value;
    std::optional<glm::dmat3> dmat3Value;
    std::optional<glm::dmat3x4> dmat3x4Value;
    std::optional<glm::dmat4x2> dmat4x2Value;
    std::optional<glm::dmat4x3> dmat4x3Value;
    std::optional<glm::dmat4x4> dmat4x4Value;
    std::optional<glm::dmat4> dmat4Value;
};
)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();
    REQUIRE(s);

    CHECK(s->name == "Parameters");
    CHECK(s->comment.empty());
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 38);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "boolVariable");
        CHECK(var->key == "\"BoolVariable\"");
        CHECK(generateTypename(var->type) == "std::optional<bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "intVariable");
        CHECK(var->key == "\"IntVariable\"");
        CHECK(generateTypename(var->type) == "std::optional<int>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "doubleValue");
        CHECK(var->key == "\"DoubleValue\"");
        CHECK(generateTypename(var->type) == "std::optional<double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "floatValue");
        CHECK(var->key == "\"FloatValue\"");
        CHECK(generateTypename(var->type) == "std::optional<float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "stringValue");
        CHECK(var->key == "\"StringValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "ivec2Value");
        CHECK(var->key == "\"Ivec2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "ivec3Value");
        CHECK(var->key == "\"Ivec3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "ivec4Value");
        CHECK(var->key == "\"Ivec4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "dvec2Value");
        CHECK(var->key == "\"Dvec2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dvec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "dvec3Value");
        CHECK(var->key == "\"Dvec3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dvec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "dvec4Value");
        CHECK(var->key == "\"Dvec4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dvec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "vec2Value");
        CHECK(var->key == "\"Vec2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "vec3Value");
        CHECK(var->key == "\"Vec3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "vec4Value");
        CHECK(var->key == "\"Vec4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "mat2x2Value");
        CHECK(var->key == "\"Mat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "mat2Value");
        CHECK(var->key == "\"Mat2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "mat2x3Value");
        CHECK(var->key == "\"Mat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "mat2x4Value");
        CHECK(var->key == "\"Mat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "mat3x2Value");
        CHECK(var->key == "\"Mat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "mat3x3Value");
        CHECK(var->key == "\"Mat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "mat3Value");
        CHECK(var->key == "\"Mat3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "mat3x4Value");
        CHECK(var->key == "\"Mat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "mat4x2Value");
        CHECK(var->key == "\"Mat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "mat4x3Value");
        CHECK(var->key == "\"Mat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "mat4x4Value");
        CHECK(var->key == "\"Mat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "mat4Value");
        CHECK(var->key == "\"Mat4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2Value");
        CHECK(var->key == "\"Dmat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "dmat2Value");
        CHECK(var->key == "\"Dmat2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3Value");
        CHECK(var->key == "\"Dmat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4Value");
        CHECK(var->key == "\"Dmat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2Value");
        CHECK(var->key == "\"Dmat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3Value");
        CHECK(var->key == "\"Dmat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "dmat3Value");
        CHECK(var->key == "\"Dmat3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4Value");
        CHECK(var->key == "\"Dmat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2Value");
        CHECK(var->key == "\"Dmat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3Value");
        CHECK(var->key == "\"Dmat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4Value");
        CHECK(var->key == "\"Dmat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "dmat4Value");
        CHECK(var->key == "\"Dmat4Value\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Variable:  Array Base Types", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    std::array<bool, 3> boolVariable;
    std::array<int, 3> intVariable;
    std::array<double, 3> doubleValue;
    std::array<float, 3> floatValue;
    std::array<std::string, 3> stringValue;
    std::array<glm::ivec2, 3> ivec2Value;
    std::array<glm::ivec3, 3> ivec3Value;
    std::array<glm::ivec4, 3> ivec4Value;
    std::array<glm::dvec2, 3> dvec2Value;
    std::array<glm::dvec3, 3> dvec3Value;
    std::array<glm::dvec4, 3> dvec4Value;
    std::array<glm::vec2, 3> vec2Value;
    std::array<glm::vec3, 3> vec3Value;
    std::array<glm::vec4, 3> vec4Value;
    std::array<glm::mat2x2, 3> mat2x2Value;
    std::array<glm::mat2, 3> mat2Value;
    std::array<glm::mat2x3, 3> mat2x3Value;
    std::array<glm::mat2x4, 3> mat2x4Value;
    std::array<glm::mat3x2, 3> mat3x2Value;
    std::array<glm::mat3x3, 3> mat3x3Value;
    std::array<glm::mat3, 3> mat3Value;
    std::array<glm::mat3x4, 3> mat3x4Value;
    std::array<glm::mat4x2, 3> mat4x2Value;
    std::array<glm::mat4x3, 3> mat4x3Value;
    std::array<glm::mat4x4, 3> mat4x4Value;
    std::array<glm::mat4, 3> mat4Value;
    std::array<glm::dmat2x2, 3> dmat2x2Value;
    std::array<glm::dmat2, 3> dmat2Value;
    std::array<glm::dmat2x3, 3> dmat2x3Value;
    std::array<glm::dmat2x4, 3> dmat2x4Value;
    std::array<glm::dmat3x2, 3> dmat3x2Value;
    std::array<glm::dmat3x3, 3> dmat3x3Value;
    std::array<glm::dmat3, 3> dmat3Value;
    std::array<glm::dmat3x4, 3> dmat3x4Value;
    std::array<glm::dmat4x2, 3> dmat4x2Value;
    std::array<glm::dmat4x3, 3> dmat4x3Value;
    std::array<glm::dmat4x4, 3> dmat4x4Value;
    std::array<glm::dmat4, 3> dmat4Value;
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
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 38);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "boolVariable");
        CHECK(var->key == "\"BoolVariable\"");
        CHECK(generateTypename(var->type) == "std::array<bool, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "intVariable");
        CHECK(var->key == "\"IntVariable\"");
        CHECK(generateTypename(var->type) == "std::array<int, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "doubleValue");
        CHECK(var->key == "\"DoubleValue\"");
        CHECK(generateTypename(var->type) == "std::array<double, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "floatValue");
        CHECK(var->key == "\"FloatValue\"");
        CHECK(generateTypename(var->type) == "std::array<float, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "stringValue");
        CHECK(var->key == "\"StringValue\"");
        CHECK(generateTypename(var->type) == "std::array<std::string, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "ivec2Value");
        CHECK(var->key == "\"Ivec2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::ivec2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "ivec3Value");
        CHECK(var->key == "\"Ivec3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::ivec3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "ivec4Value");
        CHECK(var->key == "\"Ivec4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::ivec4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "dvec2Value");
        CHECK(var->key == "\"Dvec2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dvec2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "dvec3Value");
        CHECK(var->key == "\"Dvec3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dvec3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "dvec4Value");
        CHECK(var->key == "\"Dvec4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dvec4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "vec2Value");
        CHECK(var->key == "\"Vec2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::vec2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "vec3Value");
        CHECK(var->key == "\"Vec3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::vec3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "vec4Value");
        CHECK(var->key == "\"Vec4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::vec4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "mat2x2Value");
        CHECK(var->key == "\"Mat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat2x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "mat2Value");
        CHECK(var->key == "\"Mat2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat2x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "mat2x3Value");
        CHECK(var->key == "\"Mat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat2x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "mat2x4Value");
        CHECK(var->key == "\"Mat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat2x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "mat3x2Value");
        CHECK(var->key == "\"Mat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat3x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "mat3x3Value");
        CHECK(var->key == "\"Mat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat3x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "mat3Value");
        CHECK(var->key == "\"Mat3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat3x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "mat3x4Value");
        CHECK(var->key == "\"Mat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat3x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "mat4x2Value");
        CHECK(var->key == "\"Mat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat4x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "mat4x3Value");
        CHECK(var->key == "\"Mat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat4x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "mat4x4Value");
        CHECK(var->key == "\"Mat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat4x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "mat4Value");
        CHECK(var->key == "\"Mat4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::mat4x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2Value");
        CHECK(var->key == "\"Dmat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat2x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "dmat2Value");
        CHECK(var->key == "\"Dmat2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat2x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3Value");
        CHECK(var->key == "\"Dmat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat2x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4Value");
        CHECK(var->key == "\"Dmat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat2x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2Value");
        CHECK(var->key == "\"Dmat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat3x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3Value");
        CHECK(var->key == "\"Dmat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat3x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "dmat3Value");
        CHECK(var->key == "\"Dmat3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat3x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4Value");
        CHECK(var->key == "\"Dmat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat3x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2Value");
        CHECK(var->key == "\"Dmat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat4x2, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3Value");
        CHECK(var->key == "\"Dmat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat4x3, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4Value");
        CHECK(var->key == "\"Dmat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat4x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "dmat4Value");
        CHECK(var->key == "\"Dmat4Value\"");
        CHECK(generateTypename(var->type) == "std::array<glm::dmat4x4, 3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Variable:  Variant Types", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    std::variant<std::string, float, bool> boolVariable;
    std::variant<std::string, float, int> intVariable;
    std::variant<std::string, float, double> doubleValue;
    std::variant<std::string, double, float> floatValue;
    std::variant<int, float, std::string> stringValue;
    std::variant<std::string, float, glm::ivec2> ivec2Value;
    std::variant<std::string, float, glm::ivec3> ivec3Value;
    std::variant<std::string, float, glm::ivec4> ivec4Value;
    std::variant<std::string, float, glm::dvec2> dvec2Value;
    std::variant<std::string, float, glm::dvec3> dvec3Value;
    std::variant<std::string, float, glm::dvec4> dvec4Value;
    std::variant<std::string, float, glm::vec2> vec2Value;
    std::variant<std::string, float, glm::vec3> vec3Value;
    std::variant<std::string, float, glm::vec4> vec4Value;
    std::variant<std::string, float, glm::mat2x2> mat2x2Value;
    std::variant<std::string, float, glm::mat2> mat2Value;
    std::variant<std::string, float, glm::mat2x3> mat2x3Value;
    std::variant<std::string, float, glm::mat2x4> mat2x4Value;
    std::variant<std::string, float, glm::mat3x2> mat3x2Value;
    std::variant<std::string, float, glm::mat3x3> mat3x3Value;
    std::variant<std::string, float, glm::mat3> mat3Value;
    std::variant<std::string, float, glm::mat3x4> mat3x4Value;
    std::variant<std::string, float, glm::mat4x2> mat4x2Value;
    std::variant<std::string, float, glm::mat4x3> mat4x3Value;
    std::variant<std::string, float, glm::mat4x4> mat4x4Value;
    std::variant<std::string, float, glm::mat4> mat4Value;
    std::variant<std::string, float, glm::dmat2x2> dmat2x2Value;
    std::variant<std::string, float, glm::dmat2> dmat2Value;
    std::variant<std::string, float, glm::dmat2x3> dmat2x3Value;
    std::variant<std::string, float, glm::dmat2x4> dmat2x4Value;
    std::variant<std::string, float, glm::dmat3x2> dmat3x2Value;
    std::variant<std::string, float, glm::dmat3x3> dmat3x3Value;
    std::variant<std::string, float, glm::dmat3> dmat3Value;
    std::variant<std::string, float, glm::dmat3x4> dmat3x4Value;
    std::variant<std::string, float, glm::dmat4x2> dmat4x2Value;
    std::variant<std::string, float, glm::dmat4x3> dmat4x3Value;
    std::variant<std::string, float, glm::dmat4x4> dmat4x4Value;
    std::variant<std::string, float, glm::dmat4> dmat4Value;

    std::variant<bool, std::string, float> boolVariableInvert;
    std::variant<int, std::string, float> intVariableInvert;
    std::variant<double, std::string, float> doubleValueInvert;
    std::variant<float, std::string, double> floatValueInvert;
    std::variant<std::string, int, float> stringValueInvert;
    std::variant<glm::ivec2, std::string, float> ivec2ValueInvert;
    std::variant<glm::ivec3, std::string, float> ivec3ValueInvert;
    std::variant<glm::ivec4, std::string, float> ivec4ValueInvert;
    std::variant<glm::dvec2, std::string, float> dvec2ValueInvert;
    std::variant<glm::dvec3, std::string, float> dvec3ValueInvert;
    std::variant<glm::dvec4, std::string, float> dvec4ValueInvert;
    std::variant<glm::vec2, std::string, float> vec2ValueInvert;
    std::variant<glm::vec3, std::string, float> vec3ValueInvert;
    std::variant<glm::vec4, std::string, float> vec4ValueInvert;
    std::variant<glm::mat2x2, std::string, float> mat2x2ValueInvert;
    std::variant<glm::mat2, std::string, float> mat2ValueInvert;
    std::variant<glm::mat2x3, std::string, float> mat2x3ValueInvert;
    std::variant<glm::mat2x4, std::string, float> mat2x4ValueInvert;
    std::variant<glm::mat3x2, std::string, float> mat3x2ValueInvert;
    std::variant<glm::mat3x3, std::string, float> mat3x3ValueInvert;
    std::variant<glm::mat3, std::string, float> mat3ValueInvert;
    std::variant<glm::mat3x4, std::string, float> mat3x4ValueInvert;
    std::variant<glm::mat4x2, std::string, float> mat4x2ValueInvert;
    std::variant<glm::mat4x3, std::string, float> mat4x3ValueInvert;
    std::variant<glm::mat4x4, std::string, float> mat4x4ValueInvert;
    std::variant<glm::mat4, std::string, float> mat4ValueInvert;
    std::variant<glm::dmat2x2, std::string, float> dmat2x2ValueInvert;
    std::variant<glm::dmat2, std::string, float> dmat2ValueInvert;
    std::variant<glm::dmat2x3, std::string, float> dmat2x3ValueInvert;
    std::variant<glm::dmat2x4, std::string, float> dmat2x4ValueInvert;
    std::variant<glm::dmat3x2, std::string, float> dmat3x2ValueInvert;
    std::variant<glm::dmat3x3, std::string, float> dmat3x3ValueInvert;
    std::variant<glm::dmat3, std::string, float> dmat3ValueInvert;
    std::variant<glm::dmat3x4, std::string, float> dmat3x4ValueInvert;
    std::variant<glm::dmat4x2, std::string, float> dmat4x2ValueInvert;
    std::variant<glm::dmat4x3, std::string, float> dmat4x3ValueInvert;
    std::variant<glm::dmat4x4, std::string, float> dmat4x4ValueInvert;
    std::variant<glm::dmat4, std::string, float> dmat4ValueInvert;
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
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 76);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "boolVariable");
        CHECK(var->key == "\"BoolVariable\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "intVariable");
        CHECK(var->key == "\"IntVariable\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, int>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "doubleValue");
        CHECK(var->key == "\"DoubleValue\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "floatValue");
        CHECK(var->key == "\"FloatValue\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, double, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "stringValue");
        CHECK(var->key == "\"StringValue\"");
        CHECK(generateTypename(var->type) == "std::variant<int, float, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "ivec2Value");
        CHECK(var->key == "\"Ivec2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::ivec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "ivec3Value");
        CHECK(var->key == "\"Ivec3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::ivec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "ivec4Value");
        CHECK(var->key == "\"Ivec4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::ivec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "dvec2Value");
        CHECK(var->key == "\"Dvec2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dvec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "dvec3Value");
        CHECK(var->key == "\"Dvec3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dvec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "dvec4Value");
        CHECK(var->key == "\"Dvec4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dvec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "vec2Value");
        CHECK(var->key == "\"Vec2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::vec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "vec3Value");
        CHECK(var->key == "\"Vec3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::vec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "vec4Value");
        CHECK(var->key == "\"Vec4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::vec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "mat2x2Value");
        CHECK(var->key == "\"Mat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "mat2Value");
        CHECK(var->key == "\"Mat2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "mat2x3Value");
        CHECK(var->key == "\"Mat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "mat2x4Value");
        CHECK(var->key == "\"Mat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "mat3x2Value");
        CHECK(var->key == "\"Mat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "mat3x3Value");
        CHECK(var->key == "\"Mat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "mat3Value");
        CHECK(var->key == "\"Mat3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "mat3x4Value");
        CHECK(var->key == "\"Mat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "mat4x2Value");
        CHECK(var->key == "\"Mat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "mat4x3Value");
        CHECK(var->key == "\"Mat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "mat4x4Value");
        CHECK(var->key == "\"Mat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "mat4Value");
        CHECK(var->key == "\"Mat4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2Value");
        CHECK(var->key == "\"Dmat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "dmat2Value");
        CHECK(var->key == "\"Dmat2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3Value");
        CHECK(var->key == "\"Dmat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4Value");
        CHECK(var->key == "\"Dmat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2Value");
        CHECK(var->key == "\"Dmat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3Value");
        CHECK(var->key == "\"Dmat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "dmat3Value");
        CHECK(var->key == "\"Dmat3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4Value");
        CHECK(var->key == "\"Dmat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2Value");
        CHECK(var->key == "\"Dmat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3Value");
        CHECK(var->key == "\"Dmat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4Value");
        CHECK(var->key == "\"Dmat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "dmat4Value");
        CHECK(var->key == "\"Dmat4Value\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, float, glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[38];
        REQUIRE(var);
        CHECK(var->name == "boolVariableInvert");
        CHECK(var->key == "\"BoolVariableInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<bool, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[39];
        REQUIRE(var);
        CHECK(var->name == "intVariableInvert");
        CHECK(var->key == "\"IntVariableInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<int, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[40];
        REQUIRE(var);
        CHECK(var->name == "doubleValueInvert");
        CHECK(var->key == "\"DoubleValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<double, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[41];
        REQUIRE(var);
        CHECK(var->name == "floatValueInvert");
        CHECK(var->key == "\"FloatValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<float, std::string, double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[42];
        REQUIRE(var);
        CHECK(var->name == "stringValueInvert");
        CHECK(var->key == "\"StringValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<std::string, int, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[43];
        REQUIRE(var);
        CHECK(var->name == "ivec2ValueInvert");
        CHECK(var->key == "\"Ivec2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::ivec2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[44];
        REQUIRE(var);
        CHECK(var->name == "ivec3ValueInvert");
        CHECK(var->key == "\"Ivec3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::ivec3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[45];
        REQUIRE(var);
        CHECK(var->name == "ivec4ValueInvert");
        CHECK(var->key == "\"Ivec4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::ivec4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[46];
        REQUIRE(var);
        CHECK(var->name == "dvec2ValueInvert");
        CHECK(var->key == "\"Dvec2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dvec2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[47];
        REQUIRE(var);
        CHECK(var->name == "dvec3ValueInvert");
        CHECK(var->key == "\"Dvec3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dvec3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[48];
        REQUIRE(var);
        CHECK(var->name == "dvec4ValueInvert");
        CHECK(var->key == "\"Dvec4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dvec4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[49];
        REQUIRE(var);
        CHECK(var->name == "vec2ValueInvert");
        CHECK(var->key == "\"Vec2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[50];
        REQUIRE(var);
        CHECK(var->name == "vec3ValueInvert");
        CHECK(var->key == "\"Vec3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[51];
        REQUIRE(var);
        CHECK(var->name == "vec4ValueInvert");
        CHECK(var->key == "\"Vec4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[52];
        REQUIRE(var);
        CHECK(var->name == "mat2x2ValueInvert");
        CHECK(var->key == "\"Mat2x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[53];
        REQUIRE(var);
        CHECK(var->name == "mat2ValueInvert");
        CHECK(var->key == "\"Mat2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[54];
        REQUIRE(var);
        CHECK(var->name == "mat2x3ValueInvert");
        CHECK(var->key == "\"Mat2x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat2x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[55];
        REQUIRE(var);
        CHECK(var->name == "mat2x4ValueInvert");
        CHECK(var->key == "\"Mat2x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat2x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[56];
        REQUIRE(var);
        CHECK(var->name == "mat3x2ValueInvert");
        CHECK(var->key == "\"Mat3x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat3x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[57];
        REQUIRE(var);
        CHECK(var->name == "mat3x3ValueInvert");
        CHECK(var->key == "\"Mat3x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[58];
        REQUIRE(var);
        CHECK(var->name == "mat3ValueInvert");
        CHECK(var->key == "\"Mat3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[59];
        REQUIRE(var);
        CHECK(var->name == "mat3x4ValueInvert");
        CHECK(var->key == "\"Mat3x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat3x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[60];
        REQUIRE(var);
        CHECK(var->name == "mat4x2ValueInvert");
        CHECK(var->key == "\"Mat4x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat4x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[61];
        REQUIRE(var);
        CHECK(var->name == "mat4x3ValueInvert");
        CHECK(var->key == "\"Mat4x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat4x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[62];
        REQUIRE(var);
        CHECK(var->name == "mat4x4ValueInvert");
        CHECK(var->key == "\"Mat4x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[63];
        REQUIRE(var);
        CHECK(var->name == "mat4ValueInvert");
        CHECK(var->key == "\"Mat4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::mat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[64];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2ValueInvert");
        CHECK(var->key == "\"Dmat2x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[65];
        REQUIRE(var);
        CHECK(var->name == "dmat2ValueInvert");
        CHECK(var->key == "\"Dmat2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[66];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3ValueInvert");
        CHECK(var->key == "\"Dmat2x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat2x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[67];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4ValueInvert");
        CHECK(var->key == "\"Dmat2x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat2x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[68];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2ValueInvert");
        CHECK(var->key == "\"Dmat3x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat3x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[69];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3ValueInvert");
        CHECK(var->key == "\"Dmat3x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[70];
        REQUIRE(var);
        CHECK(var->name == "dmat3ValueInvert");
        CHECK(var->key == "\"Dmat3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[71];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4ValueInvert");
        CHECK(var->key == "\"Dmat3x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat3x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[72];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2ValueInvert");
        CHECK(var->key == "\"Dmat4x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat4x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[73];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3ValueInvert");
        CHECK(var->key == "\"Dmat4x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat4x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[74];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4ValueInvert");
        CHECK(var->key == "\"Dmat4x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[75];
        REQUIRE(var);
        CHECK(var->name == "dmat4ValueInvert");
        CHECK(var->key == "\"Dmat4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::variant<glm::dmat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/Structs/Variable:  Tuple Types", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    std::tuple<std::string, float, bool> boolVariable;
    std::tuple<std::string, float, int> intVariable;
    std::tuple<std::string, float, double> doubleValue;
    std::tuple<std::string, float, float> floatValue;
    std::tuple<std::string, float, std::string> stringValue;
    std::tuple<std::string, float, glm::ivec2> ivec2Value;
    std::tuple<std::string, float, glm::ivec3> ivec3Value;
    std::tuple<std::string, float, glm::ivec4> ivec4Value;
    std::tuple<std::string, float, glm::dvec2> dvec2Value;
    std::tuple<std::string, float, glm::dvec3> dvec3Value;
    std::tuple<std::string, float, glm::dvec4> dvec4Value;
    std::tuple<std::string, float, glm::vec2> vec2Value;
    std::tuple<std::string, float, glm::vec3> vec3Value;
    std::tuple<std::string, float, glm::vec4> vec4Value;
    std::tuple<std::string, float, glm::mat2x2> mat2x2Value;
    std::tuple<std::string, float, glm::mat2> mat2Value;
    std::tuple<std::string, float, glm::mat2x3> mat2x3Value;
    std::tuple<std::string, float, glm::mat2x4> mat2x4Value;
    std::tuple<std::string, float, glm::mat3x2> mat3x2Value;
    std::tuple<std::string, float, glm::mat3x3> mat3x3Value;
    std::tuple<std::string, float, glm::mat3> mat3Value;
    std::tuple<std::string, float, glm::mat3x4> mat3x4Value;
    std::tuple<std::string, float, glm::mat4x2> mat4x2Value;
    std::tuple<std::string, float, glm::mat4x3> mat4x3Value;
    std::tuple<std::string, float, glm::mat4x4> mat4x4Value;
    std::tuple<std::string, float, glm::mat4> mat4Value;
    std::tuple<std::string, float, glm::dmat2x2> dmat2x2Value;
    std::tuple<std::string, float, glm::dmat2> dmat2Value;
    std::tuple<std::string, float, glm::dmat2x3> dmat2x3Value;
    std::tuple<std::string, float, glm::dmat2x4> dmat2x4Value;
    std::tuple<std::string, float, glm::dmat3x2> dmat3x2Value;
    std::tuple<std::string, float, glm::dmat3x3> dmat3x3Value;
    std::tuple<std::string, float, glm::dmat3> dmat3Value;
    std::tuple<std::string, float, glm::dmat3x4> dmat3x4Value;
    std::tuple<std::string, float, glm::dmat4x2> dmat4x2Value;
    std::tuple<std::string, float, glm::dmat4x3> dmat4x3Value;
    std::tuple<std::string, float, glm::dmat4x4> dmat4x4Value;
    std::tuple<std::string, float, glm::dmat4> dmat4Value;

    std::tuple<bool, std::string, float> boolVariableInvert;
    std::tuple<int, std::string, float> intVariableInvert;
    std::tuple<double, std::string, float> doubleValueInvert;
    std::tuple<float, std::string, float> floatValueInvert;
    std::tuple<std::string, std::string, float> stringValueInvert;
    std::tuple<glm::ivec2, std::string, float> ivec2ValueInvert;
    std::tuple<glm::ivec3, std::string, float> ivec3ValueInvert;
    std::tuple<glm::ivec4, std::string, float> ivec4ValueInvert;
    std::tuple<glm::dvec2, std::string, float> dvec2ValueInvert;
    std::tuple<glm::dvec3, std::string, float> dvec3ValueInvert;
    std::tuple<glm::dvec4, std::string, float> dvec4ValueInvert;
    std::tuple<glm::vec2, std::string, float> vec2ValueInvert;
    std::tuple<glm::vec3, std::string, float> vec3ValueInvert;
    std::tuple<glm::vec4, std::string, float> vec4ValueInvert;
    std::tuple<glm::mat2x2, std::string, float> mat2x2ValueInvert;
    std::tuple<glm::mat2, std::string, float> mat2ValueInvert;
    std::tuple<glm::mat2x3, std::string, float> mat2x3ValueInvert;
    std::tuple<glm::mat2x4, std::string, float> mat2x4ValueInvert;
    std::tuple<glm::mat3x2, std::string, float> mat3x2ValueInvert;
    std::tuple<glm::mat3x3, std::string, float> mat3x3ValueInvert;
    std::tuple<glm::mat3, std::string, float> mat3ValueInvert;
    std::tuple<glm::mat3x4, std::string, float> mat3x4ValueInvert;
    std::tuple<glm::mat4x2, std::string, float> mat4x2ValueInvert;
    std::tuple<glm::mat4x3, std::string, float> mat4x3ValueInvert;
    std::tuple<glm::mat4x4, std::string, float> mat4x4ValueInvert;
    std::tuple<glm::mat4, std::string, float> mat4ValueInvert;
    std::tuple<glm::dmat2x2, std::string, float> dmat2x2ValueInvert;
    std::tuple<glm::dmat2, std::string, float> dmat2ValueInvert;
    std::tuple<glm::dmat2x3, std::string, float> dmat2x3ValueInvert;
    std::tuple<glm::dmat2x4, std::string, float> dmat2x4ValueInvert;
    std::tuple<glm::dmat3x2, std::string, float> dmat3x2ValueInvert;
    std::tuple<glm::dmat3x3, std::string, float> dmat3x3ValueInvert;
    std::tuple<glm::dmat3, std::string, float> dmat3ValueInvert;
    std::tuple<glm::dmat3x4, std::string, float> dmat3x4ValueInvert;
    std::tuple<glm::dmat4x2, std::string, float> dmat4x2ValueInvert;
    std::tuple<glm::dmat4x3, std::string, float> dmat4x3ValueInvert;
    std::tuple<glm::dmat4x4, std::string, float> dmat4x4ValueInvert;
    std::tuple<glm::dmat4, std::string, float> dmat4ValueInvert;
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
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 76);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "boolVariable");
        CHECK(var->key == "\"BoolVariable\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "intVariable");
        CHECK(var->key == "\"IntVariable\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, int>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "doubleValue");
        CHECK(var->key == "\"DoubleValue\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "floatValue");
        CHECK(var->key == "\"FloatValue\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "stringValue");
        CHECK(var->key == "\"StringValue\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        CHECK(var->name == "ivec2Value");
        CHECK(var->key == "\"Ivec2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::ivec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "ivec3Value");
        CHECK(var->key == "\"Ivec3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::ivec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "ivec4Value");
        CHECK(var->key == "\"Ivec4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::ivec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "dvec2Value");
        CHECK(var->key == "\"Dvec2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dvec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "dvec3Value");
        CHECK(var->key == "\"Dvec3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dvec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "dvec4Value");
        CHECK(var->key == "\"Dvec4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dvec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "vec2Value");
        CHECK(var->key == "\"Vec2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::vec2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "vec3Value");
        CHECK(var->key == "\"Vec3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::vec3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "vec4Value");
        CHECK(var->key == "\"Vec4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::vec4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "mat2x2Value");
        CHECK(var->key == "\"Mat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "mat2Value");
        CHECK(var->key == "\"Mat2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "mat2x3Value");
        CHECK(var->key == "\"Mat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "mat2x4Value");
        CHECK(var->key == "\"Mat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "mat3x2Value");
        CHECK(var->key == "\"Mat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "mat3x3Value");
        CHECK(var->key == "\"Mat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "mat3Value");
        CHECK(var->key == "\"Mat3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "mat3x4Value");
        CHECK(var->key == "\"Mat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "mat4x2Value");
        CHECK(var->key == "\"Mat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "mat4x3Value");
        CHECK(var->key == "\"Mat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "mat4x4Value");
        CHECK(var->key == "\"Mat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "mat4Value");
        CHECK(var->key == "\"Mat4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::mat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2Value");
        CHECK(var->key == "\"Dmat2x2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "dmat2Value");
        CHECK(var->key == "\"Dmat2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat2x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3Value");
        CHECK(var->key == "\"Dmat2x3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat2x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4Value");
        CHECK(var->key == "\"Dmat2x4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat2x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2Value");
        CHECK(var->key == "\"Dmat3x2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat3x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3Value");
        CHECK(var->key == "\"Dmat3x3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "dmat3Value");
        CHECK(var->key == "\"Dmat3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat3x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4Value");
        CHECK(var->key == "\"Dmat3x4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat3x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2Value");
        CHECK(var->key == "\"Dmat4x2Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat4x2>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3Value");
        CHECK(var->key == "\"Dmat4x3Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat4x3>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4Value");
        CHECK(var->key == "\"Dmat4x4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "dmat4Value");
        CHECK(var->key == "\"Dmat4Value\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, float, glm::dmat4x4>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[38];
        REQUIRE(var);
        CHECK(var->name == "boolVariableInvert");
        CHECK(var->key == "\"BoolVariableInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<bool, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[39];
        REQUIRE(var);
        CHECK(var->name == "intVariableInvert");
        CHECK(var->key == "\"IntVariableInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<int, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[40];
        REQUIRE(var);
        CHECK(var->name == "doubleValueInvert");
        CHECK(var->key == "\"DoubleValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<double, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[41];
        REQUIRE(var);
        CHECK(var->name == "floatValueInvert");
        CHECK(var->key == "\"FloatValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<float, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[42];
        REQUIRE(var);
        CHECK(var->name == "stringValueInvert");
        CHECK(var->key == "\"StringValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<std::string, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[43];
        REQUIRE(var);
        CHECK(var->name == "ivec2ValueInvert");
        CHECK(var->key == "\"Ivec2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::ivec2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[44];
        REQUIRE(var);
        CHECK(var->name == "ivec3ValueInvert");
        CHECK(var->key == "\"Ivec3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::ivec3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[45];
        REQUIRE(var);
        CHECK(var->name == "ivec4ValueInvert");
        CHECK(var->key == "\"Ivec4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::ivec4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[46];
        REQUIRE(var);
        CHECK(var->name == "dvec2ValueInvert");
        CHECK(var->key == "\"Dvec2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dvec2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[47];
        REQUIRE(var);
        CHECK(var->name == "dvec3ValueInvert");
        CHECK(var->key == "\"Dvec3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dvec3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[48];
        REQUIRE(var);
        CHECK(var->name == "dvec4ValueInvert");
        CHECK(var->key == "\"Dvec4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dvec4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[49];
        REQUIRE(var);
        CHECK(var->name == "vec2ValueInvert");
        CHECK(var->key == "\"Vec2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::vec2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[50];
        REQUIRE(var);
        CHECK(var->name == "vec3ValueInvert");
        CHECK(var->key == "\"Vec3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::vec3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[51];
        REQUIRE(var);
        CHECK(var->name == "vec4ValueInvert");
        CHECK(var->key == "\"Vec4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::vec4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[52];
        REQUIRE(var);
        CHECK(var->name == "mat2x2ValueInvert");
        CHECK(var->key == "\"Mat2x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[53];
        REQUIRE(var);
        CHECK(var->name == "mat2ValueInvert");
        CHECK(var->key == "\"Mat2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[54];
        REQUIRE(var);
        CHECK(var->name == "mat2x3ValueInvert");
        CHECK(var->key == "\"Mat2x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[55];
        REQUIRE(var);
        CHECK(var->name == "mat2x4ValueInvert");
        CHECK(var->key == "\"Mat2x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[56];
        REQUIRE(var);
        CHECK(var->name == "mat3x2ValueInvert");
        CHECK(var->key == "\"Mat3x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat3x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[57];
        REQUIRE(var);
        CHECK(var->name == "mat3x3ValueInvert");
        CHECK(var->key == "\"Mat3x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[58];
        REQUIRE(var);
        CHECK(var->name == "mat3ValueInvert");
        CHECK(var->key == "\"Mat3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[59];
        REQUIRE(var);
        CHECK(var->name == "mat3x4ValueInvert");
        CHECK(var->key == "\"Mat3x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat3x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[60];
        REQUIRE(var);
        CHECK(var->name == "mat4x2ValueInvert");
        CHECK(var->key == "\"Mat4x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat4x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[61];
        REQUIRE(var);
        CHECK(var->name == "mat4x3ValueInvert");
        CHECK(var->key == "\"Mat4x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat4x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[62];
        REQUIRE(var);
        CHECK(var->name == "mat4x4ValueInvert");
        CHECK(var->key == "\"Mat4x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[63];
        REQUIRE(var);
        CHECK(var->name == "mat4ValueInvert");
        CHECK(var->key == "\"Mat4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[64];
        REQUIRE(var);
        CHECK(var->name == "dmat2x2ValueInvert");
        CHECK(var->key == "\"Dmat2x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[65];
        REQUIRE(var);
        CHECK(var->name == "dmat2ValueInvert");
        CHECK(var->key == "\"Dmat2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat2x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[66];
        REQUIRE(var);
        CHECK(var->name == "dmat2x3ValueInvert");
        CHECK(var->key == "\"Dmat2x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat2x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[67];
        REQUIRE(var);
        CHECK(var->name == "dmat2x4ValueInvert");
        CHECK(var->key == "\"Dmat2x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat2x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[68];
        REQUIRE(var);
        CHECK(var->name == "dmat3x2ValueInvert");
        CHECK(var->key == "\"Dmat3x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat3x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[69];
        REQUIRE(var);
        CHECK(var->name == "dmat3x3ValueInvert");
        CHECK(var->key == "\"Dmat3x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[70];
        REQUIRE(var);
        CHECK(var->name == "dmat3ValueInvert");
        CHECK(var->key == "\"Dmat3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat3x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[71];
        REQUIRE(var);
        CHECK(var->name == "dmat3x4ValueInvert");
        CHECK(var->key == "\"Dmat3x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat3x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[72];
        REQUIRE(var);
        CHECK(var->name == "dmat4x2ValueInvert");
        CHECK(var->key == "\"Dmat4x2ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat4x2, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[73];
        REQUIRE(var);
        CHECK(var->name == "dmat4x3ValueInvert");
        CHECK(var->key == "\"Dmat4x3ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat4x3, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[74];
        REQUIRE(var);
        CHECK(var->name == "dmat4x4ValueInvert");
        CHECK(var->key == "\"Dmat4x4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
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
        Variable* var = s->variables[75];
        REQUIRE(var);
        CHECK(var->name == "dmat4ValueInvert");
        CHECK(var->key == "\"Dmat4ValueInvert\"");
        CHECK(generateTypename(var->type) == "std::tuple<glm::dmat4x4, std::string, float>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}


TEST_CASE("Parsing/Structs/Variable:  Variable attributes", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Name)]] Parameters {
    int variable1 [[codegen::key(Var)]];

};
)";
    Code code = parse(Source);
    CHECK(code.structs.size() == 1);
    CHECK(code.enums.empty());
    Struct* s = code.structs.front();
    REQUIRE(s);

    CHECK(s->name == "Parameters");
    CHECK(s->comment.empty());
    CHECK(s->attributes.dictionary == "Name");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());

    REQUIRE(s->variables.size() == 1);
    REQUIRE(s->variables[0]);
    CHECK(s->variables[0]->key == "Var");

    CHECK(s->variables[0]->attributes.annotation.empty());
    CHECK(s->variables[0]->attributes.greater.empty());
    CHECK(s->variables[0]->attributes.greaterequal.empty());
    CHECK(s->variables[0]->attributes.inlist.empty());
    CHECK(s->variables[0]->attributes.inrange.empty());
    CHECK(s->variables[0]->attributes.less.empty());
    CHECK(s->variables[0]->attributes.lessequal.empty());
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
