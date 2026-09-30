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

TEST_CASE("Parsing/Structs/Attributes/String") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(Attributes)]] Parameters {
        // inListValue1 documentation
        std::string inListValue1 [[codegen::inlist("A", "B", "C")]];

        // inListValue1Optional documentation
        std::optional<std::string> inListValue1Optional
            [[codegen::inlist("A", "B", "C")]];

        // inListValue1Vector documentation
        std::vector<std::string> inListValue1Vector [[codegen::inlist("A", "B", "C")]];

        // inListValue2 documentation
        std::string inListValue2 [[codegen::inlist(List)]];

        // inListValue2Optional documentation
        std::optional<std::string> inListValue2Optional [[codegen::inlist(List)]];

        // inListValue2Vector documentation
        std::vector<std::string> inListValue2Vector [[codegen::inlist(List)]];

        // unequalValueString documentation
        std::string unequalValueString [[codegen::unequal("abcdef")]];

        // unequalValueStringOptional documentation
        std::optional<std::string> unequalValueStringOptional
            [[codegen::unequal("abcdef")]];

        // unequalValueStringVector documentation
        std::vector<std::string> unequalValueStringVector [[codegen::unequal("abcdef")]];

        // string not empty value documentation
        std::string notEmptyString [[codegen::notempty()]];

        // string not empty optional value documentation
        std::optional<std::string> notEmptyStringOptional [[codegen::notempty()]];

        // string not empty vector value documentation
        std::vector<std::string> notEmptyStringVector [[codegen::notempty()]];

        // string not empty optional vector value documentation
        std::optional<std::vector<std::string>> notEmptyStringOptionalVector
            [[codegen::notempty()]];

        // string not not empty value documentation
        std::string notNotEmptyString [[codegen::notempty(false)]];

        // string not not empty optional value documentation
        std::optional<std::string> notNotEmptyStringOptional [[codegen::notempty(false)]];

        // string not not empty vector value documentation
        std::vector<std::string> notNotEmptyStringVector [[codegen::notempty(false)]];

        // string not not empty optional vector value documentation
        std::optional<std::vector<std::string>> notNotEmptyStringOptionalVector
            [[codegen::notempty(false)]];

        // string yes not empty value documentation
        std::string yesNotEmptyString [[codegen::notempty(true)]];

        // string yes not empty optional value documentation
        std::optional<std::string> yesNotEmptyStringOptional [[codegen::notempty(true)]];

        // string yes not empty vector value documentation
        std::vector<std::string> yesNotEmptyStringVector [[codegen::notempty(true)]];

        // string yes not empty optional vector value documentation
        std::optional<std::vector<std::string>> yesNotEmptyStringOptionalVector
            [[codegen::notempty(true)]];

        // dateTime value documentation
        std::string dateTimeValue [[codegen::datetime()]];

        // optional dateTime value documentation
        std::optional<std::string> optionalDateTimeValue [[codegen::datetime()]];

        // vector dateTime value documentation
        std::vector<std::string> vectorDateTimeValue [[codegen::datetime()]];

        // optional vector dateTime value documentation
        std::optional<std::vector<std::string>> optionalVectorDateTimeValue
            [[codegen::datetime()]];

        // identifier value documentation
        std::string identifierValue [[codegen::identifier()]];

        // optional identifier value documentation
        std::optional<std::string> optionalIdentifierValue [[codegen::identifier()]];

        // vector identifier value documentation
        std::vector<std::string> vectorIdentifierValue [[codegen::identifier()]];

        // optional vector identifier value documentation
        std::optional<std::vector<std::string>> optionalVectorIdentifierValue
            [[codegen::identifier()]];

        // inListValue1 Private documentation
        std::string inListValue1Private [[codegen::inlist("A", "B", "C"), codegen::private()]];

        // inListValue1Optional Private documentation
        std::optional<std::string> inListValue1OptionalPrivate
            [[codegen::inlist("A", "B", "C"), codegen::private()]];

        // inListValue1Vector Private documentation
        std::vector<std::string> inListValue1VectorPrivate [[codegen::inlist("A", "B", "C"), codegen::private()]];

        // inListValue2 Private documentation
        std::string inListValue2Private [[codegen::inlist(List), codegen::private()]];

        // inListValue2Optional Private documentation
        std::optional<std::string> inListValue2OptionalPrivate [[codegen::inlist(List), codegen::private()]];

        // inListValue2Vector Private documentation
        std::vector<std::string> inListValue2VectorPrivate [[codegen::inlist(List), codegen::private()]];

        // unequalValueString Private documentation
        std::string unequalValueStringPrivate [[codegen::unequal("abcdef"), codegen::private()]];

        // unequalValueStringOptional Private documentation
        std::optional<std::string> unequalValueStringOptionalPrivate
            [[codegen::unequal("abcdef"), codegen::private()]];

        // unequalValueStringVector Private documentation
        std::vector<std::string> unequalValueStringVectorPrivate [[codegen::unequal("abcdef"), codegen::private()]];

        // string not empty value Private documentation
        std::string notEmptyStringPrivate [[codegen::notempty(), codegen::private()]];

        // string not empty optional value Private documentation
        std::optional<std::string> notEmptyStringOptionalPrivate [[codegen::notempty(), codegen::private()]];

        // string not empty vector value Private documentation
        std::vector<std::string> notEmptyStringVectorPrivate [[codegen::notempty(), codegen::private()]];

        // string not empty optional vector value Private documentation
        std::optional<std::vector<std::string>> notEmptyStringOptionalVectorPrivate
            [[codegen::notempty(), codegen::private()]];

        // string not not empty value Private documentation
        std::string notNotEmptyStringPrivate [[codegen::notempty(false), codegen::private()]];

        // string not not empty optional value Private documentation
        std::optional<std::string> notNotEmptyStringOptionalPrivate [[codegen::notempty(false), codegen::private()]];

        // string not not empty vector value Private documentation
        std::vector<std::string> notNotEmptyStringVectorPrivate [[codegen::notempty(false), codegen::private()]];

        // string not not empty optional vector value Private documentation
        std::optional<std::vector<std::string>> notNotEmptyStringOptionalVectorPrivate
            [[codegen::notempty(false), codegen::private()]];

        // string yes not empty value Private documentation
        std::string yesNotEmptyStringPrivate [[codegen::notempty(true), codegen::private()]];

        // string yes not empty optional value Private documentation
        std::optional<std::string> yesNotEmptyStringOptionalPrivate [[codegen::notempty(true), codegen::private()]];

        // string yes not empty vector value Private documentation
        std::vector<std::string> yesNotEmptyStringVectorPrivate [[codegen::notempty(true), codegen::private()]];

        // string yes not empty optional vector value Private documentation
        std::optional<std::vector<std::string>> yesNotEmptyStringOptionalVectorPrivate
            [[codegen::notempty(true), codegen::private()]];

        // dateTime value Private documentation
        std::string dateTimeValuePrivate [[codegen::datetime(), codegen::private()]];

        // optional dateTime value Private documentation
        std::optional<std::string> optionalDateTimeValuePrivate [[codegen::datetime(), codegen::private()]];

        // vector dateTime value Private documentation
        std::vector<std::string> vectorDateTimeValuePrivate [[codegen::datetime(), codegen::private()]];

        // optional vector dateTime value Private documentation
        std::optional<std::vector<std::string>> optionalVectorDateTimeValuePrivate
            [[codegen::datetime(), codegen::private()]];

        // identifier value Private documentation
        std::string identifierValuePrivate [[codegen::identifier(), codegen::private()]];

        // optional identifier value Private documentation
        std::optional<std::string> optionalIdentifierValuePrivate [[codegen::identifier(), codegen::private()]];

        // vector identifier value Private documentation
        std::vector<std::string> vectorIdentifierValuePrivate [[codegen::identifier(), codegen::private()]];

        // optional vector identifier value Private documentation
        std::optional<std::vector<std::string>> optionalVectorIdentifierValuePrivate
            [[codegen::identifier(), codegen::private()]];
})";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    CHECK(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());
    Struct* s = code.structs.front();
    REQUIRE(s);

    CHECK(s->name == "Parameters");
    CHECK(s->comment.empty());
    CHECK(s->attributes.dictionary == "Attributes");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 58);

    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "inListValue1");
        CHECK(var->key == "\"InListValue1\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "inListValue1 documentation");
        CHECK(var->attributes.inlist == "\"A\", \"B\", \"C\"");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->name == "inListValue1Optional");
        CHECK(var->key == "\"InListValue1Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "inListValue1Optional documentation");
        CHECK(var->attributes.inlist == "\"A\", \"B\", \"C\"");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->name == "inListValue1Vector");
        CHECK(var->key == "\"InListValue1Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "inListValue1Vector documentation");
        CHECK(var->attributes.inlist == "\"A\", \"B\", \"C\"");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->name == "inListValue2");
        CHECK(var->key == "\"InListValue2\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "inListValue2 documentation");
        CHECK(var->attributes.inlist == "List");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->name == "inListValue2Optional");
        CHECK(var->key == "\"InListValue2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "inListValue2Optional documentation");
        CHECK(var->attributes.inlist == "List");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->name == "inListValue2Vector");
        CHECK(var->key == "\"InListValue2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "inListValue2Vector documentation");
        CHECK(var->attributes.inlist == "List");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "unequalValueString");
        CHECK(var->key == "\"UnequalValueString\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "unequalValueString documentation");
        CHECK(var->attributes.unequal == "\"abcdef\"");

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
        CHECK(var->name == "unequalValueStringOptional");
        CHECK(var->key == "\"UnequalValueStringOptional\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "unequalValueStringOptional documentation");
        CHECK(var->attributes.unequal == "\"abcdef\"");

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
        CHECK(var->name == "unequalValueStringVector");
        CHECK(var->key == "\"UnequalValueStringVector\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "unequalValueStringVector documentation");
        CHECK(var->attributes.unequal == "\"abcdef\"");

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
        CHECK(var->name == "notEmptyString");
        CHECK(var->key == "\"NotEmptyString\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "string not empty value documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "notEmptyStringOptional");
        CHECK(var->key == "\"NotEmptyStringOptional\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "string not empty optional value documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "notEmptyStringVector");
        CHECK(var->key == "\"NotEmptyStringVector\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "string not empty vector value documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "notEmptyStringOptionalVector");
        CHECK(var->key == "\"NotEmptyStringOptionalVector\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "string not empty optional vector value documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyString");
        CHECK(var->key == "\"NotNotEmptyString\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "string not not empty value documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyStringOptional");
        CHECK(var->key == "\"NotNotEmptyStringOptional\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "string not not empty optional value documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyStringVector");
        CHECK(var->key == "\"NotNotEmptyStringVector\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "string not not empty vector value documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyStringOptionalVector");
        CHECK(var->key == "\"NotNotEmptyStringOptionalVector\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "string not not empty optional vector value documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyString");
        CHECK(var->key == "\"YesNotEmptyString\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "string yes not empty value documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyStringOptional");
        CHECK(var->key == "\"YesNotEmptyStringOptional\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "string yes not empty optional value documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyStringVector");
        CHECK(var->key == "\"YesNotEmptyStringVector\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "string yes not empty vector value documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyStringOptionalVector");
        CHECK(var->key == "\"YesNotEmptyStringOptionalVector\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "string yes not empty optional vector value documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "dateTimeValue");
        CHECK(var->key == "\"DateTimeValue\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "dateTime value documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "optionalDateTimeValue");
        CHECK(var->key == "\"OptionalDateTimeValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "optional dateTime value documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "vectorDateTimeValue");
        CHECK(var->key == "\"VectorDateTimeValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "vector dateTime value documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorDateTimeValue");
        CHECK(var->key == "\"OptionalVectorDateTimeValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "optional vector dateTime value documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "identifierValue");
        CHECK(var->key == "\"IdentifierValue\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "identifier value documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "optionalIdentifierValue");
        CHECK(var->key == "\"OptionalIdentifierValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "optional identifier value documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "vectorIdentifierValue");
        CHECK(var->key == "\"VectorIdentifierValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "vector identifier value documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorIdentifierValue");
        CHECK(var->key == "\"OptionalVectorIdentifierValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "optional vector identifier value documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "inListValue1Private");
        CHECK(var->key == "\"InListValue1Private\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "inListValue1 Private documentation");
        CHECK(var->attributes.inlist == "\"A\", \"B\", \"C\"");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "inListValue1OptionalPrivate");
        CHECK(var->key == "\"InListValue1OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "inListValue1Optional Private documentation");
        CHECK(var->attributes.inlist == "\"A\", \"B\", \"C\"");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "inListValue1VectorPrivate");
        CHECK(var->key == "\"InListValue1VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "inListValue1Vector Private documentation");
        CHECK(var->attributes.inlist == "\"A\", \"B\", \"C\"");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "inListValue2Private");
        CHECK(var->key == "\"InListValue2Private\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "inListValue2 Private documentation");
        CHECK(var->attributes.inlist == "List");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "inListValue2OptionalPrivate");
        CHECK(var->key == "\"InListValue2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "inListValue2Optional Private documentation");
        CHECK(var->attributes.inlist == "List");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "inListValue2VectorPrivate");
        CHECK(var->key == "\"InListValue2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "inListValue2Vector Private documentation");
        CHECK(var->attributes.inlist == "List");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "unequalValueStringPrivate");
        CHECK(var->key == "\"UnequalValueStringPrivate\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "unequalValueString Private documentation");
        CHECK(var->attributes.unequal == "\"abcdef\"");

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
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "unequalValueStringOptionalPrivate");
        CHECK(var->key == "\"UnequalValueStringOptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "unequalValueStringOptional Private documentation");
        CHECK(var->attributes.unequal == "\"abcdef\"");

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
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "unequalValueStringVectorPrivate");
        CHECK(var->key == "\"UnequalValueStringVectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "unequalValueStringVector Private documentation");
        CHECK(var->attributes.unequal == "\"abcdef\"");

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
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[38];
        REQUIRE(var);
        CHECK(var->name == "notEmptyStringPrivate");
        CHECK(var->key == "\"NotEmptyStringPrivate\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "string not empty value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[39];
        REQUIRE(var);
        CHECK(var->name == "notEmptyStringOptionalPrivate");
        CHECK(var->key == "\"NotEmptyStringOptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "string not empty optional value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[40];
        REQUIRE(var);
        CHECK(var->name == "notEmptyStringVectorPrivate");
        CHECK(var->key == "\"NotEmptyStringVectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "string not empty vector value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[41];
        REQUIRE(var);
        CHECK(var->name == "notEmptyStringOptionalVectorPrivate");
        CHECK(var->key == "\"NotEmptyStringOptionalVectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "string not empty optional vector value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty == true);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[42];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyStringPrivate");
        CHECK(var->key == "\"NotNotEmptyStringPrivate\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "string not not empty value Private documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[43];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyStringOptionalPrivate");
        CHECK(var->key == "\"NotNotEmptyStringOptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "string not not empty optional value Private documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[44];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyStringVectorPrivate");
        CHECK(var->key == "\"NotNotEmptyStringVectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "string not not empty vector value Private documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[45];
        REQUIRE(var);
        CHECK(var->name == "notNotEmptyStringOptionalVectorPrivate");
        CHECK(var->key == "\"NotNotEmptyStringOptionalVectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "string not not empty optional vector value Private documentation");
        CHECK(!var->attributes.mustBeNotEmpty);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[46];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyStringPrivate");
        CHECK(var->key == "\"YesNotEmptyStringPrivate\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "string yes not empty value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[47];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyStringOptionalPrivate");
        CHECK(var->key == "\"YesNotEmptyStringOptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "string yes not empty optional value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[48];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyStringVectorPrivate");
        CHECK(var->key == "\"YesNotEmptyStringVectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "string yes not empty vector value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[49];
        REQUIRE(var);
        CHECK(var->name == "yesNotEmptyStringOptionalVectorPrivate");
        CHECK(var->key == "\"YesNotEmptyStringOptionalVectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "string yes not empty optional vector value Private documentation");
        CHECK(var->attributes.mustBeNotEmpty);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[50];
        REQUIRE(var);
        CHECK(var->name == "dateTimeValuePrivate");
        CHECK(var->key == "\"DateTimeValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "dateTime value Private documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[51];
        REQUIRE(var);
        CHECK(var->name == "optionalDateTimeValuePrivate");
        CHECK(var->key == "\"OptionalDateTimeValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "optional dateTime value Private documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[52];
        REQUIRE(var);
        CHECK(var->name == "vectorDateTimeValuePrivate");
        CHECK(var->key == "\"VectorDateTimeValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "vector dateTime value Private documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[53];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorDateTimeValuePrivate");
        CHECK(var->key == "\"OptionalVectorDateTimeValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "optional vector dateTime value Private documentation");
        CHECK(var->attributes.isDateTime);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[54];
        REQUIRE(var);
        CHECK(var->name == "identifierValuePrivate");
        CHECK(var->key == "\"IdentifierValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "identifier value Private documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[55];
        REQUIRE(var);
        CHECK(var->name == "optionalIdentifierValuePrivate");
        CHECK(var->key == "\"OptionalIdentifierValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::string>");
        CHECK(var->comment == "optional identifier value Private documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[56];
        REQUIRE(var);
        CHECK(var->name == "vectorIdentifierValuePrivate");
        CHECK(var->key == "\"VectorIdentifierValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "vector identifier value Private documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[57];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorIdentifierValuePrivate");
        CHECK(var->key == "\"OptionalVectorIdentifierValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::string>>");
        CHECK(var->comment == "optional vector identifier value Private documentation");
        CHECK(var->attributes.isIdentifier);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
