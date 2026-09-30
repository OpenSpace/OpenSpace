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

TEST_CASE("Parsing/Structs/Attributes/Path") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(Attributes)]] Parameters {
        // file value documentation
        std::filesystem::path fileValue;

        // optional file value documentation
        std::optional<std::filesystem::path> optionalFileValue;

        // vector file value documentation
        std::vector<std::filesystem::path> vectorFileValue;

        // optional vector file value documentation
        std::optional<std::vector<std::filesystem::path>> optionalVectorFileValue;

        // directory value documentation
        std::filesystem::path directoryValue [[codegen::directory()]];

        // optional directory value documentation
        std::optional<std::filesystem::path> optionalDirectoryValue
            [[codegen::directory()]];

        // vector directory value documentation
        std::vector<std::filesystem::path> vectorDirectoryValue [[codegen::directory()]];

        // optional vector directory value documentation
        std::optional<std::vector<std::filesystem::path>> optionalVectorDirectoryValue
            [[codegen::directory()]];

        // not directory value documentation
        std::filesystem::path notDirectoryValue [[codegen::directory(false)]];

        // not optional directory value documentation
        std::optional<std::filesystem::path> notOptionalDirectoryValue
            [[codegen::directory(false)]];

        // not vector directory value documentation
        std::vector<std::filesystem::path> notVectorDirectoryValue
            [[codegen::directory(false)]];

        // not optional vector directory value documentation
        std::optional<std::vector<std::filesystem::path>> notOptionalVectorDirectoryValue
            [[codegen::directory(false)]];

        // yes directory value documentation
        std::filesystem::path yesDirectoryValue [[codegen::directory(true)]];

        // yes optional directory value documentation
        std::optional<std::filesystem::path> yesOptionalDirectoryValue
            [[codegen::directory(true)]];

        // yes vector directory value documentation
        std::vector<std::filesystem::path> yesVectorDirectoryValue
            [[codegen::directory(true)]];

        // yes optional vector directory value documentation
        std::optional<std::vector<std::filesystem::path>> yesOptionalVectorDirectoryValue
            [[codegen::directory(true)]];

        // file value Private documentation
        std::filesystem::path fileValuePrivate [[codegen::private()]];

        // optional file value Private documentation
        std::optional<std::filesystem::path> optionalFileValuePrivate [[codegen::private()]];

        // vector file value Private documentation
        std::vector<std::filesystem::path> vectorFileValuePrivate [[codegen::private()]];

        // optional vector file value Private documentation
        std::optional<std::vector<std::filesystem::path>> optionalVectorFileValuePrivate [[codegen::private()]];

        // directory value Private documentation
        std::filesystem::path directoryValuePrivate [[codegen::directory(), codegen::private()]];

        // optional directory value Private documentation
        std::optional<std::filesystem::path> optionalDirectoryValuePrivate
            [[codegen::directory(), codegen::private()]];

        // vector directory value Private documentation
        std::vector<std::filesystem::path> vectorDirectoryValuePrivate [[codegen::directory(), codegen::private()]];

        // optional vector directory value Private documentation
        std::optional<std::vector<std::filesystem::path>> optionalVectorDirectoryValuePrivate
            [[codegen::directory(), codegen::private()]];


        // not directory value Private documentation
        std::filesystem::path notDirectoryValuePrivate [[codegen::directory(false), codegen::private()]];

        // not optional directory value Private documentation
        std::optional<std::filesystem::path> notOptionalDirectoryValuePrivate
            [[codegen::directory(false), codegen::private()]];

        // not vector directory value Private documentation
        std::vector<std::filesystem::path> notVectorDirectoryValuePrivate
            [[codegen::directory(false), codegen::private()]];

        // not optional vector directory value Private documentation
        std::optional<std::vector<std::filesystem::path>> notOptionalVectorDirectoryValuePrivate 
            [[codegen::directory(false), codegen::private()]];


        // yes directory value Private documentation
        std::filesystem::path yesDirectoryValuePrivate  [[codegen::directory(true), codegen::private()]];

        // yes optional directory value Private documentation
        std::optional<std::filesystem::path> yesOptionalDirectoryValuePrivate
            [[codegen::directory(true), codegen::private()]];

        // yes vector directory value Private documentation
        std::vector<std::filesystem::path> yesVectorDirectoryValuePrivate
            [[codegen::directory(true), codegen::private()]];

        // yes optional vector directory value Private documentation
        std::optional<std::vector<std::filesystem::path>> yesOptionalVectorDirectoryValuePrivate
            [[codegen::directory(true), codegen::private()]];


        // default mustexist documentation
        std::filesystem::path defaultMustExistValue;

        // default optional mustexist documentation
        std::optional<std::filesystem::path> defaultOptionalMustExistValue;

        // default vector mustexist documentation
        std::vector<std::filesystem::path> defaultVectorMustExistValue;

        // default optionalvector mustexist documentation
        std::optional<std::vector<std::filesystem::path>> defaultOptionalVectorMustExistValue;


        // yes mustexist documentation
        std::filesystem::path yesMustExistValue [[codegen::mustexist(true)]];

        // yes optional mustexist documentation
        std::optional<std::filesystem::path> yesOptionalMustExistValue [[codegen::mustexist(true)]];

        // yes vector mustexist documentation
        std::vector<std::filesystem::path> yesVectorMustExistValue [[codegen::mustexist(true)]];

        // yes optionalvector mustexist documentation
        std::optional<std::vector<std::filesystem::path>> yesOptionalVectorMustExistValue [[codegen::mustexist(true)]];


        // no mustexist documentation
        std::filesystem::path noMustExistValue [[codegen::mustexist(false)]];

        // no optional mustexist documentation
        std::optional<std::filesystem::path> noOptionalMustExistValue [[codegen::mustexist(false)]];

        // no vector mustexist documentation
        std::vector<std::filesystem::path> noVectorMustExistValue [[codegen::mustexist(false)]];

        // no optionalvector mustexist documentation
        std::optional<std::vector<std::filesystem::path>> noOptionalVectorMustExistValue [[codegen::mustexist(false)]];


        // default mustexist dictionary documentation
        std::filesystem::path defaultMustExistDictionaryValue [[codegen::directory()]];

        // default optional mustexist dictionary documentation
        std::optional<std::filesystem::path> defaultOptionalMustExistDictionaryValue [[codegen::directory()]];

        // default vector mustexist dictionary documentation
        std::vector<std::filesystem::path> defaultVectorMustExistDictionaryValue [[codegen::directory()]];

        // default optionalvector mustexist dictionary documentation
        std::optional<std::vector<std::filesystem::path>> defaultOptionalVectorMustExistDictionaryValue [[codegen::directory()]];


        // yes mustexist dictionary documentation
        std::filesystem::path yesMustExistDictionaryValue [[codegen::mustexist(true), codegen::directory()]];

        // yes optional mustexist dictionary documentation
        std::optional<std::filesystem::path> yesOptionalMustExistDictionaryValue [[codegen::mustexist(true), codegen::directory()]];

        // yes vector mustexist dictionary documentation
        std::vector<std::filesystem::path> yesVectorMustExistDictionaryValue [[codegen::mustexist(true), codegen::directory()]];

        // yes optionalvector mustexist dictionary documentation
        std::optional<std::vector<std::filesystem::path>> yesOptionalVectorMustExistDictionaryValue [[codegen::mustexist(true), codegen::directory()]];


        // no mustexist dictionary documentation
        std::filesystem::path noMustExistDictionaryValue [[codegen::mustexist(false), codegen::directory()]];

        // no optional mustexist dictionary documentation
        std::optional<std::filesystem::path> noOptionalMustExistDictionaryValue [[codegen::mustexist(false), codegen::directory()]];

        // no vector mustexist dictionary documentation
        std::vector<std::filesystem::path> noVectorMustExistDictionaryValue [[codegen::mustexist(false), codegen::directory()]];

        // no optionalvector mustexist dictionary documentation
        std::optional<std::vector<std::filesystem::path>> noOptionalVectorMustExistDictionaryValue [[codegen::mustexist(false), codegen::directory()]];
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
    REQUIRE(s->variables.size() == 56);

    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "fileValue");
        CHECK(var->key == "\"FileValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "file value documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[1];
        REQUIRE(var);
        CHECK(var->name == "optionalFileValue");
        CHECK(var->key == "\"OptionalFileValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "optional file value documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[2];
        REQUIRE(var);
        CHECK(var->name == "vectorFileValue");
        CHECK(var->key == "\"VectorFileValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "vector file value documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[3];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorFileValue");
        CHECK(var->key == "\"OptionalVectorFileValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "optional vector file value documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[4];
        REQUIRE(var);
        CHECK(var->name == "directoryValue");
        CHECK(var->key == "\"DirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[5];
        REQUIRE(var);
        CHECK(var->name == "optionalDirectoryValue");
        CHECK(var->key == "\"OptionalDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "optional directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "vectorDirectoryValue");
        CHECK(var->key == "\"VectorDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "vector directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorDirectoryValue");
        CHECK(var->key == "\"OptionalVectorDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "optional vector directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "notDirectoryValue");
        CHECK(var->key == "\"NotDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "not directory value documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "notOptionalDirectoryValue");
        CHECK(var->key == "\"NotOptionalDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "not optional directory value documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "notVectorDirectoryValue");
        CHECK(var->key == "\"NotVectorDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "not vector directory value documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "notOptionalVectorDirectoryValue");
        CHECK(var->key == "\"NotOptionalVectorDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "not optional vector directory value documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "yesDirectoryValue");
        CHECK(var->key == "\"YesDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "yes directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalDirectoryValue");
        CHECK(var->key == "\"YesOptionalDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "yes optional directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "yesVectorDirectoryValue");
        CHECK(var->key == "\"YesVectorDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "yes vector directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalVectorDirectoryValue");
        CHECK(var->key == "\"YesOptionalVectorDirectoryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "yes optional vector directory value documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "fileValuePrivate");
        CHECK(var->key == "\"FileValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "file value Private documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "optionalFileValuePrivate");
        CHECK(var->key == "\"OptionalFileValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "optional file value Private documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "vectorFileValuePrivate");
        CHECK(var->key == "\"VectorFileValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "vector file value Private documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorFileValuePrivate");
        CHECK(var->key == "\"OptionalVectorFileValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "optional vector file value Private documentation");

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
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "directoryValuePrivate");
        CHECK(var->key == "\"DirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "optionalDirectoryValuePrivate");
        CHECK(var->key == "\"OptionalDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "optional directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "vectorDirectoryValuePrivate");
        CHECK(var->key == "\"VectorDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "vector directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "optionalVectorDirectoryValuePrivate");
        CHECK(var->key == "\"OptionalVectorDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "optional vector directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "notDirectoryValuePrivate");
        CHECK(var->key == "\"NotDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "not directory value Private documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "notOptionalDirectoryValuePrivate");
        CHECK(var->key == "\"NotOptionalDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "not optional directory value Private documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "notVectorDirectoryValuePrivate");
        CHECK(var->key == "\"NotVectorDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "not vector directory value Private documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "notOptionalVectorDirectoryValuePrivate");
        CHECK(var->key == "\"NotOptionalVectorDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "not optional vector directory value Private documentation");
        CHECK(!var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "yesDirectoryValuePrivate");
        CHECK(var->key == "\"YesDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "yes directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalDirectoryValuePrivate");
        CHECK(var->key == "\"YesOptionalDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "yes optional directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "yesVectorDirectoryValuePrivate");
        CHECK(var->key == "\"YesVectorDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "yes vector directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalVectorDirectoryValuePrivate");
        CHECK(var->key == "\"YesOptionalVectorDirectoryValuePrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "yes optional vector directory value Private documentation");
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "defaultMustExistValue");
        CHECK(var->key == "\"DefaultMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "default mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "defaultOptionalMustExistValue");
        CHECK(var->key == "\"DefaultOptionalMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "default optional mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "defaultVectorMustExistValue");
        CHECK(var->key == "\"DefaultVectorMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "default vector mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "defaultOptionalVectorMustExistValue");
        CHECK(var->key == "\"DefaultOptionalVectorMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "default optionalvector mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "yesMustExistValue");
        CHECK(var->key == "\"YesMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "yes mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalMustExistValue");
        CHECK(var->key == "\"YesOptionalMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "yes optional mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[38];
        REQUIRE(var);
        CHECK(var->name == "yesVectorMustExistValue");
        CHECK(var->key == "\"YesVectorMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "yes vector mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[39];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalVectorMustExistValue");
        CHECK(var->key == "\"YesOptionalVectorMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "yes optionalvector mustexist documentation");
        CHECK(var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[40];
        REQUIRE(var);
        CHECK(var->name == "noMustExistValue");
        CHECK(var->key == "\"NoMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "no mustexist documentation");
        CHECK(!var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[41];
        REQUIRE(var);
        CHECK(var->name == "noOptionalMustExistValue");
        CHECK(var->key == "\"NoOptionalMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "no optional mustexist documentation");
        CHECK(!var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[42];
        REQUIRE(var);
        CHECK(var->name == "noVectorMustExistValue");
        CHECK(var->key == "\"NoVectorMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "no vector mustexist documentation");
        CHECK(!var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[43];
        REQUIRE(var);
        CHECK(var->name == "noOptionalVectorMustExistValue");
        CHECK(var->key == "\"NoOptionalVectorMustExistValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "no optionalvector mustexist documentation");
        CHECK(!var->attributes.mustExist);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[44];
        REQUIRE(var);
        CHECK(var->name == "defaultMustExistDictionaryValue");
        CHECK(var->key == "\"DefaultMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "default mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[45];
        REQUIRE(var);
        CHECK(var->name == "defaultOptionalMustExistDictionaryValue");
        CHECK(var->key == "\"DefaultOptionalMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "default optional mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[46];
        REQUIRE(var);
        CHECK(var->name == "defaultVectorMustExistDictionaryValue");
        CHECK(var->key == "\"DefaultVectorMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "default vector mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[47];
        REQUIRE(var);
        CHECK(var->name == "defaultOptionalVectorMustExistDictionaryValue");
        CHECK(var->key == "\"DefaultOptionalVectorMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "default optionalvector mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[48];
        REQUIRE(var);
        CHECK(var->name == "yesMustExistDictionaryValue");
        CHECK(var->key == "\"YesMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "yes mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[49];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalMustExistDictionaryValue");
        CHECK(var->key == "\"YesOptionalMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "yes optional mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[50];
        REQUIRE(var);
        CHECK(var->name == "yesVectorMustExistDictionaryValue");
        CHECK(var->key == "\"YesVectorMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "yes vector mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[51];
        REQUIRE(var);
        CHECK(var->name == "yesOptionalVectorMustExistDictionaryValue");
        CHECK(var->key == "\"YesOptionalVectorMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "yes optionalvector mustexist dictionary documentation");
        CHECK(var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[52];
        REQUIRE(var);
        CHECK(var->name == "noMustExistDictionaryValue");
        CHECK(var->key == "\"NoMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::filesystem::path");
        CHECK(var->comment == "no mustexist dictionary documentation");
        CHECK(!var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[53];
        REQUIRE(var);
        CHECK(var->name == "noOptionalMustExistDictionaryValue");
        CHECK(var->key == "\"NoOptionalMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::filesystem::path>");
        CHECK(var->comment == "no optional mustexist dictionary documentation");
        CHECK(!var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[54];
        REQUIRE(var);
        CHECK(var->name == "noVectorMustExistDictionaryValue");
        CHECK(var->key == "\"NoVectorMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::vector<std::filesystem::path>");
        CHECK(var->comment == "no vector mustexist dictionary documentation");
        CHECK(!var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[55];
        REQUIRE(var);
        CHECK(var->name == "noOptionalVectorMustExistDictionaryValue");
        CHECK(var->key == "\"NoOptionalVectorMustExistDictionaryValue\"");
        CHECK(generateTypename(var->type) == "std::optional<std::vector<std::filesystem::path>>");
        CHECK(var->comment == "no optionalvector mustexist dictionary documentation");
        CHECK(!var->attributes.mustExist);
        CHECK(var->attributes.isDirectory);

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
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }


    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
