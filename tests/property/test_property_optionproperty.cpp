/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/properties/misc/optionproperty.h>

TEST_CASE("OptionProperty: No Option", "[optionproperty]") {
    const openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    CHECK_FALSE(p.hasOption());
}

TEST_CASE("OptionProperty: Single Option Single Zero", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc"});

    p.addOption(0, "a");

    p = 0;

    CHECK(p.option().value == 0);
    CHECK(p.option().description == "a");
}

TEST_CASE("OptionProperty: Single Option Single Negative", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-1, "a");
    p = -1;

    CHECK(p.option().value == -1);
    CHECK(p.option().description == "a");
}

TEST_CASE("OptionProperty: Single Option Single Positive", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({ { 1, "a" } });
    p = 1;

    CHECK(p.option().value == 1);
    CHECK(p.option().description == "a");
}

TEST_CASE("OptionProperty: Single Option Multiple Zero", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({ "a" });
    p = 0;

    CHECK(p.option().value == 0);
    CHECK(p.option().description == "a");
}

TEST_CASE("OptionProperty: Single Option Multiple Negative", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({ { -1, "a" } });
    p = -1;

    CHECK(p.option().value == -1);
    CHECK(p.option().description == "a");
}

TEST_CASE("OptionProperty: Single Option Multiple Positive", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({ { 1, "a" } });
    p = 1;

    CHECK(p.option().value == 1);
    CHECK(p.option().description == "a");
}

TEST_CASE("OptionProperty: Single Options Zero Based Consecutive", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(0, "a");
    p.addOption(1, "b");
    p.addOption(2, "c");

    p = 0;
    CHECK(p.option().value == 0);
    CHECK(p.option().description == "a");

    p = 1;
    CHECK(p.option().value == 1);
    CHECK(p.option().description == "b");

    p = 2;
    CHECK(p.option().value == 2);
    CHECK(p.option().description == "c");
}

TEST_CASE("OptionProperty: Single Options Zero Based Non Consecutive", "[optionproperty]")
{
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(0, "a");
    p.addOption(2, "b");
    p.addOption(4, "c");

    p = 0;
    CHECK(p.option().value == 0);
    CHECK(p.option().description == "a");

    p = 2;
    CHECK(p.option().value == 2);
    CHECK(p.option().description == "b");

    p = 4;
    CHECK(p.option().value == 4);
    CHECK(p.option().description == "c");
}

TEST_CASE("OptionProperty: Single Options Negative Based Consecutive", "[optionproperty]")
{
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-1, "a");
    p.addOption(-2, "b");
    p.addOption(-3, "c");

    p = -1;
    CHECK(p.option().value == -1);
    CHECK(p.option().description == "a");

    p = -2;
    CHECK(p.option().value == -2);
    CHECK(p.option().description == "b");

    p = -3;
    CHECK(p.option().value == -3);
    CHECK(p.option().description == "c");
}

TEST_CASE(
    "OptionProperty: Single Options Non Zero Based Non Consecutive",
    "[optionproperty]")
{
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-1, "a");
    p.addOption(-3, "b");
    p.addOption(-5, "c");

    p = -1;
    CHECK(p.option().value == -1);
    CHECK(p.option().description == "a");

    p = -3;
    CHECK(p.option().value == -3);
    CHECK(p.option().description == "b");

    p = -5;
    CHECK(p.option().value == -5);
    CHECK(p.option().description == "c");
}

TEST_CASE("OptionProperty: Single Options Zero Based Alternating", "[optionproperty]") {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(0, "a");
    p.addOption(2, "b");
    p.addOption(-4, "c");

    p = 0;
    CHECK(p.option().value == 0);
    CHECK(p.option().description == "a");

    p = 2;
    CHECK(p.option().value == 2);
    CHECK(p.option().description == "b");

    p = -4;
    CHECK(p.option().value == -4);
    CHECK(p.option().description == "c");
}

TEST_CASE("OptionProperty: Single Options Non Zero Based Alternating", "[optionproperty]")
{
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-20, "a");
    p.addOption(2, "b");
    p.addOption(-10, "c");

    p = -20;
    CHECK(p.option().value == -20);
    CHECK(p.option().description == "a");

    p = 2;
    CHECK(p.option().value == 2);
    CHECK(p.option().description == "b");

    p = -10;
    CHECK(p.option().value == -10);
    CHECK(p.option().description == "c");
}
