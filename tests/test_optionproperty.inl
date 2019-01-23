/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/properties/optionproperty.h>

class OptionPropertyTest : public testing::Test {};

TEST_F(OptionPropertyTest, NoOption) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    ASSERT_EQ(false, p.hasOption());
}

TEST_F(OptionPropertyTest, SingleOptionSingleZero) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc"});

    p.addOption(0, "a");

    p = 0;

    ASSERT_EQ(0, p.option().value);
    ASSERT_EQ("a", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionSingleNegative) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-1, "a");
    p = -1;

    ASSERT_EQ(-1, p.option().value);
    ASSERT_EQ("a", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionSinglePositive) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({
        { 1, "a" }
    });
    p = 1;

    ASSERT_EQ(1, p.option().value);
    ASSERT_EQ("a", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionMultipleZero) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({ "a" });
    p = 0;

    ASSERT_EQ(0, p.option().value);
    ASSERT_EQ("a", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionMultipleNegative) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({
        { -1, "a" }
    });
    p = -1;

    ASSERT_EQ(-1, p.option().value);
    ASSERT_EQ("a", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionMultiplePositive) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOptions({
        { 1, "a" }
    });
    p = 1;

    ASSERT_EQ(1, p.option().value);
    ASSERT_EQ("a", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionsZeroBasedConsecutive) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(0, "a");
    p.addOption(1, "b");
    p.addOption(2, "c");

    p = 0;
    ASSERT_EQ(0, p.option().value);
    ASSERT_EQ("a", p.option().description);

    p = 1;
    ASSERT_EQ(1, p.option().value);
    ASSERT_EQ("b", p.option().description);

    p = 2;
    ASSERT_EQ(2, p.option().value);
    ASSERT_EQ("c", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionsZeroBasedNonConsecutive) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(0, "a");
    p.addOption(2, "b");
    p.addOption(4, "c");

    p = 0;
    ASSERT_EQ(0, p.option().value);
    ASSERT_EQ("a", p.option().description);

    p = 2;
    ASSERT_EQ(2, p.option().value);
    ASSERT_EQ("b", p.option().description);

    p = 4;
    ASSERT_EQ(4, p.option().value);
    ASSERT_EQ("c", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionsNegativeBasedConsecutive) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-1, "a");
    p.addOption(-2, "b");
    p.addOption(-3, "c");

    p = -1;
    ASSERT_EQ(-1, p.option().value);
    ASSERT_EQ("a", p.option().description);

    p = -2;
    ASSERT_EQ(-2, p.option().value);
    ASSERT_EQ("b", p.option().description);

    p = -3;
    ASSERT_EQ(-3, p.option().value);
    ASSERT_EQ("c", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionsNonZeroBasedNonConsecutive) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-1, "a");
    p.addOption(-3, "b");
    p.addOption(-5, "c");

    p = -1;
    ASSERT_EQ(-1, p.option().value);
    ASSERT_EQ("a", p.option().description);

    p = -3;
    ASSERT_EQ(-3, p.option().value);
    ASSERT_EQ("b", p.option().description);

    p = -5;
    ASSERT_EQ(-5, p.option().value);
    ASSERT_EQ("c", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionsZeroBasedAlternating) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(0, "a");
    p.addOption(2, "b");
    p.addOption(-4, "c");

    p = 0;
    ASSERT_EQ(0, p.option().value);
    ASSERT_EQ("a", p.option().description);

    p = 2;
    ASSERT_EQ(2, p.option().value);
    ASSERT_EQ("b", p.option().description);

    p = -4;
    ASSERT_EQ(-4, p.option().value);
    ASSERT_EQ("c", p.option().description);
}

TEST_F(OptionPropertyTest, SingleOptionsNonZeroBasedAlternating) {
    openspace::properties::OptionProperty p({ "id", "gui", "desc" });

    p.addOption(-20, "a");
    p.addOption(2, "b");
    p.addOption(-10, "c");

    p = -20;
    ASSERT_EQ(-20, p.option().value);
    ASSERT_EQ("a", p.option().description);

    p = 2;
    ASSERT_EQ(2, p.option().value);
    ASSERT_EQ("b", p.option().description);

    p = -10;
    ASSERT_EQ(-10, p.option().value);
    ASSERT_EQ("c", p.option().description);
}
