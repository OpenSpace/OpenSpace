/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include "catch2/catch.hpp"
#include "test_common.h"

#include "openspace/scene/profile.h"
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/exception.h>
#include <iostream>
#include <iomanip>

using namespace openspace;

namespace {
}

static void addLineHeaderForFailureMessage(std::string& s, size_t lineNumber) {
    s = "@line " + std::to_string(lineNumber) + ": '" + s + "'";
}

TEST_CASE("profile: Convert profileFile to asset", "[profile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFull_string = stringFromTestProfileFormat(test);

    ProfileFile pf = makeProfileFromString(testFull_string);

    Profile p;
    REQUIRE_NOTHROW(
        p.convertToScene(pf)
    );
}

TEST_CASE("profile: Verify conversion to scene", "[profile]") {
    ProfileFile pf = makeProfileFromString(newHorizonsProfileInput);

    Profile p;
    std::string result;
    REQUIRE_NOTHROW(
        result = p.convertToScene(pf)
    );

    std::string testing, comparing;
    StringPerLineReader sr_result(result);
    StringPerLineReader sr_standard(newHorizonsExpectedSceneOutput);

    size_t lineN = 1;
    while (sr_result.getNextLine(testing)) {
        sr_standard.getNextLine(comparing);
        addLineHeaderForFailureMessage(testing, lineN);
        addLineHeaderForFailureMessage(comparing, lineN);
        REQUIRE(testing == comparing);
        lineN++;
    }
    //If this fails there are extra lines in the comparison string that weren't in result
    REQUIRE(sr_standard.getNextLine(comparing) == false);
}

