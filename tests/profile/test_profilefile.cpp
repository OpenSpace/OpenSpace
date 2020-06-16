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

#include "openspace/scene/profilefile.h"
#include "test_common.h"
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/exception.h>
#include <iostream>
#include <iomanip>

using namespace openspace;

namespace {
}

TEST_CASE("profileFile: Simple read and verify", "[profileFile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFile = absPath("${TEMPORARY}/profile-test-simple");
    {
        std::string testFull_string = stringFromTestProfileFormat(test);
        std::ofstream f(testFile);
        f << testFull_string;
    }

    ProfileFile pf(testFile);

    std::vector<std::string> tVect;

    REQUIRE(pf.version() == test.tsv[1]);
    REQUIRE(pf.time() == test.tst[1]);
    REQUIRE(pf.camera() == test.tsc[1]);
    tVect = pf.modules();
    REQUIRE(tVect[0] == test.tsm[1]);
    REQUIRE(tVect[1] == test.tsm[2]);
    REQUIRE(tVect[2] == test.tsm[3]);
    tVect = pf.assets();
    REQUIRE(tVect[0] == test.tsa[1]);
    REQUIRE(tVect[1] == test.tsa[2]);
    REQUIRE(tVect[2] == test.tsa[3]);
    tVect = pf.properties();
    REQUIRE(tVect[0] == test.tsp[1]);
    REQUIRE(tVect[1] == test.tsp[2]);
    REQUIRE(tVect[2] == test.tsp[3]);
    REQUIRE(tVect[3] == test.tsp[4]);
    tVect = pf.keybindings();
    REQUIRE(tVect[0] == test.tsk[1]);
    REQUIRE(tVect[1] == test.tsk[2]);
    REQUIRE(tVect[2] == test.tsk[3]);
    REQUIRE(tVect[3] == test.tsk[4]);
    tVect = pf.markNodes();
    REQUIRE(tVect[0] == test.tsn[1]);
    REQUIRE(tVect[1] == test.tsn[2]);
    REQUIRE(tVect[2] == test.tsn[3]);
}

TEST_CASE("profileFile: Unrecognized header", "[profileFile]") {
    testProfileFormat test = buildTestProfile1();

    test.tsa[0] = "#Azzet";
    std::string testFull_string = stringFromTestProfileFormat(test);
    std::istringstream iss(testFull_string);

    ProfileFile pf("default.profile");
    REQUIRE_THROWS_WITH(
        pf.readIn([&iss](std::string& line) {
            if (getline(iss, line))
                return true;
            else
                return false;
            }
        ),
        Catch::Matchers::Contains ( "Invalid section header" )
            && Catch::Matchers::Contains( "#Azzet" )
    );
}

TEST_CASE("profileFile: Bad number of fields", "[profileFile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFull_string;
    test.tsm[1] = "globebrowsing\t\t\t";
    testFull_string = stringFromTestProfileFormat(test);
    {
        std::istringstream iss(testFull_string);
        ProfileFile pf("default.profile");
        REQUIRE_THROWS_WITH(
            pf.readIn([&iss](std::string& line) {
                if (getline(iss, line))
                    return true;
                else
                    return false;
                }
            ),
            Catch::Matchers::Contains ("fields required in a Module entry")
        );
    }

    test.tsm[1] = "globebrowsing\t\t";
    test.tsc[1] = "setNavigationState\t\"NewHorizons\"\t\"Root\"\t-6.572656E1, -7.239404E1, -2.111890E1\t0.102164, -0.362945, 0.926193\t\t";
    testFull_string = stringFromTestProfileFormat(test);
    {
        std::istringstream iss(testFull_string);
        ProfileFile pf("default.profile");
        REQUIRE_THROWS_WITH(
            pf.readIn([&iss](std::string& line) {
                if (getline(iss, line))
                    return true;
                else
                    return false;
                }
            ),
            Catch::Matchers::Contains ("fields required in Camera entry")
        );
    }
}

TEST_CASE("profileFile: Too many lines in time entry", "[profileFile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFull_string;
    test.tst.push_back("relative\t\"-1 day\"");
    testFull_string = stringFromTestProfileFormat(test);
    {
        std::istringstream iss(testFull_string);
        ProfileFile pf("default.profile");
        REQUIRE_THROWS_WITH(
            pf.readIn([&iss](std::string& line) {
                if (getline(iss, line))
                    return true;
                else
                    return false;
                }
            ),
            Catch::Matchers::Contains ("Too many lines in time section")
        );
    }
}

TEST_CASE("profileFile: Required field missing", "[profileFile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFull_string;
    test.tsc[1] = "setNavigationState\t\"NewHorizons\"\ttest\t\"Root\"\t\t0.102164, -0.362945, 0.926193\t\t";

    testFull_string = stringFromTestProfileFormat(test);
    {
        std::istringstream iss(testFull_string);
        ProfileFile pf("default.profile");
        REQUIRE_THROWS_WITH(
            pf.readIn([&iss](std::string& line) {
                if (getline(iss, line))
                    return true;
                else
                    return false;
                }
            ),
            Catch::Matchers::Contains ("Camera navigation setNavigationState position vector(arg 4/8) is required")
        );
    }

    test.tsc[1] = "setNavigationState\t\"NewHorizons\"\t\t\"Root\"\t1, 2, 3\t0.102164, -0.362945, 0.926193\t\t";
    test.tsk[3] = "F10\tSets the time to the orbital B event.\tSet orbital B event time\t/Missions/Osiris Rex\t\t\"openspace.printInfo('Set time: Orbital B'); openspace.time.setTime('2019-APR-08 10:35:27.186')\"";
    testFull_string = stringFromTestProfileFormat(test);
    {
        std::istringstream iss(testFull_string);
        ProfileFile pf("default.profile");
        REQUIRE_THROWS_WITH(
            pf.readIn([&iss](std::string& line) {
                if (getline(iss, line))
                    return true;
                else
                    return false;
                }
            ),
            Catch::Matchers::Contains ("Keybinding local(T/F)(arg 4/6) is required")
        );
    }
}

TEST_CASE("profileFile: Write test", "[profileFile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFile = absPath("${TEMPORARY}/profile-test-write-test");
    std::string testFull_string = stringFromTestProfileFormat(test);
    {
        std::ofstream f(testFile);
        f << testFull_string;
    }

    ProfileFile pf(testFile);

    std::string result = pf.writeToString();
    REQUIRE(testFull_string == result);
}
