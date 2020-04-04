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
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/exception.h>
#include <iostream>
#include <iomanip>

using namespace openspace;

namespace {
}

struct testProfileFormat {
    std::vector<std::string> tsv;
    std::vector<std::string> tsm;
    std::vector<std::string> tsa;
    std::vector<std::string> tsp;
    std::vector<std::string> tsk;
    std::vector<std::string> tst;
    std::vector<std::string> tsc;
    std::vector<std::string> tsn;
};

testProfileFormat buildTestProfile1() {
    testProfileFormat tp1;
    tp1.tsv.push_back("#Version");
    tp1.tsv.push_back("123.4");
    tp1.tsm.push_back("#Module");
    tp1.tsm.push_back("globebrowsing\t\t");
    tp1.tsm.push_back("gaia\tprint(\"success.\")\t");
    tp1.tsm.push_back("volume\t\tquit");
    tp1.tsa.push_back("#Asset");
    tp1.tsa.push_back("scene/solarsystem/planets/earth/moon/moon\trequired");
    tp1.tsa.push_back("scene/solarsystem/missions/apollo/apollo8\trequested");
    tp1.tsa.push_back("scene/solarsystem/planets/earth/earth\t");
    tp1.tsp.push_back("#Property");
    tp1.tsp.push_back("setPropertyValue\tNavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance\t20.000000");
    tp1.tsp.push_back("setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse");
    tp1.tsp.push_back("setPropertyValue\tScene.Charon.Renderable.Enabled\tfalse");
    tp1.tsp.push_back("setPropertyValueSingle\tScene.PlutoBarycenterTrail.Renderable.Enabled\tfalse");
    tp1.tsk.push_back("#Keybinding");
    tp1.tsk.push_back("F8\tSets the time to the approach at Bennu.\tSet Bennu approach time\t/Missions/Osiris Rex\tfalse\t\"openspace.printInfo('Set time: Approach'); openspace.time.setTime('2018-SEP-11 21:31:01.183')\"");
    tp1.tsk.push_back("F9\tSets the time to the preliminary survey of Bennu.\tSet Bennu survey time\t/Missions/Osiris Rex\tfalse\t\"openspace.printInfo('Set time: Preliminary Survey'); openspace.time.setTime('2018-NOV-20 01:13:12.183')\"");
    tp1.tsk.push_back("F10\tSets the time to the orbital B event.\tSet orbital B event time\t/Missions/Osiris Rex\tfalse\t\"openspace.printInfo('Set time: Orbital B'); openspace.time.setTime('2019-APR-08 10:35:27.186')\"");
    tp1.tsk.push_back("F11\tSets the time to the recon event.\tSet recon event time\t/Missions/Osiris Rex\tfalse\t\"openspace.printInfo('Set time: Recon'); openspace.time.setTime('2019-MAY-25 03:50:31.195')\"");
    tp1.tst.push_back("#Time");
    tp1.tst.push_back("absolute\t1977-12-21T12:51:51.0");
    tp1.tsc.push_back("#Camera");
    tp1.tsc.push_back("setNavigationState\t\"NewHorizons\"\t\t\"Root\"\t-6.572656E1, -7.239404E1, -2.111890E1\t0.102164, -0.362945, 0.926193\t\t");
    tp1.tsn.push_back("#MarkNodes");
    tp1.tsn.push_back("Pluto");
    tp1.tsn.push_back("NewHorizons");
    tp1.tsn.push_back("Charon");

    return tp1;
}

std::string stringFromSingleProfileSection(std::vector<std::string>& section) {
    std::string result;
    for (std::string s : section) {
        result += s + "\n";
    }
    result += "\n";
    return result;
}

std::string stringFromTestProfileFormat(testProfileFormat& tpf) {
    std::string fullProfile;

    fullProfile += stringFromSingleProfileSection(tpf.tsv);
    fullProfile += stringFromSingleProfileSection(tpf.tsm);
    fullProfile += stringFromSingleProfileSection(tpf.tsa);
    fullProfile += stringFromSingleProfileSection(tpf.tsp);
    fullProfile += stringFromSingleProfileSection(tpf.tsk);
    fullProfile += stringFromSingleProfileSection(tpf.tst);
    fullProfile += stringFromSingleProfileSection(tpf.tsc);
    fullProfile += stringFromSingleProfileSection(tpf.tsn);

    return fullProfile;
}

TEST_CASE("profileFile: Simple read and verify", "[profileFile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFull_string = stringFromTestProfileFormat(test);
    std::istringstream iss(testFull_string);

    ProfileFile pf;
    pf.readLines([&iss](std::string& line) {
        if (getline(iss, line))
            return true;
        else
            return false;
    });

    std::vector<std::string> tVect;

    REQUIRE(pf.getVersion() == test.tsv[1]);
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

    ProfileFile pf;
    REQUIRE_THROWS_WITH(
        pf.readLines([&iss](std::string& line) {
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
        ProfileFile pf;
        REQUIRE_THROWS_WITH(
            pf.readLines([&iss](std::string& line) {
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
        ProfileFile pf;
        REQUIRE_THROWS_WITH(
            pf.readLines([&iss](std::string& line) {
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
        ProfileFile pf;
        REQUIRE_THROWS_WITH(
            pf.readLines([&iss](std::string& line) {
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
        ProfileFile pf;
        REQUIRE_THROWS_WITH(
            pf.readLines([&iss](std::string& line) {
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
        ProfileFile pf;
        REQUIRE_THROWS_WITH(
            pf.readLines([&iss](std::string& line) {
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
