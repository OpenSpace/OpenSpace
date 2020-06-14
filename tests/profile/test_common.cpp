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
#include "openspace/scene/profilefile.h"
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/exception.h>
#include <iostream>
#include <sstream>
#include <iomanip>

using namespace openspace;

namespace {
}

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

std::string stringFromSingleProfileSection(std::vector<std::string>& section,
                                                     bool blankLineSeparator)
{
    std::string result;
    for (std::string s : section) {
        result += s + "\n";
    }
    if (blankLineSeparator) {
        result += "\n";
    }
    return result;
}

std::string stringFromTestProfileFormat(testProfileFormat& tpf) {
    std::string fullProfile;

    fullProfile += stringFromSingleProfileSection(tpf.tsv, true);
    fullProfile += stringFromSingleProfileSection(tpf.tsm, true);
    fullProfile += stringFromSingleProfileSection(tpf.tsa, true);
    fullProfile += stringFromSingleProfileSection(tpf.tsp, true);
    fullProfile += stringFromSingleProfileSection(tpf.tsk, true);
    fullProfile += stringFromSingleProfileSection(tpf.tst, true);
    fullProfile += stringFromSingleProfileSection(tpf.tsc, true);
    fullProfile += stringFromSingleProfileSection(tpf.tsn, false);

    return fullProfile;
}

StringPerLineReader::StringPerLineReader(std::string s) : _iss(s) {
}

bool StringPerLineReader::getNextLine(std::string& line) {
    if (getline(_iss, line))
        return true;
    else
        return false;
}
