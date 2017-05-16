/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/mission/mission.h>

#include <openspace/documentation/verifier.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>

namespace {
    const char* KeyName = "Name";
    const char* KeyDescription = "Description";
    const char* KeyPhases = "Phases";
    const char* KeyTimeRange = "TimeRange";
}

namespace openspace {

documentation::Documentation MissionPhase::Documentation() {
    using namespace documentation;

    return {
        "Missions and Mission Phases",
        "core_mission_mission",
        {
            {
                KeyName,
                new StringVerifier,
                "The human readable name of this mission or mission phase that is "
                "displayed to the user.",
                Optional::No
            },
            {
                KeyDescription,
                new StringVerifier,
                "A description of this mission or mission phase.",
                Optional::Yes
            },
            {
                KeyTimeRange,
                new ReferencingVerifier("core_util_timerange"),
                "The time range for which this mission or mission phase is valid. If no "
                "time range is specified, the ranges of sub mission phases are used "
                "instead.",
                Optional::Yes
            },
            {
                KeyPhases,
                new ReferencingVerifier("core_mission_mission"),
                "The phases into which this mission or mission phase is separated.",
                Optional::Yes
            }
        },
        Exhaustive::Yes
    };
}

MissionPhase::MissionPhase(const ghoul::Dictionary& dict) {
    _name = dict.value<std::string>(KeyName);
    dict.getValue(KeyDescription, _description);
    
    ghoul::Dictionary childDicts;
    if (dict.getValue(KeyPhases, childDicts)) {
        // This is a nested mission phase
        _subphases.reserve(childDicts.size());
        for (size_t i = 0; i < childDicts.size(); ++i) {
            std::string key = std::to_string(i + 1);
            _subphases.emplace_back(childDicts.value<ghoul::Dictionary>(key));
        }

        // Ensure subphases are sorted
        std::stable_sort(
            _subphases.begin(),
            _subphases.end(),
            [](const MissionPhase& a, const MissionPhase& b) {
                return a.timeRange().start < b.timeRange().start;
            }
        );

        // Calculate the total time range of all subphases
        TimeRange timeRangeSubPhases;
        timeRangeSubPhases.start = _subphases[0].timeRange().start;
        timeRangeSubPhases.end = _subphases.back().timeRange().end;

        // user may specify an overall time range. In that case expand this timerange.
        ghoul::Dictionary timeRangeDict;
        if (dict.getValue(KeyTimeRange, timeRangeDict)) {
            TimeRange overallTimeRange(timeRangeDict);
            if (!overallTimeRange.includes(timeRangeSubPhases)) {
                throw ghoul::RuntimeError(
                    "User specified time range must at least include its subphases'",
                    "Mission (" + _name + ")"
                );
            }

            _timeRange.include(overallTimeRange);
        }
        else {
            // Its OK to not specify an overall time range, the time range for the 
            // subphases will simply be used. 
            _timeRange.include(timeRangeSubPhases);
        }
    }
    else {
        ghoul::Dictionary timeRangeDict;
        if (dict.getValue(KeyTimeRange, timeRangeDict)) {
            _timeRange = TimeRange(timeRangeDict); // throws exception if unable to parse
        }
        else {
            throw ghoul::RuntimeError(
                "If there are no subphases specified, the time range has to be specified",
                "Mission (" + _name + ")"
            );
        }
    }
}

std::string MissionPhase::name() const {
    return _name;
}

TimeRange MissionPhase::timeRange() const {
    return _timeRange;
}

std::string MissionPhase::description() const {
    return _description;
}

std::vector<MissionPhase> MissionPhase::phases() const {
    return _subphases;
}

MissionPhase::Trace MissionPhase::phaseTrace(double time, int maxDepth) const {
    Trace trace;
    if (_timeRange.includes(time)) {
        trace.push_back(std::cref(*this));
        phaseTrace(time, trace, maxDepth);
    }
    return trace;
}

void MissionPhase::phaseTrace(double time, Trace& trace, int maxDepth) const {
    ghoul_assert(maxDepth >= 0, "maxDepth must not be negative");
    
    if (maxDepth == 0) {
        return;
    }

    for (const MissionPhase& phase : _subphases) {
        if (phase.timeRange().includes(time)) {
            trace.push_back(phase);
            phase.phaseTrace(time, trace, maxDepth - 1);
            return;
        }
        else if (phase.timeRange().start > time) {
            // Since time ranges are sorted we can do early termination
            return;
        }
    }
}

Mission missionFromFile(const std::string& filename) {
    ghoul_assert(!filename.empty(), "filename must not be empty");
    ghoul_assert(!FileSys.containsToken(filename), "filename must not contain tokens");
    ghoul_assert(FileSys.fileExists(filename), "filename must exist");

    ghoul::Dictionary missionDict;
    ghoul::lua::loadDictionaryFromFile(filename, missionDict);

    documentation::testSpecificationAndThrow(
        MissionPhase::Documentation(),
        missionDict,
        "Mission"
    );

    return MissionPhase(missionDict);
}

}  // namespace openspace
