/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <algorithm>
#include <filesystem>
#include <optional>

namespace {
    struct [[codegen::Dictionary(MissionPhase)]] Parameters {
        // The human readable name of this mission or mission phase that is displayed to
        // the user
        std::string name;

        // A description of this mission or mission phase
        std::optional<std::string> description;

        struct TimeRange {
            std::string start
                [[codegen::annotation("A string representing a valid date")]];
            std::optional<std::string> end
                [[codegen::annotation("A string representing a valid date")]];
        };
        // The time range for which this mission or mission phase is valid. If no time
        // range is specified, the ranges of sub mission phases are used instead
        std::optional<TimeRange> timeRange;

        // The phases into which this mission or mission phase is separated
        std::optional<std::vector<ghoul::Dictionary>> phases
            [[codegen::reference("core_mission_mission")]];

        // An image that can be presented to the user during this phase of a mission
        std::optional<std::string> image;
        std::optional<std::string> link;

        // Actions associated with this phase
        std::optional<std::vector<std::string>> actions;

        // Important dates
        struct Milestone {
            // An image that can be presented to the user during this phase of a mission
            std::string date;
            std::string name;
            std::optional<std::string> description;
            std::optional<std::string> image;
            std::optional<std::string> link;
            std::optional<std::vector<std::string>> actions;
        };
        std::optional<std::vector<Milestone>> milestones;
    };
#include "mission_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation MissionPhase::Documentation() {
    return codegen::doc<Parameters>("core_mission_mission");
}

MissionPhase::MissionPhase(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _name = p.name;
    _description = p.description.value_or(_description);
    _image = p.image.value_or(_image);
    _link = p.link.value_or(_link);

    if (p.phases.has_value() && !p.phases->empty()) {
        _subphases.reserve(p.phases->size());
        for (const ghoul::Dictionary& phase : *p.phases) {
            _subphases.emplace_back(phase);
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

        // user may specify an overall time range. In that case expand this timerange
        if (p.timeRange.has_value()) {
            const std::string start = p.timeRange->start;
            const std::string end = p.timeRange->end.value_or(start);

            const TimeRange overallTimeRange = TimeRange(
                SpiceManager::ref().ephemerisTimeFromDate(start),
                SpiceManager::ref().ephemerisTimeFromDate(end)
            );

            if (!overallTimeRange.includes(timeRangeSubPhases)) {
                throw ghoul::RuntimeError(fmt::format(
                    "User specified time range must at least include its subphases'",
                    "Mission ({})", _name
                ));
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
        if (p.timeRange.has_value()) {
            const std::string start = p.timeRange->start;
            const std::string end = p.timeRange->end.value_or(start);

            _timeRange = TimeRange(
                SpiceManager::ref().ephemerisTimeFromDate(start),
                SpiceManager::ref().ephemerisTimeFromDate(end)
            );
        }
        else {
            throw ghoul::RuntimeError(fmt::format(
                "If there are no subphases specified, the time range has to be specified",
                "Mission ({})", _name
            ));
        }
    }

    if (p.actions.has_value()) {
        _actions = p.actions.value_or(_actions);
    }

    if (p.milestones.has_value()) {
        _milestones.reserve(p.milestones->size());
        for (const Parameters::Milestone& milestone : *p.milestones) {
            std::string name = milestone.name;
            Milestone newDate = {
                .name = std::move(name),
                .date = Time(milestone.date)
            };
            if (milestone.description.has_value()) {
                newDate.description = milestone.description.value();
            }
            if (milestone.image.has_value()) {
                newDate.image = milestone.image.value();
            }
            if (milestone.link.has_value()) {
                newDate.link = milestone.link.value();
            }
            if (milestone.actions.has_value()) {
                newDate.actions = milestone.actions.value();
            }
            _milestones.emplace_back(newDate);
        }
    }
}

const std::string& MissionPhase::name() const {
    return _name;
}

const TimeRange& MissionPhase::timeRange() const {
    return _timeRange;
}

const std::string& MissionPhase::description() const {
    return _description;
}

const std::string& MissionPhase::image() const {
    return _image;
}

const std::string& MissionPhase::link() const {
    return _link;
}

const std::vector<Milestone>& MissionPhase::milestones() const {
    return _milestones;
}

const std::vector<MissionPhase>& MissionPhase::phases() const {
    return _subphases;
}

const std::vector<std::string>& MissionPhase::actions() const {
    return _actions;
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
    //ghoul_assert(maxDepth >= 0, "maxDepth must not be negative");

    if (maxDepth == 0) {
        return;
    }

    for (const MissionPhase& phase : _subphases) {
        if (phase.timeRange().includes(time)) {
            trace.emplace_back(phase);
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
    ghoul_assert(std::filesystem::is_regular_file(filename), "filename must exist");

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
