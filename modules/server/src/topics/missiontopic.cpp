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

#include <modules/server/include/topics/missiontopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/engine/globals.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/logging/logmanager.h>

using nlohmann::json;

namespace openspace {

bool MissionTopic::isDone() const {
    return true;
}

nlohmann::json MissionTopic::missionJson() const {
    const std::map<std::string, Mission>& missions =
        global::missionManager->missionMap();

    const ImageSequencer& sequencer = ImageSequencer::ref();
    const std::vector<double>& captureTimes = sequencer.captureProgression();
    std::vector<std::string> captureTimesString(captureTimes.size());

    for (size_t i = 0; i < captureTimes.size(); i++) {
        std::string str = SpiceManager::ref().dateFromEphemerisTime(
            sequencer.nextCaptureTime(captureTimes[i]),
            "YYYY-MM-DDTHR:MN:SC"
        );
        captureTimesString[i] = std::move(str);
    }
    json json;
    for (auto const& [identifier, mission] : missions) {
        nlohmann::json missionJson = createPhaseJson(mission);
        missionJson["capturetimes"] = captureTimesString;
        json[identifier] = missionJson;
    }
    return json;
}

nlohmann::json MissionTopic::createPhaseJson(const MissionPhase& phase) const {
    json phases = json::array();
    for (const MissionPhase& missionPhase : phase.phases()) {
        json subphaseJson = createPhaseJson(missionPhase);
        phases.push_back(std::move(subphaseJson));
    }

    json milestones = json::array();
    const std::vector<Milestone>& dates = phase.milestones();
    for (const Milestone& date : dates) {
        json jsonDate = {
            { "date", std::string(date.date.ISO8601()) },
            { "name", date.name }
        };

        if (date.description.has_value()) {
            jsonDate["description"] = *date.description;;
        }
        if (date.image.has_value()) {
            jsonDate["image"] = *date.image;
        }
        if (date.link.has_value()) {
            jsonDate["link"] = *date.link;
        }
        if (date.actions.has_value()) {
            jsonDate["actions"] = *date.actions;
        }
        milestones.push_back(std::move(jsonDate));
    }

    std::string startTimeString = std::string(Time(phase.timeRange().start).ISO8601());
    std::string endTimeString = std::string(Time(phase.timeRange().end).ISO8601());

    nlohmann::json phaseJson = {
        { "name", phase.name() },
        { "description", phase.description() },
        { "actions", phase.actions() },
        { "timerange", {
            { "start", startTimeString },
            { "end", endTimeString }
        }},
        { "phases", phases },
        { "image", phase.image() },
        { "link", phase.link() },
        { "milestones", milestones }
    };

    return phaseJson;
}

void MissionTopic::handleJson(const nlohmann::json&) {
    const nlohmann::json data = { {"missions", missionJson()} };
    _connection->sendJson(wrappedPayload(data));
}

} // namespace openspace
