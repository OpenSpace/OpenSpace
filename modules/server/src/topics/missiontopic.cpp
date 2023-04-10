/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/util/time.h>
#include <ghoul/logging/logmanager.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/util/spicemanager.h>

using nlohmann::json;

namespace {
    constexpr const char* EventKey = "event";
    constexpr const char* StartSubscription = "start_subscription";
    constexpr const char* StopSubscription = "stop_subscription";
} // namespace

namespace openspace {

bool MissionTopic::isDone() const {
    return true;
}

nlohmann::json MissionTopic::missionJson() const {
    
    const std::map<std::string, Mission>& missions =
        global::missionManager->missionMap();

    ImageSequencer& sequencer = ImageSequencer::ref();
    const std::vector<double>& captureTimes = sequencer.captureProgression();
    std::vector<std::string> captureTimesString(captureTimes.size());

    for (int i = 0; i < captureTimes.size(); i++) {
        const std::string& str = SpiceManager::ref().dateFromEphemerisTime(
            sequencer.nextCaptureTime(captureTimes[i]),
            "YYYY-MM-DDTHR:MN:SC"
        );
        captureTimesString[i] = str;
    }
    json json;
    for (auto const& [name, mission] : missions) {
        nlohmann::json missionJson = createPhaseJson(mission);
        missionJson["capturetimes"] = captureTimesString;
        json.push_back(missionJson);
    }

    return json;
}

nlohmann::json MissionTopic::createPhaseJson(const MissionPhase& phase) const {

    json phases = json::array();
    for (const MissionPhase& missionPhase : phase.phases()) {
        json subphaseJson = createPhaseJson(missionPhase);
        phases.push_back(subphaseJson);
    }

    json importandDates = json::array();
    for (const std::pair<std::string, Time> date : phase.importantDates()) {
        json jsonDate = {
            { "date", std::string(date.second.ISO8601())},
            { "name", date.first }
        };
        importandDates.push_back(jsonDate);
    }

    std::string startTimeString = std::string(Time(phase.timeRange().start).ISO8601());
    std::string endTimeString = std::string(Time(phase.timeRange().end).ISO8601());

    nlohmann::json phaseJson = {
        { "name", phase.name() },
        { "description", phase.description() },
        { "actions" , phase.actions() },
        { "timerange", {
            { "start" ,startTimeString },
            { "end" ,endTimeString }
        }},
        { "phases", phases },
        { "media",{
            { "image", phase.image() }
        }},
        { "importantDates" , importandDates }
    };

    return phaseJson;
}

void MissionTopic::handleJson(const nlohmann::json& input) {
    //const std::string& event = input.at(EventKey).get<std::string>();
    //if (event == StartSubscription) {
    //    // TODO: subsribe to misisons to emit events
    //    // missionManager.subscribe(); ...
    //}
    //else if (event == StopSubscription) {
    //    // TODO: Unsubsribe to misisons
    //    // missionManager.subscribe(); ...
    //    return;
    //}
    nlohmann::json data = { {"missions", missionJson()} };
    _connection->sendJson(wrappedPayload(data));
}

} // namespace openspace
