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

#include <modules/server/include/topics/flightcontrollertopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>

#include <openspace/interaction/inputstate.h>
#include <openspace/interaction/websocketcamerastates.h>
#include <openspace/interaction/websocketinputstate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>

#include <iterator>
#include <unordered_map>

namespace {

    using AxisType = openspace::interaction::WebsocketCameraStates::AxisType;

    constexpr const char* _loggerCat = "FlightControllerTopic";
    constexpr const char* TypeKey = "type";

    constexpr const char* FlightControllerType = "flightcontroller";

    const std::string OrbitX = "orbitX";
    const std::string OrbitY = "orbitY";
    const std::string ZoomIn = "zoomIn";
    const std::string ZoomOut = "zoomOut";
    const std::string LocalRollX = "localRollX";
    const std::string LocalRollY = "localRollY";
    const std::string GlobalRollX = "globalRollX";
    const std::string GlobalRollY = "globalRollY";
    const std::string PanX = "panX";
    const std::string PanY = "panY";

    const static std::unordered_map<std::string, AxisType> AxisIndexMap ({
        {OrbitX, AxisType::OrbitX},
        {OrbitY, AxisType::OrbitY},
        {ZoomIn, AxisType::ZoomIn},
        {ZoomOut, AxisType::ZoomOut},
        {LocalRollX, AxisType::LocalRollX},
        {LocalRollY, AxisType::LocalRollY},
        {GlobalRollX, AxisType::GlobalRollX},
        {GlobalRollY, AxisType::GlobalRollY},
        {PanX, AxisType::PanX},
        {PanY, AxisType::PanY}
    });

    const int Axes = 10;

} // namespace

using nlohmann::json;

namespace openspace {

FlightControllerTopic::FlightControllerTopic()
: _isDone(false)
{
    for (auto it = AxisIndexMap.begin(); it != AxisIndexMap.end(); ++it) {
        OsEng.navigationHandler().setWebsocketAxisMapping(
            int(std::distance(AxisIndexMap.begin(), it)),
            it->second);
    }

    OsEng.navigationHandler().setWebsocketInputStates(_inputStates);
}

FlightControllerTopic::~FlightControllerTopic() {
}

bool FlightControllerTopic::isDone() const {
    return _isDone;
}

void FlightControllerTopic::handleJson(const nlohmann::json& json) {
    auto state = _inputStates.begin();
    std::fill(state->axes.begin(), state->axes.end(), 0);
    state->isConnected = true;

    for (auto it = json.begin(); it != json.end(); ++it) {
        const auto mapIt = AxisIndexMap.find(it.key());
        if (mapIt == AxisIndexMap.end()) {
            LWARNING(fmt::format(
                "No axis named {} (value: {})", it.key() , it.value()
            ));
            continue;
        }

        state->axes[std::distance(AxisIndexMap.begin(), mapIt)] = float(it.value());
    }
}

} // namespace openspace
