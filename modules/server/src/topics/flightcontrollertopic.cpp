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
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/util/timemanager.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "FlightControllerTopic";
    constexpr const char* EventKey = "event";

    constexpr const char* FlightControllerEvent = "flightcontroller_event";

    constexpr const char* OrbitX = "orbitX";
    constexpr const char* OrbitY = "orbitY";
    constexpr const char* ZoomIn = "zoomIn";
    constexpr const char* ZoomOut = "zoomOut";
    constexpr const char* LocalRollX = "localRollX";
    constexpr const char* LocalRollY = "localRollY";
    constexpr const char* GlobalRollX = "globalRollX";
    constexpr const char* GlobalRollY = "globalRollY";
    constexpr const char* PanX = "panX";
    constexpr const char* PanY = "panY";

    const std::vector<const char*> InteractionProperties {
        OrbitX,
        OrbitY,
        ZoomIn,
        ZoomOut,
        LocalRollX,
        LocalRollY,
        GlobalRollX,
        GlobalRollY,
        PanX,
        PanY
    };
} // namespace

using nlohmann::json;

namespace openspace {

FlightControllerTopic::~FlightControllerTopic() {
}

bool FlightControllerTopic::isDone() const {
    return true;
}

void FlightControllerTopic::handleJson(const nlohmann::json& json) {
    std::string key = json.at(OrbitX).get<std::string>();
    const std::string& event = json.at(EventKey).get<std::string>();

    if (event == FlightControllerEvent) {
        for (auto it = json.begin(); it != json.end(); ++it) {
            LDEBUG(fmt::format("{} : {}", it.key(), it.value()));
        }
    }
}

} // namespace openspace
