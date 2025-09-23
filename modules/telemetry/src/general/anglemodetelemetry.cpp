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

#include <modules/telemetry/include/general/anglemodetelemetry.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "AngleModeTelemetry";

    // Indices for data items
    constexpr int NumDataItems = 2;
    constexpr int AngleModeIndex = 0;
    constexpr int IncludeElevationIndex = 1;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        AngleModeTelemetryInfo =
    {
        "AngleModeTelemetry",
        "Angle Mode Telemetry",
        "Telemetry that gathers data of what angle calculation mode is currently used "
        "for all telemetries in the telemetry module"
    };
} // namespace

namespace openspace {

AngleModeTelemetry::AngleModeTelemetry(const std::string& ip, int port)
    : TelemetryBase(AngleModeTelemetryInfo, ip, port)
{}

bool AngleModeTelemetry::updateData(const Camera*) {
    // Get the current angle settings
    TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
    if (!module) {
        LERROR("Could not find the TelemetryModule");
        return false;
    }
    TelemetryModule::AngleCalculationMode angleMode = module->angleCalculationMode();
    bool includeElevation = module->includeElevationAngle();

    // Check if this data is new, otherwise don't update it
    TelemetryModule::AngleCalculationMode prevAngleMode = _angleMode;
    bool prevIncludeElevation = _includeElevation;
    bool dataWasUpdated = false;

    if (angleMode != prevAngleMode) {
        _angleMode = angleMode;
        dataWasUpdated = true;
    }

    if (includeElevation != prevIncludeElevation) {
        _includeElevation = includeElevation;
        dataWasUpdated = true;
    }

    // Make sure that the first message is sent, even if the values are default and no
    // change has been detected
    if (!_isInitialized) {
        _isInitialized = true;
        return true;
    }

    return dataWasUpdated;
}

void AngleModeTelemetry::sendData() {
    std::string label = "/Mode";

    std::vector<OpenSoundControlDataType> data(NumDataItems);
    data[AngleModeIndex] = static_cast<int>(_angleMode);
    data[IncludeElevationIndex] = static_cast<int>(_includeElevation);

    _connection->send(label, data);
}

} // namespace openspace
