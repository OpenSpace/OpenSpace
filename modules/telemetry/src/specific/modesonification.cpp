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

#include <modules/telemetry/include/specific/modesonification.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

namespace {
    constexpr std::string_view _loggerCat = "ModeSonification";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        ModeSonificationInfo =
    {
       "ModeSonification",
       "Surround Mode Sonification",
       "Sonification that alters all other sonificatoins based on the current surround "
       "mode"
    };

} // namespace

namespace openspace {

ModeSonification::ModeSonification(const std::string& ip, int port)
    : TelemetryBase(ModeSonificationInfo, ip, port)
{
    // Assume None at start
    _currentMode = TelemetryModule::SurroundMode::None;

    // Get access to the sonification module
    _sonificationModule = global::moduleEngine->module<TelemetryModule>();
    if (!_sonificationModule) {
        LERROR("Could not find the SonificationModule");
    }
}

void ModeSonification::update(const Camera*) {
    if (!_enabled || !_sonificationModule) {
        return;
    }

    bool hasNewData = getData();

    // Only send data if something new has happened
    if (hasNewData) {
        sendData();
    }
}

void ModeSonification::stop() {}

bool ModeSonification::getData() {
    // Check if this data is new, otherwise don't update it
    TelemetryModule::SurroundMode mode = _sonificationModule->surroundMode();
    TelemetryModule::SurroundMode prevMode = _currentMode;
    bool shouldSendData = false;

    if (mode != prevMode) {
        _currentMode = mode;
        shouldSendData = true;
    }

    return shouldSendData;
}

void ModeSonification::sendData() {
    std::string label = "/Mode";

    std::vector<OscDataType> data(NumDataItems);
    data[ModeIndex] = static_cast<int>(_currentMode);

    _connection->send(label, data);
}

} // namespace openspace
