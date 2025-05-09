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

#include <modules/telemetry/include/general/focustelemetry.h>

#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>

namespace {
    // Indices for data items
    constexpr int NumDataItems = 1;
    constexpr int FocusNodeIndex = 0;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        FocusTelemetryInfo =
    {
        "FocusTelemetry",
        "Focus Telemetry",
        "Telemetry that sends out the current focus node to the Open Sound Control "
        "receiver."
    };

} // namespace

namespace openspace {

FocusTelemetry::FocusTelemetry(const std::string& ip, int port)
    : TelemetryBase(FocusTelemetryInfo, ip, port)
{}

bool FocusTelemetry::updateData(const Camera*) {
    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        // Scene is likely not yet initialized
        return false;
    }

    // Check if this data is new, otherwise don't update it
    std::string prevFocus = _currentFocus;
    bool dataWasUpdated = false;

    if (focusNode->identifier() != prevFocus) {
        _currentFocus = focusNode->identifier();
        dataWasUpdated = true;
    }

    return dataWasUpdated;
}

void FocusTelemetry::sendData() {
    std::string label = "/Focus";

    std::vector<OpenSoundControlDataType> data(NumDataItems);
    data[FocusNodeIndex] = _currentFocus;

    _connection->send(label, data);
}

} // namespace openspace
