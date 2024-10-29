/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/sonification/include/focussonification.h>

#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>

namespace {
    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        FocusSonificationInfo =
    {
        "FocusSonification",
        "Focus Sonification",
        "Sonification that keeps track of the current focus node in the scene"
    };

} // namespace

namespace openspace {

FocusSonification::FocusSonification(const std::string& ip, int port)
    : SonificationBase(FocusSonificationInfo, ip, port)
{}

void FocusSonification::update(const Camera*) {
    if (!_enabled) {
        return;
    }

    bool hasNewData = getData();

    // Only send data if something new has happened
    if (hasNewData) {
        sendData();
    }
}

void FocusSonification::stop() {}

bool FocusSonification::getData() {
    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        // Scene is likely not yet initialized
        return false;
    }

    // Check if this data is new, otherwise don't update it
    std::string prevFocus = _currentFocus;
    bool shouldSendData = false;

    if (focusNode->identifier() != prevFocus) {
        _currentFocus = focusNode->identifier();
        shouldSendData = true;
    }

    return shouldSendData;
}

void FocusSonification::sendData() {
    std::string label = "/Focus";

    std::vector<OscDataType> data(NumDataItems);
    data[FocusNodeIndex] = _currentFocus;

    _connection->send(label, data);
}

} // namespace openspace
