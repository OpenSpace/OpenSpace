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

#include <openspace/interaction/interactionmonitor.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo IdleTimeInfo = {
        "IdleTime",
        "Idle Time",
        "Time in seconds that has passed from latest registered interaction until the "
        "application goes idle.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IsInActiveStateInfo = {
        "IsInActiveState",
        "Is State Active",
        "Keeps track whether the interaction session is in active state or not. False if "
        "application is in idle state, true if it is in active state.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace::interaction {

InteractionMonitor::InteractionMonitor()
    : properties::PropertyOwner({ "InteractionMonitor", "Interaction Monitor" })
    , _isInActiveState(IsInActiveStateInfo, false)
    , _idleTime(IdleTimeInfo, 120.f, 0.f, 300.f)
{
    addProperty(_isInActiveState);
    addProperty(_idleTime);
}

void InteractionMonitor::setActivityState(bool isActive) {
    _isInActiveState = isActive;
}

void InteractionMonitor::setIdleTime(float time) {
    _idleTime = time;
}

void InteractionMonitor::updateActivityState() {
    const double currentApplicationTime = global::windowDelegate->applicationTime();
    const double timeDiff = currentApplicationTime - _lastInteractionTime;

    if (timeDiff >= _idleTime && _isInActiveState) {
        _isInActiveState = false;
    }
}

void InteractionMonitor::markInteraction() {
    _lastInteractionTime = global::windowDelegate->applicationTime();
    _isInActiveState = true;
}

} // namespace openspace::interaction
