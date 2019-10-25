/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

namespace {
    constexpr const char* _loggerCat = "InteractionMonitor";

    constexpr openspace::properties::Property::PropertyInfo IdleTimeInfo = {
        "IdleTime",
        "Idle Time",
        "Time passed from latest registerad interaction until application goes idle."
    };
    constexpr openspace::properties::Property::PropertyInfo IsInActiveStateInfo = {
        "IsInActiveStateInfo",
        "Is State Active",
        "Keeps track whether the interaction session is in active state or not. " 
        "False if application is in idle state, true if it is in active state."
    };
} // namespace

namespace openspace::interaction {

    InteractionMonitor::InteractionMonitor()
        : properties::PropertyOwner({ "InteractionMonitor" })
        , _isInActiveState(IsInActiveStateInfo, false)
        , _idleTime( properties::DoubleProperty(IdleTimeInfo, 120.0))
    {
        addProperty(_isInActiveState);
        addProperty(_idleTime);
    }

    void InteractionMonitor::setActivityState(bool isActive)
    {
        _isInActiveState.setValue(isActive);
    }

    void InteractionMonitor::updateActivityState()
    {
        double currentApplicationTime = global::windowDelegate.applicationTime();
        double timeDiff = currentApplicationTime - _lastInteractionTime;

        if (timeDiff >= _idleTime.value() && _isInActiveState) {
            setActivityState(false);
        }
    }

    void InteractionMonitor::registerInteraction(std::string interactionType)
    {
        _lastInteractionTime = global::windowDelegate.applicationTime();
        _lastInteractionType = interactionType;
        setActivityState(true);
        // LDEBUG(fmt::format("Interaction Type {} at time {}", _lastInteractionType, _lastInteractionTime));
    }

}
