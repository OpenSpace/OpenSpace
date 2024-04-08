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

#include <openspace/interaction/joystickinputstate.h>

#include <ghoul/glm.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/misc/stringconversion.h>
#include <algorithm>
#include <map>
#include <numeric>

namespace openspace::interaction {

int JoystickInputStates::numAxes(const std::string& joystickName) const {
    if (joystickName.empty()) {
        int maxNumAxes = -1;
        for (auto it = begin(); it < end(); it++) {
            if (it->nAxes > maxNumAxes) {
                maxNumAxes = it->nAxes;
            }
        }
        return maxNumAxes;
    }

    for (const JoystickInputState& state : *this) {
        if (state.name == joystickName) {
            return state.nAxes;
        }
    }
    return -1;
}

int JoystickInputStates::numButtons(const std::string& joystickName) const {
    if (joystickName.empty()) {
        int maxNumButtons = -1;
        for (const JoystickInputState& state : *this) {
            if (state.nButtons > maxNumButtons) {
                maxNumButtons = state.nButtons;
            }
        }
        return maxNumButtons;
    }

    for (const JoystickInputState& state : *this) {
        if (state.name == joystickName) {
            return state.nButtons;
        }
    }

    return -1;
}

float JoystickInputStates::axis(const std::string& joystickName, int axis) const {
    ghoul_precondition(axis >= 0, "axis must be 0 or positive");

    if (joystickName.empty()) {
        const float res = std::accumulate(
            begin(),
            end(),
            0.f,
            [axis](float value, const JoystickInputState& state) {
                if (state.isConnected) {
                    value += state.axes[axis];
                }
                return value;
            }
        );

        // If multiple joysticks are connected, we might get values outside the -1,1 range
        // by summing them up
        return glm::clamp(res, -1.f, 1.f);
    }

    for (const JoystickInputState& state : *this) {
        if (state.name == joystickName) {
            return state.axes[axis];
        }
    }

    return 0.f;
}

bool JoystickInputStates::button(const std::string& joystickName, int button,
                                 JoystickAction action) const
{
    ghoul_precondition(button >= 0, "button must be 0 or positive");

    if (joystickName.empty()) {
        const bool res = std::any_of(
            begin(),
            end(),
            [button, action](const JoystickInputState& state) {
                return state.isConnected ? (state.buttons[button] == action) : false;
            }
        );
        return res;
    }

    for (const JoystickInputState& state : *this) {
        if (state.name == joystickName) {
            return state.isConnected ? (state.buttons[button] == action) : false;
        }
    }

    return false;
}

} // namespace openspace::interaction
