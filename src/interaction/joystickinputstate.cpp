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

#include <openspace/interaction/joystickinputstate.h>

#include <ghoul/glm.h>

namespace openspace::interaction {

bool operator==(const JoystickInputState& lhs, const JoystickInputState& rhs) noexcept {
    return lhs.name == rhs.name &&
        lhs.axes == rhs.axes && lhs.nAxes == rhs.nAxes &&
        lhs.buttons == rhs.buttons && lhs.nButtons == rhs.nButtons;
}

bool operator!=(const JoystickInputState& lhs, const JoystickInputState& rhs) noexcept {
    return !(lhs == rhs);
}

bool operator==(const JoystickInputStates& lhs, const JoystickInputStates& rhs) noexcept {
    for (int i = 0; i < lhs.size(); ++i) {
        if (lhs[i] != nullptr ^ rhs[i] != nullptr) {
            // One of the states was empty, the other one was not
            return false;
        }

        if (lhs[i] && rhs[i] && (*lhs[i] != *rhs[i])) {
            return false;
        }
    }
    return true;
}

bool operator!=(const JoystickInputStates& lhs, const JoystickInputStates& rhs) noexcept {
    return !(lhs == rhs);
}


JoystickInputState::~JoystickInputState() {
    // The other pointers are owned by GLFW
    // buttonsTriggered gets allocated in main 
    delete buttonsTriggered;
}

float JoystickInputStates::axis(int i) const {
    float res = 0.f;
    for (const std::unique_ptr<JoystickInputState>& state : *this) {
        if (state) {
            res += state->axes[i];
        }
    }
    glm::clamp(res, -1.f, 1.f);
    return res;
}

bool JoystickInputStates::buttonPressed(int i) const {
    bool res = false;
    for (const std::unique_ptr<JoystickInputState>& state : *this) {
        if (state) {
            res |= state->buttons[i] == 1;
        }
    }
    return res;
}


bool JoystickInputStates::buttonTriggered(int i) const {
    bool res = false;
    for (const std::unique_ptr<JoystickInputState>& state : *this) {
        if (state) {
            res |= state->buttonsTriggered[i] == 1;
        }
    }
    return res;
}

} // namespace openspace::interaction
