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

//bool operator==(const JoystickInputState& lhs, const JoystickInputState& rhs) noexcept {
//    return lhs.name == rhs.name &&
//        lhs.axes == rhs.axes && lhs.nAxes == rhs.nAxes &&
//        lhs.buttons == rhs.buttons && lhs.nButtons == rhs.nButtons;
//}
//
//bool operator!=(const JoystickInputState& lhs, const JoystickInputState& rhs) noexcept {
//    return !(lhs == rhs);
//}
//
//bool operator==(const JoystickInputStates& lhs, const JoystickInputStates& rhs) noexcept {
//    for (int i = 0; i < lhs.size(); ++i) {
//        if (lhs[i] != nullptr ^ rhs[i] != nullptr) {
//            // One of the states was empty, the other one was not
//            return false;
//        }
//
//        if (lhs[i] && rhs[i] && (*lhs[i] != *rhs[i])) {
//            return false;
//        }
//    }
//    return true;
//}
//
//bool operator!=(const JoystickInputStates& lhs, const JoystickInputStates& rhs) noexcept {
//    return !(lhs == rhs);
//}


float JoystickInputStates::axis(int i) const {
    float res = 0.f;
    for (const JoystickInputState& state : *this) {
        if (state.isConnected) {
            res += state.axes[i];
        }
    }

    // If multiple joysticks are connected, we might get values outside the -1,1 range by
    // summing them up
    glm::clamp(res, -1.f, 1.f);
    return res;
}

bool JoystickInputStates::button(int i, JoystickAction action) const {
    bool res = false;
    for (const JoystickInputState& state : *this) {
        if (state.isConnected) {
            res |= state.buttons[i] == action;
        }
    }
    return res;
}

} // namespace openspace::interaction
