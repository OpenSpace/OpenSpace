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

#ifndef __OPENSPACE_CORE___JOYSTICKINPUTSTATE___H__
#define __OPENSPACE_CORE___JOYSTICKINPUTSTATE___H__

#include <array>
#include <memory>
#include <string>

namespace openspace::interaction {

enum class JoystickAction : uint8_t {
    Idle = 0,
    Press = 1,
    Repeat = 2,
    Release = 3
};

struct JoystickInputState {
    bool isConnected = false;

    std::string name;

    int nAxes = 0;
    std::array<float, 8> axes;

    int nButtons = 0;
    std::array<JoystickAction, 32> buttons;
};

// Number is derived from GLFW constants
struct JoystickInputStates : public std::array<JoystickInputState, 16> {
    float axis(int i) const;
    bool button(int i, JoystickAction action) const;
};

//bool operator==(const JoystickInputState& lhs, const JoystickInputState& rhs) noexcept;
//bool operator!=(const JoystickInputState& lhs, const JoystickInputState& rhs) noexcept;
//bool operator==(const JoystickInputStates& lhs, const JoystickInputStates& rhs) noexcept;
//bool operator!=(const JoystickInputStates& lhs, const JoystickInputStates& rhs) noexcept;


} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___JOYSTICKSTATE___H__
