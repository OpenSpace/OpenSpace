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

#include <ghoul/misc/stringconversion.h>
#include <array>
#include <memory>
#include <string>

namespace openspace::interaction {

/**
 * Actions that any button of a joystick can have. Each button must be in one of these
 * states
 */
enum class JoystickAction : uint8_t {
    /// Idle state if the button is unpressed and has been unpressed since last frame
    Idle = 0,
    /// If the button has been pressed since the last frame
    Press,
    /// If the button has been pressed since longer than last frame
    Repeat,
    /// If the button was released since the last frame
    Release
};

/**
 * The input state of a single joystick.
 */
struct JoystickInputState {
    /// The maximum number of supported axes
    static constexpr const int MaxAxes = 8;
    /// The maximum number of supported buttons
    static constexpr const int MaxButtons = 32;

    /// Marks whether this joystick is connected. If this value is \c false, all other
    /// members of this struct are undefined
    bool isConnected = false;

    /// The name of this joystick
    std::string name;

    /// The number of axes that this joystick supports
    int nAxes = 0;
    /// The values for each axis. Each value is in the range [-1, 1]. Only the first
    /// \c nAxes values are defined values, the rest are undefined
    std::array<float, MaxAxes> axes;

    /// The number of buttons that this joystick possesses
    int nButtons = 0;
    /// The status of each button. Only the first \c nButtons values are defined, the rest
    /// are undefined
    std::array<JoystickAction, MaxButtons> buttons;
};

/// The maximum number of joysticks that are supported by this system. This number is
/// derived from the available GLFW constants
constexpr const int MaxJoysticks = 16;
struct JoystickInputStates : public std::array<JoystickInputState, MaxJoysticks> {
    /**
     * This function adds the contributions of all connected joysticks for the provided
     * \p axis. After adding each joysticks contribution, the result is clamped to [-1,1].
     * If a joystick does not possess a particular axis, it's does not contribute to the
     * sum.
     *
     * \param axis The numerical axis for which the values are added
     * \return The summed axis values of all connected joysticks
     *
     * \pre \p axis must be 0 or positive
     */
    float axis(int axis) const;

    /**
     * This functions checks whether any connected joystick has its \p button in the
     * passed \p action. Any joystick that does not posses the \p button, it will be
     * ignored.
     *
     * \param button The button that is to be checked
     * \param action The action which is checked for each button
     * \return \c true if there is at least one joystick whose \param button is in the
     *         \p action state
     *
     * \pre \p button must be 0 or positive
     */
    bool button(int button, JoystickAction action) const;
};

} // namespace openspace::interaction

namespace ghoul {

template <>
std::string to_string(const openspace::interaction::JoystickAction& action);

template <>
openspace::interaction::JoystickAction from_string(const std::string& str);

} // namespace ghoul

#endif // __OPENSPACE_CORE___JOYSTICKINPUTSTATE___H__
