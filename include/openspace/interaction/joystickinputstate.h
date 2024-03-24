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

#ifndef __OPENSPACE_CORE___JOYSTICKINPUTSTATE___H__
#define __OPENSPACE_CORE___JOYSTICKINPUTSTATE___H__

#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/fmt.h>
#include <array>
#include <memory>
#include <string>

namespace openspace::interaction {

/**
 * Actions that any button of a joystick can have. Each button must be in one of these
 * states.
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
    /// Marks whether this joystick is connected. If this value is `false`, all other
    /// members of this struct are undefined
    bool isConnected = false;

    /// The name of this joystick
    std::string name;

    /// The number of axes that this joystick supports
    int nAxes = 0;
    /// The values for each axis. Each value is in the range [-1, 1]
    std::vector<float> axes;

    /// The number of buttons that this joystick possesses
    int nButtons = 0;
    /// The status of each button
    std::vector<JoystickAction> buttons;
};

/// The maximum number of joysticks that are supported by this system. This number is
/// derived from the available GLFW constants
constexpr int MaxJoysticks = 16;
struct JoystickInputStates : public std::array<JoystickInputState, MaxJoysticks> {
    /// The maximum number of joysticks that are supported by this system. This number is
    /// derived from the available GLFW constants
    static constexpr int MaxNumJoysticks = 16;

    /**
     * This function return the number of axes the joystick with the given name has.
     *
     * \param joystickName The name of the joystick to check how many axes it has,
     *        if empty the max number of axes for all joysticks are returned
     * \return The number of axes for the joystick with the given name
     */
    int numAxes(const std::string& joystickName = "") const;

    /**
     * This function return the number of buttons the joystick with the given name has.
     *
     * \param joystickName The name of the joystick to check how many buttons it has,
     *        if empty the max number of buttons for all joysticks are returned
     * \return The number of buttons for the joystick with the given name
     */
    int numButtons(const std::string& joystickName = "") const;

    /**
     * This function adds the contributions of all connected joysticks for the provided
     * \p axis. After adding each joysticks contribution, the result is clamped to [-1,1].
     * If a joystick does not possess a particular axis, it's does not contribute to the
     * sum.
     *
     * \param joystickName The name of the joystick, if empty all joysticks are combined
     * \param axis The numerical axis for which the values are added
     * \return The summed axis values of all connected joysticks
     *
     * \pre \p axis must be 0 or positive
     */
    float axis(const std::string& joystickName, int axis) const;

    /**
     * This functions checks whether any connected joystick has its \p button in the
     * passed \p action. Any joystick that does not posses the \p button, it will be
     * ignored.
     *
     * \param joystickName The name of the joystick, if empty all joysticks are combined
     * \param button The button that is to be checked
     * \param action The action which is checked for each button
     * \return `true` if there is at least one joystick whose \p button is in the
     *         \p action state
     *
     * \pre \p button must be 0 or positive
     */
    bool button(const std::string& joystickName, int button, JoystickAction action) const;
};

} // namespace openspace::interaction

namespace ghoul {

template <>
inline std::string to_string(const openspace::interaction::JoystickAction& value) {
    switch (value) {
        case openspace::interaction::JoystickAction::Idle:    return "Idle";
        case openspace::interaction::JoystickAction::Press:   return "Press";
        case openspace::interaction::JoystickAction::Repeat:  return "Repeat";
        case openspace::interaction::JoystickAction::Release: return "Release";
        default:                                             throw MissingCaseException();
    }
}

template <>
constexpr openspace::interaction::JoystickAction from_string(std::string_view string) {
    if (string == "Idle")    { return openspace::interaction::JoystickAction::Idle; }
    if (string == "Press")   { return openspace::interaction::JoystickAction::Press; }
    if (string == "Repeat")  { return openspace::interaction::JoystickAction::Repeat; }
    if (string == "Release") { return openspace::interaction::JoystickAction::Release; }

    throw RuntimeError(std::format("Unknown action '{}'", string));
}

} // namespace ghoul

#endif // __OPENSPACE_CORE___JOYSTICKINPUTSTATE___H__
