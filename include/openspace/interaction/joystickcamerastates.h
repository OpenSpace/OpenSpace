/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_CORE___JOYSTICKCAMERASTATES___H__
#define __OPENSPACE_CORE___JOYSTICKCAMERASTATES___H__

#include <openspace/interaction/camerainteractionstates.h>

#include <openspace/interaction/joystickinputstate.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/stringconversion.h>
#include <map>
#include <vector>

namespace openspace::interaction {

class JoystickCameraStates : public CameraInteractionStates {
public:
    enum class AxisType {
        None = 0,
        OrbitX,
        OrbitY,
        ZoomIn,
        ZoomOut,
        Zoom,
        LocalRollX,
        LocalRollY,
        GlobalRollX,
        GlobalRollY,
        PanX,
        PanY
    };

    BooleanType(AxisInvert);
    BooleanType(AxisNormalize);
    BooleanType(ButtonCommandRemote);

    struct AxisInformation {
        AxisType type = AxisType::None;
        AxisInvert invert = AxisInvert::No;
        AxisNormalize normalize = AxisNormalize::No;

        // The axis values can either go back to 0 when the joystick is released or it can
        // stay at the value it was before the joystick was released.
        // The latter is called a sticky axis, when the values don't go back to 0.
        bool isSticky = false;

        float deadzone = 0.f;

        // Every axis can have their own sensitivity
        double sensitivity = 0.0;
    };

    JoystickCameraStates(double sensitivity, double velocityScaleFactor);

    void updateStateFromInput(const InputState& inputState, double deltaTime) override;

    void setAxisMapping(int axis, AxisType mapping,
        AxisInvert shouldInvert = AxisInvert::No,
        AxisNormalize shouldNormalize = AxisNormalize::No,
        bool isSticky = false, double sensitivity = 0.0
    );

    AxisInformation axisMapping(int axis) const;

    void setDeadzone(int axis, float deadzone);
    float deadzone(int axis) const;


    void bindButtonCommand(int button, std::string command, JoystickAction action,
        ButtonCommandRemote remote, std::string documentation);
    void clearButtonCommand(int button);
    std::vector<std::string> buttonCommand(int button) const;

private:
    // We use an array for the axes and a map for the buttons since the axis are going to
    // be accessed much more often and thus have to be more efficient. And storing a few
    // extra AxisInformation that are not used will not matter that much; finding an axis
    // location in a potential map each frame, however, would

    std::array<AxisInformation, JoystickInputState::MaxAxes> _axisMapping;

    // This array is used to store the old axis values from the previous frame,
    // it is used to calculate the difference in the values in the case of a sticky axis
    std::array<float, JoystickInputState::MaxAxes> _prevAxisValues;

    struct ButtonInformation {
        std::string command;
        JoystickAction action;
        ButtonCommandRemote synchronization;
        std::string documentation;
    };

    std::multimap<int, ButtonInformation> _buttonMapping;
};

} // namespace openspace::interaction

namespace ghoul {

template <>
inline std::string to_string(
                      const openspace::interaction::JoystickCameraStates::AxisType& value)
{
    using T = openspace::interaction::JoystickCameraStates::AxisType;
    switch (value) {
        case T::None:        return "None";
        case T::OrbitX:      return "Orbit X";
        case T::OrbitY:      return "Orbit Y";
        case T::ZoomIn:      return "Zoom In";
        case T::ZoomOut:     return "Zoom Out";
        case T::Zoom:        return "Zoom In and Out";
        case T::LocalRollX:  return "LocalRoll X";
        case T::LocalRollY:  return "LocalRoll Y";
        case T::GlobalRollX: return "GlobalRoll X";
        case T::GlobalRollY: return "GlobalRoll Y";
        case T::PanX:        return "Pan X";
        case T::PanY:        return "Pan Y";
        default:             return "";
        }
}

template <>
constexpr openspace::interaction::JoystickCameraStates::AxisType
from_string(std::string_view string)
{
    using T = openspace::interaction::JoystickCameraStates::AxisType;

    if (string == "None") { return T::None; }
    if (string == "Orbit X") { return T::OrbitX; }
    if (string == "Orbit Y") { return T::OrbitY; }
    if (string == "Zoom In") { return T::ZoomIn; }
    if (string == "Zoom Out") { return T::ZoomOut; }
    if (string == "Zoom") { return T::Zoom; }
    if (string == "LocalRoll X") { return T::LocalRollX; }
    if (string == "LocalRoll Y") { return T::LocalRollY; }
    if (string == "GlobalRoll X") { return T::GlobalRollX; }
    if (string == "GlobalRoll Y") { return T::GlobalRollY; }
    if (string == "Pan X") { return T::PanX; }
    if (string == "Pan Y") { return T::PanY; }

    throw RuntimeError("Unkonwn axis type '" + std::string(string) + "'");
}

} // namespace ghoul

#endif // __OPENSPACE_CORE___JOYSTICKCAMERASTATES___H__
