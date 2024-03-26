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
        LocalRoll,
        GlobalRoll,
        PanX,
        PanY,
        Property
    };

    enum class JoystickType {
        JoystickLike,
        TriggerLike
    };

    BooleanType(AxisInvert);
    BooleanType(ButtonCommandRemote);
    BooleanType(AxisFlip);

    struct AxisInformation {
        AxisType type = AxisType::None;
        AxisInvert invert = AxisInvert::No;
        JoystickType joystickType = JoystickType::JoystickLike;
        AxisFlip flip = AxisFlip::No;

        // The axis values can either go back to 0 when the joystick is released or it can
        // stay at the value it was before the joystick was released.
        // The latter is called a sticky axis, when the values don't go back to 0.
        bool isSticky = false;

        float deadzone = 0.f;

        // Every axis can have their own sensitivity
        double sensitivity = 0.0;

        // The property info if the type is Property
        std::string propertyUri;
        float minValue = 0.f;
        float maxValue = 1.f;
        bool isRemote = true;
    };

    JoystickCameraStates(double sensitivity, double velocityScaleFactor);

    void updateStateFromInput(
        const JoystickInputStates& joystickInputStates, double deltaTime);

    void setAxisMapping(const std::string& joystickName, int axis, AxisType mapping,
        AxisInvert shouldInvert = AxisInvert::No,
        JoystickType joystickType = JoystickType::JoystickLike,
        bool isSticky = false, AxisFlip shouldFlip = AxisFlip::No,
        double sensitivity = 0.0
    );

    void setAxisMappingProperty(const std::string& joystickName, int axis,
        std::string propertyUri, float min = 0.f, float max = 1.f,
        AxisInvert shouldInvert = AxisInvert::No, bool isRemote = true
    );

    AxisInformation axisMapping(const std::string& joystickName, int axis) const;

    void setDeadzone(const std::string& joystickName, int axis, float deadzone);
    float deadzone(const std::string& joystickName, int axis) const;

    void bindButtonCommand(const std::string& joystickName, int button,
        std::string command, JoystickAction action, ButtonCommandRemote remote,
        std::string documentation);
    void clearButtonCommand(const std::string& joystickName, int button);
    std::vector<std::string> buttonCommand(const std::string& joystickName,
        int button) const;

private:
    struct JoystickCameraState {
        std::string joystickName;

        // We use a vector for the axes and a map for the buttons since the axis are going
        // to be accessed much more often and thus have to be more efficient
        std::vector<AxisInformation> axisMapping;

        // This vector is used to store the old axis values from the previous frame, it is
        // used to calculate the difference in the values in the case of a sticky axis
        std::vector<float> prevAxisValues;

        struct ButtonInformation {
            // The script that is run when the button is activated
            std::string command;

            // When is the button considered activated
            JoystickAction action;

            // If the script should be syncronised to other remote sessions or not
            ButtonCommandRemote synchronization;

            // Short documentation on what the script of this button does
            std::string documentation;
        };

        std::multimap<int, ButtonInformation> buttonMapping;
    };

    std::vector<JoystickCameraState> _joystickCameraStates;

    // Find the item in _joystickCameraStates that corresponds to the given joystickName
    // return a pointer to the item, if not found then return nullptr
    JoystickCameraState* joystickCameraState(const std::string& joystickName);
    const JoystickCameraState* joystickCameraState(const std::string& joystickName) const;

    // Ues getJoystickCameraState(name) to find the joystickCameraState that
    // corresponds to the given joystickName. If not found then add a new item if possible
    JoystickCameraState* findOrAddJoystickCameraState(const std::string& joystickName);
};

} // namespace openspace::interaction

namespace ghoul {

template <>
inline std::string to_string(
                      const openspace::interaction::JoystickCameraStates::AxisType& value)
{
    using T = openspace::interaction::JoystickCameraStates::AxisType;
    switch (value) {
        case T::None:       return "None";
        case T::OrbitX:     return "Orbit X";
        case T::OrbitY:     return "Orbit Y";
        case T::ZoomIn:     return "Zoom In";
        case T::ZoomOut:    return "Zoom Out";
        case T::Zoom:       return "Zoom In and Out";
        case T::LocalRoll:  return "LocalRoll";
        case T::GlobalRoll: return "GlobalRoll";
        case T::PanX:       return "Pan X";
        case T::PanY:       return "Pan Y";
        case T::Property:   return "Property";
        default:            return "";
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
    if (string == "LocalRoll") { return T::LocalRoll; }
    if (string == "GlobalRoll") { return T::GlobalRoll; }
    if (string == "Pan X") { return T::PanX; }
    if (string == "Pan Y") { return T::PanY; }
    if (string == "Property") { return T::Property; }

    throw RuntimeError(std::format("Unknown axis type '{}'", string), "Joystick");
}

template <>
inline std::string to_string(
    const openspace::interaction::JoystickCameraStates::JoystickType& value)
{
    using T = openspace::interaction::JoystickCameraStates::JoystickType;
    switch (value) {
        case T::JoystickLike: return "JoystickLike";
        case T::TriggerLike:  return "TriggerLike";
        default:              return "";
    }
}

template <>
constexpr openspace::interaction::JoystickCameraStates::JoystickType
from_string(std::string_view string)
{
    using T = openspace::interaction::JoystickCameraStates::JoystickType;

    if (string == "JoystickLike") { return T::JoystickLike; }
    if (string == "TriggerLike") { return T::TriggerLike; }

    throw RuntimeError(std::format("Unknown joystick type '{}'", string), "Joystick");
}

} // namespace ghoul

#endif // __OPENSPACE_CORE___JOYSTICKCAMERASTATES___H__
