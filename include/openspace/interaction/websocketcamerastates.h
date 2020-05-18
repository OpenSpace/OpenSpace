/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___WEBSOCKETCAMERASTATES___H__
#define __OPENSPACE_CORE___WEBSOCKETCAMERASTATES___H__

#include <openspace/interaction/camerainteractionstates.h>

#include <openspace/interaction/websocketinputstate.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/stringconversion.h>
#include <map>
#include <vector>

namespace openspace::interaction {

class WebsocketCameraStates : public CameraInteractionStates {
public:
    enum class AxisType {
        None = 0,
        OrbitX,
        OrbitY,
        ZoomIn,
        ZoomOut,
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

        float deadzone = 0.f;
    };

    WebsocketCameraStates(double sensitivity, double velocityScaleFactor);

    void updateStateFromInput(const InputState& inputState, double deltaTime) override;

    void setAxisMapping(int axis, AxisType mapping,
        AxisInvert shouldInvert = AxisInvert::No,
        AxisNormalize shouldNormalize = AxisNormalize::No
    );

    AxisInformation axisMapping(int axis) const;

    void setDeadzone(int axis, float deadzone);
    float deadzone(int axis) const;


    void bindButtonCommand(int button, std::string command, WebsocketAction action,
        ButtonCommandRemote remote, std::string documentation);
    void clearButtonCommand(int button);
    std::vector<std::string> buttonCommand(int button) const;

private:
    // We use an array for the axes and a map for the buttons since the axis are going to
    // be accessed much more often and thus have to be more efficient. And storing a few
    // extra AxisInformation that are not used will not matter that much; finding an axis
    // location in a potential map each frame, however, would

    std::array<AxisInformation, WebsocketInputState::MaxAxes> _axisMapping;

    struct ButtonInformation {
        std::string command;
        WebsocketAction action;
        ButtonCommandRemote synchronization;
        std::string documentation;
    };

    std::multimap<int, ButtonInformation> _buttonMapping;
};

} // namespace openspace::interaction

namespace ghoul {

template <>
std::string to_string(
    const openspace::interaction::WebsocketCameraStates::AxisType& type);

template <>
openspace::interaction::WebsocketCameraStates::AxisType
from_string(const std::string& string);

} // namespace ghoul

#endif // __OPENSPACE_CORE___WEBSOCKETCAMERASTATES___H__
