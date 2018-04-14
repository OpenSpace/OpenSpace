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

#ifndef __OPENSPACE_CORE___JOYSTICKSTATE___H__
#define __OPENSPACE_CORE___JOYSTICKSTATE___H__

#include <openspace/interaction/inputdevicestates.h>

#include <openspace/interaction/joystickinputstate.h>
#include <ghoul/misc/boolean.h>
#include <map>

namespace openspace::interaction {

class JoystickStates : public InputDeviceStates {
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

    JoystickStates(double sensitivity, double velocityScaleFactor);

    void updateStateFromInput(const InputState& inputState, double deltaTime) override;

    void setAxisMapping(
        int axis,
        AxisType mapping,
        AxisInvert shouldInvert = AxisInvert::No,
        AxisNormalize shouldNormalize = AxisNormalize::No
    );

private:
    struct AxisInformation {
        AxisType type = AxisType::None;
        AxisInvert invert = AxisInvert::No;
        AxisNormalize normalize = AxisNormalize::No;
    };
    std::array<AxisInformation, JoystickInputState::MaxAxes> _axisMapping;
};


} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___JOYSTICKSTATE___H__
