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

#include <openspace/interaction/joystickstate.h>

#include <openspace/interaction/inputstate.h>

namespace openspace::interaction {

JoystickStates::JoystickStates(double sensitivity, double velocityScaleFactor)
    : InputDeviceStates(sensitivity, velocityScaleFactor)
{
    _axisMapping[0] = { AxisType::OrbitX, AxisInvert::No, AxisNormalize::No };
    _axisMapping[1] = { AxisType::OrbitY, AxisInvert::No, AxisNormalize::No };
    _axisMapping[2] = { AxisType::PanX, AxisInvert::Yes, AxisNormalize::No };
    _axisMapping[3] = { AxisType::PanY, AxisInvert::Yes, AxisNormalize::No };
    _axisMapping[4] = { AxisType::ZoomIn, AxisInvert::No, AxisNormalize::Yes };
    _axisMapping[5] = { AxisType::ZoomOut, AxisInvert::No, AxisNormalize::Yes };
}

void JoystickStates::updateStateFromInput(const InputState& inputState, double deltaTime)
{
    glm::dvec2 globalRotation;
    double zoom = 0.0;
    //glm::dvec2 truckMovement;
    glm::dvec2 localRoll;
    glm::dvec2 globalRoll;
    glm::dvec2 localRotation;

    for (int i = 0; i < JoystickInputState::MaxAxes; ++i) {
        AxisInformation t = _axisMapping[i];
        if (t.type == AxisType::None) {
            continue;
        }

        float value = inputState.joystickAxis(i);
        
        if (t.normalize) {
            value = (value + 1.f) / 2.f;
        }

        if (t.invert) {
            value *= -1.f;
        }

        value *= _sensitivity;

        switch (t.type) {
            case AxisType::None:
                break;
            case AxisType::OrbitX:
                globalRotation.x = value;
                break;
            case AxisType::OrbitY:
                globalRotation.y = value;
                break;
            case AxisType::ZoomIn:
                zoom += value;
                break;
            case AxisType::ZoomOut:
                zoom -= value;
                break;
            case AxisType::LocalRollX:
                localRoll.x = value;
                break;
            case AxisType::LocalRollY:
                localRoll.y = value;
                break;
            case AxisType::GlobalRollX:
                globalRoll.x = value;
                break;
            case AxisType::GlobalRollY:
                globalRoll.y = value;
                break;
            case AxisType::PanX:
                localRotation.x = value;
                break;
            case AxisType::PanY:
                localRotation.y = value;
                break;
        }
    }

    _globalRotationState.velocity.set(globalRotation, deltaTime);
    _truckMovementState.velocity.set(glm::dvec2(zoom), deltaTime);
    _localRollState.velocity.set(localRoll, deltaTime);
    _globalRollState.velocity.set(globalRoll, deltaTime);
    _localRotationState.velocity.set(localRotation, deltaTime);

    // Rolling
    //_globalRotationState.velocity.set(
    //    glm::dvec2(
    //        inputState.joystickAxis(0) * _sensitivity,
    //        inputState.joystickAxis(1) * _sensitivity
    //    ),
    //    deltaTime
    //);

    //// Zooming
    //float zoomOut = (inputState.joystickAxis(4) + 1.f) / 2.f;
    //float zoomIn = (inputState.joystickAxis(5) + 1.f) / 2.f;
    //float zoom = -zoomOut + zoomIn;

    //_truckMovementState.velocity.set(
    //    glm::dvec2(
    //        zoom * _sensitivity
    //    ),
    //    deltaTime
    //);

    //if (inputState.joystickButton(_rollToggleButton)) {
    //    _isInRollMode = !_isInRollMode;
    //}

    //if (_isInRollMode) {
    //    _globalRollState.velocity.set(
    //        glm::dvec2(
    //            -inputState.joystickAxis(2) * _sensitivity,
    //            inputState.joystickAxis(3) * _sensitivity
    //        ),
    //        deltaTime
    //    );

    //    _localRotationState.velocity.decelerate(deltaTime);
    //}
    //else {
    //    // Panning
    //    // The x-axis in panning is inverted
    //    _localRotationState.velocity.set(
    //        glm::dvec2(
    //            -inputState.joystickAxis(2) * _sensitivity,
    //            inputState.joystickAxis(3) * _sensitivity
    //        ),
    //        deltaTime
    //    );

    //    _globalRollState.velocity.decelerate(deltaTime);
    //}
}

void JoystickStates::setAxisMapping(int axis, AxisType mapping, AxisInvert shouldInvert,
                                    AxisNormalize shouldNormalize)
{
    ghoul_assert(axis < JoystickInputState::MaxAxes, "axis must be < MaxAxes");

    _axisMapping[axis] = { mapping, shouldInvert, shouldNormalize };
}


} // namespace openspace::interaction
