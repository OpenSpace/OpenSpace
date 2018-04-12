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

#include <openspace/interaction/delayedvariable.h>
#include <ghoul/glm.h>

//@TODO(abock):  This file and mousestate.h/cpp should be merged into one common file,
//               the only difference between the two is how the buttons and axis values
//               are interpreted
namespace openspace::interaction {

class InputState;

struct JoystickState {
    JoystickState(double scaleFactor);
    void setFriction(double friction);

    void setVelocityScaleFactor(double scaleFactor);

    DelayedVariable<glm::dvec2, double> velocity;
};

class JoystickStates {
public:
    JoystickStates(double sensitivity, double velocityScaleFactor);
    void updateJoystickStatesFromInput(const InputState& inputState, double deltaTime);
    void setRotationalFriction(double friction);
    void setHorizontalFriction(double friction);
    void setVerticalFriction(double friction);
    void setSensitivity(double sensitivity);
    void setVelocityScaleFactor(double scaleFactor);

    glm::dvec2 globalRotationJoystickVelocity() const;
    glm::dvec2 localRotationJoystickVelocity() const;
    glm::dvec2 truckMovementJoystickVelocity() const;
    glm::dvec2 localRollJoystickVelocity() const;
    glm::dvec2 globalRollJoystickVelocity() const;

private:
    double _sensitivity;

    bool _isInRollMode = false;
    int _rollToggleButton = 6;

    JoystickState _globalRotationJoystickState;
    JoystickState _localRotationJoystickState;
    JoystickState _truckMovementJoystickState;
    JoystickState _localRollJoystickState;
    JoystickState _globalRollJoystickState;
};


} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___JOYSTICKSTATE___H__
