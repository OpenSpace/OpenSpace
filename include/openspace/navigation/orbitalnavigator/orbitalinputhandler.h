/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___ORBITALINPUTHANDLER___H__
#define __OPENSPACE_CORE___ORBITALINPUTHANDLER___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/navigation/orbitalnavigator/joystickcamerastates.h>
#include <openspace/navigation/orbitalnavigator/mousecamerastates.h>
#include <openspace/navigation/orbitalnavigator/scriptcamerastates.h>
#include <openspace/navigation/orbitalnavigator/websocketcamerastates.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

namespace openspace::interaction {


class OrbitalInputHandler : public properties::PropertyOwner {
public:
    OrbitalInputHandler(double friction);

    JoystickCameraStates& joystickStates();
    const JoystickCameraStates& joystickStates() const;

    WebsocketCameraStates& websocketStates();
    const WebsocketCameraStates& websocketStates() const;

    ScriptCameraStates& scriptStates();
    const ScriptCameraStates& scriptStates() const;

    bool hasNonZeroVelocity() const;
    bool hasTranslationalVelocity() const;
    void resetVelocities();

    double localRollVelocity() const;
    double globalRollVelocity() const;
    glm::dvec2 localRotationVelocity() const;
    glm::dvec2 globalRotationVelocity() const;
    double truckMovementVelocity() const;

    void updateStatesFromInput(double deltaTime);

    void updateFrictionFactor(double friction);
    void setRollFrictionEnabled(bool enabled);
    void setRotationalFrictionEnabled(bool enabled);
    void setZoomFrictionEnabled(bool enabled);

private:
    properties::FloatProperty _mouseSensitivity;
    properties::FloatProperty _joystickSensitivity;
    properties::FloatProperty _websocketSensitivity;

    properties::BoolProperty _invertMouseButtons;

    MouseCameraStates _mouseStates;
    JoystickCameraStates _joystickStates;
    WebsocketCameraStates _websocketStates;
    ScriptCameraStates _scriptStates;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___ORBITALINPUTHANDLER___H__
