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

#include <openspace/interaction/camerainteractionstates.h>

#include <openspace/interaction/inputstate.h>

namespace openspace::interaction {

CameraInteractionStates::InteractionState::InteractionState(double scaleFactor)
    : previousPosition(0.0, 0.0)
    , velocity(scaleFactor, 1)
{}

void CameraInteractionStates::InteractionState::setFriction(double friction) {
    velocity.setFriction(friction);
}

void CameraInteractionStates::InteractionState::setVelocityScaleFactor(double scaleFactor)
{
    velocity.setScaleFactor(scaleFactor);
}

CameraInteractionStates::CameraInteractionStates(double sensitivity,
                                                 double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationState(velocityScaleFactor)
    , _localRotationState(velocityScaleFactor)
    , _truckMovementState(velocityScaleFactor)
    , _localRollState(velocityScaleFactor)
    , _globalRollState(velocityScaleFactor)
{}

void CameraInteractionStates::setRotationalFriction(double friction) {
    _localRotationState.setFriction(friction);
    _localRollState.setFriction(friction);
    _globalRollState.setFriction(friction);
}

void CameraInteractionStates::setHorizontalFriction(double friction) {
    _globalRotationState.setFriction(friction);
}

void CameraInteractionStates::setVerticalFriction(double friction) {
    _truckMovementState.setFriction(friction);
}

void CameraInteractionStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void CameraInteractionStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationState.setVelocityScaleFactor(scaleFactor);
    _localRotationState.setVelocityScaleFactor(scaleFactor);
    _truckMovementState.setVelocityScaleFactor(scaleFactor);
    _localRollState.setVelocityScaleFactor(scaleFactor);
    _globalRollState.setVelocityScaleFactor(scaleFactor);
}

glm::dvec2 CameraInteractionStates::globalRotationVelocity() const{
    return _globalRotationState.velocity.get();
}

glm::dvec2 CameraInteractionStates::localRotationVelocity() const{
    return _localRotationState.velocity.get();
}

glm::dvec2 CameraInteractionStates::truckMovementVelocity() const{
    return _truckMovementState.velocity.get();
}

glm::dvec2 CameraInteractionStates::localRollVelocity() const{
    return _localRollState.velocity.get();
}

glm::dvec2 CameraInteractionStates::globalRollVelocity() const{
    return _globalRollState.velocity.get();
}

} // namespace openspace::interaction
