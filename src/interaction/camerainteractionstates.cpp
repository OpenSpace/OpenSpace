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

#include <openspace/interaction/camerainteractionstates.h>

namespace openspace::interaction {

template <typename T>
CameraInteractionStates::InteractionState<T>::InteractionState(double scaleFactor)
    : previousValue(T(0.0))
    , velocity(scaleFactor, 1)
{}

template <typename T>
void CameraInteractionStates::InteractionState<T>::setFriction(double friction) {
    velocity.setFriction(friction);
}

template <typename T>
void CameraInteractionStates::InteractionState<T>::setVelocityScaleFactor(
                                                                       double scaleFactor)
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

void CameraInteractionStates::resetVelocities() {
    _globalRotationState.velocity.setHard({ 0.0, 0.0 });
    _localRotationState.velocity.setHard({ 0.0, 0.0 });
    _truckMovementState.velocity.setHard(0.0);
    _localRollState.velocity.setHard(0.0);
    _globalRollState.velocity.setHard(0.0);
}

bool CameraInteractionStates::hasNonZeroVelocities(bool checkOnlyMovement) const {
    glm::dvec2 sum = glm::dvec2(0.0);
    sum += globalRotationVelocity();
    sum += truckMovementVelocity();
    if (!checkOnlyMovement) {
        sum += localRotationVelocity();
        sum += localRotationVelocity();
        sum += localRollVelocity();
        sum += globalRollVelocity();
    }
    // Epsilon size based on that even if no interaction is happening,
    // there might still be some residual velocity in the variables
    return glm::length(sum) > 0.001;
}

glm::dvec2 CameraInteractionStates::globalRotationVelocity() const{
    return _globalRotationState.velocity.get();
}

glm::dvec2 CameraInteractionStates::localRotationVelocity() const{
    return _localRotationState.velocity.get();
}

double CameraInteractionStates::truckMovementVelocity() const{
    return _truckMovementState.velocity.get();
}

double CameraInteractionStates::localRollVelocity() const{
    return _localRollState.velocity.get();
}

double CameraInteractionStates::globalRollVelocity() const{
    return _globalRollState.velocity.get();
}

} // namespace openspace::interaction
