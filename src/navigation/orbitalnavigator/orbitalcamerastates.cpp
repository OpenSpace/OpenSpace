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

#include <openspace/navigation/orbitalnavigator/orbitalcamerastates.h>

namespace openspace::interaction {

template <typename T>
OrbitalCameraStates::CameraInteractionState<T>::CameraInteractionState(double scaleFactor)
    : velocity(scaleFactor, 1.0)
{}

OrbitalCameraStates::OrbitalCameraStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationState(velocityScaleFactor)
    , _localRotationState(velocityScaleFactor)
    , _truckMovementState(velocityScaleFactor)
    , _localRollState(velocityScaleFactor)
    , _globalRollState(velocityScaleFactor)
{}

void OrbitalCameraStates::setRotationalFriction(bool enabled) {
    _localRotationState.velocity.setFriction(enabled);
    _localRollState.velocity.setFriction(enabled);
    _globalRollState.velocity.setFriction(enabled);
}

void OrbitalCameraStates::setHorizontalFriction(bool enabled) {
    _globalRotationState.velocity.setFriction(enabled);
}

void OrbitalCameraStates::setVerticalFriction(bool enabled) {
    _truckMovementState.velocity.setFriction(enabled);
}

void OrbitalCameraStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void OrbitalCameraStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationState.velocity.setScaleFactor(scaleFactor);
    _localRotationState.velocity.setScaleFactor(scaleFactor);
    _truckMovementState.velocity.setScaleFactor(scaleFactor);
    _localRollState.velocity.setScaleFactor(scaleFactor);
    _globalRollState.velocity.setScaleFactor(scaleFactor);
}

void OrbitalCameraStates::resetVelocities() {
    _globalRotationState.velocity.setImmediate({ 0.0, 0.0 });
    _localRotationState.velocity.setImmediate({ 0.0, 0.0 });
    _truckMovementState.velocity.setImmediate(0.0);
    _localRollState.velocity.setImmediate(0.0);
    _globalRollState.velocity.setImmediate(0.0);
}

bool OrbitalCameraStates::hasNonZeroVelocities(bool checkOnlyMovement) const {
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

glm::dvec2 OrbitalCameraStates::globalRotationVelocity() const{
    return _globalRotationState.velocity.get();
}

glm::dvec2 OrbitalCameraStates::localRotationVelocity() const{
    return _localRotationState.velocity.get();
}

double OrbitalCameraStates::truckMovementVelocity() const{
    return _truckMovementState.velocity.get();
}

double OrbitalCameraStates::localRollVelocity() const{
    return _localRollState.velocity.get();
}

double OrbitalCameraStates::globalRollVelocity() const{
    return _globalRollState.velocity.get();
}

} // namespace openspace::interaction
