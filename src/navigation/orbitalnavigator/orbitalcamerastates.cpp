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

OrbitalCameraStates::OrbitalCameraStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationVelocity(velocityScaleFactor)
    , _localRotationVelocity(velocityScaleFactor)
    , _truckMovementVelocity(velocityScaleFactor)
    , _localRollVelocity(velocityScaleFactor)
    , _globalRollVelocity(velocityScaleFactor)
{}

void OrbitalCameraStates::setRotationalFriction(bool enabled) {
    _localRotationVelocity.setFriction(enabled);
    _localRollVelocity.setFriction(enabled);
    _globalRollVelocity.setFriction(enabled);
}

void OrbitalCameraStates::setHorizontalFriction(bool enabled) {
    _globalRotationVelocity.setFriction(enabled);
}

void OrbitalCameraStates::setVerticalFriction(bool enabled) {
    _truckMovementVelocity.setFriction(enabled);
}

void OrbitalCameraStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void OrbitalCameraStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationVelocity.setScaleFactor(scaleFactor);
    _localRotationVelocity.setScaleFactor(scaleFactor);
    _truckMovementVelocity.setScaleFactor(scaleFactor);
    _localRollVelocity.setScaleFactor(scaleFactor);
    _globalRollVelocity.setScaleFactor(scaleFactor);
}

void OrbitalCameraStates::resetVelocities() {
    _globalRotationVelocity.setImmediate({ 0.0, 0.0 });
    _localRotationVelocity.setImmediate({ 0.0, 0.0 });
    _truckMovementVelocity.setImmediate(0.0);
    _localRollVelocity.setImmediate(0.0);
    _globalRollVelocity.setImmediate(0.0);
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
    return _globalRotationVelocity.get();
}

glm::dvec2 OrbitalCameraStates::localRotationVelocity() const{
    return _localRotationVelocity.get();
}

double OrbitalCameraStates::truckMovementVelocity() const{
    return _truckMovementVelocity.get();
}

double OrbitalCameraStates::localRollVelocity() const{
    return _localRollVelocity.get();
}

double OrbitalCameraStates::globalRollVelocity() const{
    return _globalRollVelocity.get();
}

} // namespace openspace::interaction
