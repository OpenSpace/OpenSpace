/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/interaction/orbitalinteractionmode.h>

#include <openspace/scene/scenegraphnode.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

namespace {
    const std::string _loggerCat = "OrbitalInteractionMode";
}

namespace openspace {
namespace interaction {

// OrbitalInteractionMode
OrbitalInteractionMode::MouseStates::MouseStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationMouseState(velocityScaleFactor)
    , _localRotationMouseState(velocityScaleFactor)
    , _truckMovementMouseState(velocityScaleFactor)
    , _localRollMouseState(velocityScaleFactor)
    , _globalRollMouseState(velocityScaleFactor)
{}

void OrbitalInteractionMode::MouseStates::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
    glm::dvec2 mousePosition = inputState.getMousePosition();

    bool button1Pressed = inputState.isMouseButtonPressed(MouseButton::Button1);
    bool button2Pressed = inputState.isMouseButtonPressed(MouseButton::Button2);
    bool button3Pressed = inputState.isMouseButtonPressed(MouseButton::Button3);
    bool keyCtrlPressed = inputState.isKeyPressed(Key::LeftControl);
    bool keyShiftPressed = inputState.isKeyPressed(Key::LeftShift);
    
    // Update the mouse states
    if (button1Pressed && !keyShiftPressed) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRotationMouseState.previousPosition - mousePosition;
            _localRotationMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _globalRotationMouseState.previousPosition = mousePosition;
            _globalRotationMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRotationMouseState.previousPosition - mousePosition;
            _globalRotationMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _localRotationMouseState.previousPosition = mousePosition;
            _localRotationMouseState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button1Pressed
        _localRotationMouseState.previousPosition = mousePosition;
        _localRotationMouseState.velocity.decelerate(deltaTime);

        _globalRotationMouseState.previousPosition = mousePosition;
        _globalRotationMouseState.velocity.decelerate(deltaTime);
    }
    if (button2Pressed) {
        glm::dvec2 mousePositionDelta =
            _truckMovementMouseState.previousPosition - mousePosition;
        _truckMovementMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);
    }
    else { // !button2Pressed
        _truckMovementMouseState.previousPosition = mousePosition;
        _truckMovementMouseState.velocity.decelerate(deltaTime);
    }
    if (button3Pressed || (keyShiftPressed && button1Pressed)) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRollMouseState.previousPosition - mousePosition;
            _localRollMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _globalRollMouseState.previousPosition = mousePosition;
            _globalRollMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRollMouseState.previousPosition - mousePosition;
            _globalRollMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _localRollMouseState.previousPosition = mousePosition;
            _localRollMouseState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button3Pressed
        _globalRollMouseState.previousPosition = mousePosition;
        _globalRollMouseState.velocity.decelerate(deltaTime);

        _localRollMouseState.previousPosition = mousePosition;
        _localRollMouseState.velocity.decelerate(deltaTime);
    }
}

void OrbitalInteractionMode::MouseStates::setRotationalFriction(double friction) {
    _localRotationMouseState.setFriction(friction);
    _localRollMouseState.setFriction(friction);
    _globalRollMouseState.setFriction(friction);
}

void OrbitalInteractionMode::MouseStates::setHorizontalFriction(double friction) {
    _globalRotationMouseState.setFriction(friction);
}

void OrbitalInteractionMode::MouseStates::setVerticalFriction(double friction) {
    _truckMovementMouseState.setFriction(friction);
}

void OrbitalInteractionMode::MouseStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void OrbitalInteractionMode::MouseStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _localRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _truckMovementMouseState.setVelocityScaleFactor(scaleFactor);
    _localRollMouseState.setVelocityScaleFactor(scaleFactor);
    _globalRollMouseState.setVelocityScaleFactor(scaleFactor);
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedGlobalRotationMouseVelocity() {
    return _globalRotationMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedLocalRotationMouseVelocity() {
    return _localRotationMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedTruckMovementMouseVelocity() {
    return _truckMovementMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedLocalRollMouseVelocity() {
    return _localRollMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedGlobalRollMouseVelocity() {
    return _globalRollMouseState.velocity.get();
}

OrbitalInteractionMode::OrbitalInteractionMode(
    std::shared_ptr<MouseStates> mouseStates)
    : InteractionMode()
    , _mouseStates(mouseStates) {
        
}

OrbitalInteractionMode::~OrbitalInteractionMode() {

}

void OrbitalInteractionMode::updateCameraStateFromMouseStates(Camera& camera, double deltaTime) {
    using namespace glm;
    if (_focusNode) {
        // Read the current state of the camera and focus node
        dvec3 camPos = camera.positionVec3();
        
        // Follow focus nodes movement
        dvec3 centerPos = _focusNode->worldPosition();
        dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;

        dquat totalRotation = camera.rotationQuaternion();
        dvec3 directionToCenter = normalize(centerPos - camPos);
        dvec3 lookUp = camera.lookUpVectorWorldSpace();
        double boundingSphere = _focusNode->boundingSphere();
        dvec3 camDirection = camera.viewDirectionWorldSpace();

        // Declare other variables used in interaction calculations
        double minHeightAboveBoundingSphere = 1;
        dvec3 centerToCamera = camPos - centerPos;
        dvec3 centerToBoundingSphere;

        // Create the internal representation of the local and global camera rotations
        dmat4 lookAtMat = lookAt(
            dvec3(0, 0, 0),
            directionToCenter,
            normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
        dquat globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        dquat localCameraRotation = inverse(globalCameraRotation) * totalRotation;

        { // Do local roll
            glm::dquat cameraRollRotation =
                glm::angleAxis(_mouseStates->synchedLocalRollMouseVelocity().x * deltaTime, dvec3(0, 0, 1));
            localCameraRotation = localCameraRotation * cameraRollRotation;
        }
        if (!_rotateToFocusNodeInterpolator.isInterpolating())
        { // Do local rotation
            dvec3 eulerAngles(_mouseStates->synchedLocalRotationMouseVelocity().y, _mouseStates->synchedLocalRotationMouseVelocity().x, 0);
            dquat rotationDiff = dquat(eulerAngles * deltaTime);

            localCameraRotation = localCameraRotation * rotationDiff;
        }
        else
        { // Interpolate local rotation to focus node
            double t = _rotateToFocusNodeInterpolator.value();
            localCameraRotation = slerp(localCameraRotation, dquat(dvec3(0.0)), glm::min(t * deltaTime, 1.0));
            _rotateToFocusNodeInterpolator.step(deltaTime);
            //if (t > 0.999) {
            //    _rotateToFocusNodeInterpolator.end();
            //}
        }
        { // Do global rotation
            dvec2 smoothMouseVelocity = _mouseStates->synchedGlobalRotationMouseVelocity();
            dvec3 eulerAngles(-smoothMouseVelocity.y, -smoothMouseVelocity.x, 0);
            dquat rotationDiffCamSpace = dquat(eulerAngles * deltaTime);

            dquat newRotationCamspace = globalCameraRotation * rotationDiffCamSpace;
            dquat rotationDiffWorldSpace = newRotationCamspace * inverse(globalCameraRotation); 
            dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace - centerToCamera;

            camPos += rotationDiffVec3;
            dvec3 centerToCamera = camPos - centerPos;
            directionToCenter = normalize(-centerToCamera);

            dvec3 lookUpWhenFacingCenter =
                globalCameraRotation * dvec3(camera.lookUpVectorCameraSpace());
            dmat4 lookAtMat = lookAt(
                dvec3(0, 0, 0),
                directionToCenter,
                lookUpWhenFacingCenter);
            globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            centerToBoundingSphere =
                -directionToCenter *
                boundingSphere;
            camPos += -(centerToCamera - centerToBoundingSphere) *
                _mouseStates->synchedTruckMovementMouseVelocity().y * deltaTime;
        }
        { // Roll around sphere normal
            dquat cameraRollRotation =
                angleAxis(_mouseStates->synchedGlobalRollMouseVelocity().x * deltaTime, -directionToCenter);
            globalCameraRotation = cameraRollRotation * globalCameraRotation;
        }
        { // Push up to surface
            dvec3 sphereSurfaceToCamera = camPos - (centerPos + centerToBoundingSphere);

            double distFromSphereSurfaceToCamera = length(sphereSurfaceToCamera);
            camPos += -directionToCenter *
                max(minHeightAboveBoundingSphere - distFromSphereSurfaceToCamera, 0.0);
        }
      
        // Update the camera state
        camera.setPositionVec3(camPos);
        camera.setRotation(globalCameraRotation * localCameraRotation);
    }
}

bool OrbitalInteractionMode::followingNodeRotation() const {
    return false;
}

void OrbitalInteractionMode::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
    _mouseStates->updateMouseStatesFromInput(inputState, deltaTime);
}

} // namespace interaction
} // namespace openspace
