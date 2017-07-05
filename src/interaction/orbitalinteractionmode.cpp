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

OrbitalInteractionMode::CameraRotationDecomposition
	OrbitalInteractionMode::decomposeCameraRotation(
		glm::dvec3 cameraPosition,
		glm::dquat cameraRotation,
		glm::dvec3 cameraLookUp,
		glm::dvec3 cameraViewDirection)
{
    // Read the current state of the camera and focus node
    glm::dvec3 camPos = cameraPosition;
    glm::dvec3 centerPos = _focusNode->worldPosition();
	glm::dvec3 directionToCenter = normalize(centerPos - camPos);

    // Create the internal representation of the local and global camera rotations
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0, 0, 0),
        directionToCenter,
        normalize(cameraViewDirection + cameraLookUp)); // To avoid problem with lookup in up direction
    glm::dquat globalCameraRotation = glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
    glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) * cameraRotation;

    return { localCameraRotation, globalCameraRotation };
}

void OrbitalInteractionMode::performRoll(double deltaTime, glm::dquat& localCameraRotation) {
    glm::dquat rollQuat = glm::angleAxis(
        _mouseStates->synchedLocalRollMouseVelocity().x * deltaTime,
        glm::dvec3(0, 0, 1)
    );
    localCameraRotation = localCameraRotation * rollQuat;
}

void OrbitalInteractionMode::performLocalRotation(double deltaTime, glm::dquat& localCameraRotation) {
    glm::dvec3 eulerAngles(
        _mouseStates->synchedLocalRotationMouseVelocity().y,
        _mouseStates->synchedLocalRotationMouseVelocity().x,
        0
    );
    glm::dquat rotationDiff = glm::dquat(eulerAngles * deltaTime);
	localCameraRotation = localCameraRotation * rotationDiff;
}

void OrbitalInteractionMode::interpolateLocalRotation(double deltaTime, glm::dquat& localCameraRotation) {
    double t = _rotateToFocusNodeInterpolator.value();
    _rotateToFocusNodeInterpolator.setDeltaTime(deltaTime);
    _rotateToFocusNodeInterpolator.step();
	localCameraRotation = glm::slerp(
        localCameraRotation,
        glm::dquat(glm::dvec3(0.0)),
        glm::min(t * _rotateToFocusNodeInterpolator.deltaTimeScaled(), 1.0));

    if (angle(localCameraRotation) < 0.01) {
        _rotateToFocusNodeInterpolator.end();
    }
}

void OrbitalInteractionMode::performHorizontalTranslationAndRotation(
	double deltaTime,
	glm::dvec3 objectPosition,
	glm::dvec3& cameraPosition,
	glm::dquat& globalCameraRotation)
{
	glm::dvec3 centerToCamera = cameraPosition - objectPosition;

    glm::dvec2 smoothMouseVelocity = _mouseStates->synchedGlobalRotationMouseVelocity();
    glm::dvec3 eulerAngles(-smoothMouseVelocity.y, -smoothMouseVelocity.x, 0);
    glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles * deltaTime);

    glm::dquat newRotationCamspace = globalCameraRotation * rotationDiffCamSpace;
    glm::dquat rotationDiffWorldSpace = newRotationCamspace * inverse(globalCameraRotation); 
    glm::dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace - centerToCamera;

	cameraPosition += rotationDiffVec3;
	centerToCamera = cameraPosition - objectPosition;
	glm::dvec3 directionToCenter = normalize(-centerToCamera);

    glm::dvec3 lookUpWhenFacingCenter =
        globalCameraRotation * glm::dvec3(0.0, 1.0, 0.0);
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0, 0, 0),
        directionToCenter,
        lookUpWhenFacingCenter);
	globalCameraRotation = glm::normalize(quat_cast(inverse(lookAtMat)));
}

void OrbitalInteractionMode::performVerticalTranslation(
    double deltaTime,
    double boundingSphere,
    glm::dvec3 objectPosition,
    glm::dvec3& cameraPosition)
{
    glm::dvec3 centerToCamera = cameraPosition - objectPosition;
    glm::dvec3 directionToCenter = normalize(-centerToCamera);
    glm::dvec3 centerToBoundingSphere = -directionToCenter * boundingSphere;
    
    cameraPosition += -(centerToCamera - centerToBoundingSphere) *
        _mouseStates->synchedTruckMovementMouseVelocity().y * deltaTime;
}

void OrbitalInteractionMode::performHorizontalRotation(
    double deltaTime,
    glm::dvec3 objectPosition,
    glm::dvec3& cameraPosition,
    glm::dquat& globalCameraRotation)
{
    glm::dvec3 centerToCamera = cameraPosition - objectPosition;
    glm::dvec3 directionToCenter = normalize(-centerToCamera);
    
    glm::dquat cameraRollRotation =
        angleAxis(_mouseStates->synchedGlobalRollMouseVelocity().x * deltaTime,
            -directionToCenter);
    globalCameraRotation = cameraRollRotation * globalCameraRotation;
}


void OrbitalInteractionMode::pushToSurface(
    double deltaTime,
    double boundingSphere,
    glm::dvec3 objectPosition,
    glm::dvec3& cameraPosition)
{
    double minHeightAboveBoundingSphere = 1;

    glm::dvec3 centerToCamera = cameraPosition - objectPosition;
    glm::dvec3 directionToCenter = normalize(-centerToCamera);
    glm::dvec3 centerToBoundingSphere = -directionToCenter * boundingSphere;

    glm::dvec3 sphereSurfaceToCamera = cameraPosition -
        (objectPosition + centerToBoundingSphere);

    double distFromSphereSurfaceToCamera = length(sphereSurfaceToCamera);
    cameraPosition += -directionToCenter *
        glm::max(minHeightAboveBoundingSphere - distFromSphereSurfaceToCamera, 0.0);     
}

void OrbitalInteractionMode::updateCameraStateFromMouseStates(Camera& camera, double deltaTime) {
    if (_focusNode) {
        // Read the current state of the camera and focus node
        glm::dvec3 camPos = camera.positionVec3();
        glm::dvec3 centerPos = _focusNode->worldPosition();
        double boundingSphere = _focusNode->boundingSphere();
        
        // Follow focus nodes movement
        glm::dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;
        
        CameraRotationDecomposition camRot = decomposeCameraRotation(
            camPos,
            camera.rotationQuaternion(),
            camera.lookUpVectorWorldSpace(),
            camera.viewDirectionWorldSpace());

        performRoll(deltaTime, camRot.localRotation);
		if (_rotateToFocusNodeInterpolator.isInterpolating()) {
			interpolateLocalRotation(deltaTime, camRot.localRotation);
		}
        else {
            performLocalRotation(deltaTime, camRot.localRotation);
        }

        performHorizontalTranslationAndRotation(
            deltaTime,
            centerPos,
            camPos,
            camRot.globalRotation
        );

        performVerticalTranslation(deltaTime, boundingSphere, centerPos, camPos);
        performHorizontalRotation(deltaTime, centerPos, camPos, camRot.globalRotation);
        pushToSurface(deltaTime, boundingSphere, centerPos, camPos);

        // Update the camera state (re-combine the global and local rotation)
        camera.setPositionVec3(camPos);
        camera.setRotation(camRot.globalRotation * camRot.localRotation);
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
