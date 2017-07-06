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
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    const std::string _loggerCat = "OrbitalNavigator";
}

namespace openspace {
namespace interaction {

// OrbitalNavigator
OrbitalNavigator::MouseStates::MouseStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationMouseState(velocityScaleFactor)
    , _localRotationMouseState(velocityScaleFactor)
    , _truckMovementMouseState(velocityScaleFactor)
    , _localRollMouseState(velocityScaleFactor)
    , _globalRollMouseState(velocityScaleFactor)
{
}

void OrbitalNavigator::MouseStates::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
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

void OrbitalNavigator::MouseStates::setRotationalFriction(double friction) {
    _localRotationMouseState.setFriction(friction);
    _localRollMouseState.setFriction(friction);
    _globalRollMouseState.setFriction(friction);
}

void OrbitalNavigator::MouseStates::setHorizontalFriction(double friction) {
    _globalRotationMouseState.setFriction(friction);
}

void OrbitalNavigator::MouseStates::setVerticalFriction(double friction) {
    _truckMovementMouseState.setFriction(friction);
}

void OrbitalNavigator::MouseStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void OrbitalNavigator::MouseStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _localRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _truckMovementMouseState.setVelocityScaleFactor(scaleFactor);
    _localRollMouseState.setVelocityScaleFactor(scaleFactor);
    _globalRollMouseState.setVelocityScaleFactor(scaleFactor);
}

glm::dvec2 OrbitalNavigator::MouseStates::synchedGlobalRotationMouseVelocity() {
    return _globalRotationMouseState.velocity.get();
}

glm::dvec2 OrbitalNavigator::MouseStates::synchedLocalRotationMouseVelocity() {
    return _localRotationMouseState.velocity.get();
}

glm::dvec2 OrbitalNavigator::MouseStates::synchedTruckMovementMouseVelocity() {
    return _truckMovementMouseState.velocity.get();
}

glm::dvec2 OrbitalNavigator::MouseStates::synchedLocalRollMouseVelocity() {
    return _localRollMouseState.velocity.get();
}

glm::dvec2 OrbitalNavigator::MouseStates::synchedGlobalRollMouseVelocity() {
    return _globalRollMouseState.velocity.get();
}

OrbitalNavigator::OrbitalNavigator(
    std::shared_ptr<MouseStates> mouseStates)
    : _mouseStates(mouseStates)
{
	_followFocusNodeRotationDistance = 2.0;
	_minimumAllowedDistance = 10.0;

    auto smoothStep = 
        [](double t) {
            double res = 3.0 * t*t  - 2.0 * t*t*t;
            return glm::clamp(res, 0.0, 1.0);
        };
    //_followRotationInterpolator.setTransferFunction(smoothStep);


        
    // The transfer function is used here to get a different interpolation than the one
    // obtained from newValue = lerp(0, currentValue, dt). That one will result in an
    // exponentially decreasing value but we want to be able to control it. Either as
    // a linear interpolation or a smooth step interpolation. Therefore we use
    // newValue = lerp(0, currentValue * f(t) * dt) where f(t) is the transfer function
    // and lerp is a linear iterpolation
    // lerp(endValue, startValue, interpolationParameter).
    //
    // The transfer functions are derived from:
    // f(t) = d/dt ( ln(1 / f_orig(t)) ) where f_orig is the transfer function that would
    // be used if the interpolation was sinply linear between a start value and an end
    // value instead of current value and end value (0) as we use it when inerpoláting.
    // As an example f_orig(t) = 1 - t yields f(t) = 1 / (1 - t) which results in a linear
    // interpolation from 1 to 0.

    auto smoothStepDerivedTranferFunction = 
        [](double t) {
            return (6 * (t + t*t) / (1 - 3 * t*t + 2 * t*t*t));
        };
    auto linearDerivedTranferFunction = 
        [](double t) {
            return 1 / (1 - t);
        };
    _rotateToFocusNodeInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);
}

OrbitalNavigator::~OrbitalNavigator() {

}

OrbitalNavigator::CameraRotationDecomposition
	OrbitalNavigator::decomposeCameraRotation(
		glm::dvec3 cameraPosition,
		glm::dquat cameraRotation,
		glm::dvec3 cameraLookUp,
		glm::dvec3 cameraViewDirection)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();
    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));

    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

	glm::dvec3 directionFromSurfaceToCameraModelSpace = posHandle.referenceSurfaceOutDirection;
	glm::dvec3 directionFromSurfaceToCamera =
		glm::normalize(glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);

    // Create the internal representation of the local and global camera rotations
	glm::dmat4 lookAtMat = glm::lookAt(
		glm::dvec3(0.0, 0.0, 0.0),
        -directionFromSurfaceToCamera,
        normalize(cameraViewDirection + cameraLookUp)); // To avoid problem with lookup in up direction
	glm::dquat globalCameraRotation = glm::normalize(glm::quat_cast(inverse(lookAtMat)));
	glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) * cameraRotation;

    return { localCameraRotation, globalCameraRotation };
}

void OrbitalNavigator::performRoll(double deltaTime, glm::dquat& localCameraRotation) {
    glm::dquat rollQuat = glm::angleAxis(
        _mouseStates->synchedLocalRollMouseVelocity().x * deltaTime,
        glm::dvec3(0, 0, 1)
    );
    localCameraRotation = localCameraRotation * rollQuat;
}

void OrbitalNavigator::performLocalRotation(double deltaTime, glm::dquat& localCameraRotation) {
    glm::dvec3 eulerAngles(
        _mouseStates->synchedLocalRotationMouseVelocity().y,
        _mouseStates->synchedLocalRotationMouseVelocity().x,
        0
    );
    glm::dquat rotationDiff = glm::dquat(eulerAngles * deltaTime);
	localCameraRotation = localCameraRotation * rotationDiff;
}

void OrbitalNavigator::interpolateLocalRotation(double deltaTime, glm::dquat& localCameraRotation) {
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

void OrbitalNavigator::performHorizontalTranslationAndRotation(
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



void OrbitalNavigator::performHorizontalTranslation(
    double deltaTime,
    glm::dvec3 objectPosition,
    glm::dquat& focusNodeRotationDiff,
    glm::dvec3& cameraPosition,
    glm::dquat& globalCameraRotation)
{
	using namespace glm;
    // Uniform variables
    double ellipsoidShrinkTerm = 0;
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    // Get position handle
    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    dvec3 surfaceNormal =
        normalize(dmat3(modelTransform) * posHandle.referenceSurfaceOutDirection);

    dvec3 posDiff = cameraPosition - objectPosition;

    dvec3 centerToReferenceSurface = dmat3(modelTransform) * posHandle.centerToReferenceSurface;
    dvec3 centerToActualSurfaceModelSpace = posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    dvec3 centerToActualSurface = dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    double distFromSurfaceToCamera = glm::length(actualSurfaceToCamera);

	double distFromCenterToSurface = length(centerToActualSurface);
	double distFromCenterToCamera = length(posDiff);

    double speedScale =
        distFromCenterToSurface > 0.0 ?
        glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0) :
        1.0;

    // Get rotation in camera space
    glm::dvec3 eulerAngles = glm::dvec3(
        -_mouseStates->synchedGlobalRotationMouseVelocity().y * deltaTime,
        -_mouseStates->synchedGlobalRotationMouseVelocity().x * deltaTime,
        0) * speedScale;
    glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

    // Transform to world space
    glm::dquat rotationDiffWorldSpace =
        globalCameraRotation *
        rotationDiffCamSpace *
        glm::inverse(globalCameraRotation);

    // Rotate and find the difference vector
    glm::dvec3 rotationDiffVec3 =
        (distFromCenterToCamera * surfaceNormal)
         * rotationDiffWorldSpace
        - (distFromCenterToCamera * surfaceNormal);

    // Add difference to position
    cameraPosition += rotationDiffVec3;
}


void OrbitalNavigator::followFocusNodeRotation(
    glm::dvec3 objectPosition,
    glm::dquat& focusNodeRotationDiff,
    glm::dvec3& cameraPosition)
{
    glm::dvec3 posDiff = cameraPosition - objectPosition;
    glm::dvec3 rotationDiffVec3AroundCenter =
        posDiff
        * focusNodeRotationDiff
        - (posDiff);
    cameraPosition += rotationDiffVec3AroundCenter;
}


void OrbitalNavigator::performGlobalRotation(
    glm::dvec3 objectPosition,
    glm::dquat& focusNodeRotationDiff,
    glm::dvec3& cameraPosition,
    glm::dquat& globalCameraRotation)
{
    using namespace glm;

    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    glm::dvec3 directionFromSurfaceToCameraModelSpace = posHandle.referenceSurfaceOutDirection;
    glm::dvec3 directionFromSurfaceToCamera =
      normalize(dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);

    glm::dvec3 lookUpWhenFacingSurface =
        inverse(focusNodeRotationDiff) * globalCameraRotation * glm::dvec3(0.0, 1.0, 0.0);
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0, 0, 0),
        -directionFromSurfaceToCamera,
        lookUpWhenFacingSurface);
    globalCameraRotation =
        glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
}

void OrbitalNavigator::performVerticalTranslation(
    double deltaTime,
    glm::dvec3 objectPosition,
    glm::dvec3& cameraPosition)
{
	using namespace glm;

    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    dvec3 posDiff = cameraPosition - objectPosition;

    dvec3 centerToReferenceSurface = dmat3(modelTransform) * posHandle.centerToReferenceSurface;
    dvec3 centerToActualSurfaceModelSpace = posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    dvec3 centerToActualSurface = dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;

    cameraPosition += -actualSurfaceToCamera *
        _mouseStates->synchedTruckMovementMouseVelocity().y * deltaTime;
}

void OrbitalNavigator::performHorizontalRotation(
    double deltaTime,
    glm::dvec3 cameraPosition,
    glm::dquat& globalCameraRotation)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    glm::dvec3 directionFromSurfaceToCameraModelSpace = posHandle.referenceSurfaceOutDirection;
    glm::dvec3 directionFromSurfaceToCamera =
      glm::normalize(glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);

    glm::dquat cameraRollRotation =
        glm::angleAxis(_mouseStates->synchedGlobalRollMouseVelocity().x * deltaTime, directionFromSurfaceToCamera);
    globalCameraRotation = cameraRollRotation * globalCameraRotation;
}


void OrbitalNavigator::pushToSurface(
    double minHeightAboveGround,
    glm::dvec3 objectPosition,
    glm::dvec3& cameraPosition)
{
	using namespace glm;
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    dvec3 posDiff = cameraPosition - objectPosition;

	dvec3 referenceSurfaceOutDirection = glm::dmat3(modelTransform) * posHandle.referenceSurfaceOutDirection;
    dvec3 centerToReferenceSurface = glm::dmat3(modelTransform) * posHandle.centerToReferenceSurface;
    dvec3 centerToActualSurfaceModelSpace = posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    dvec3 centerToActualSurface = glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    double surfaceToCameraSigned =
        glm::length(actualSurfaceToCamera) *
        glm::sign(dot(actualSurfaceToCamera, referenceSurfaceOutDirection));

    cameraPosition += referenceSurfaceOutDirection *
        glm::max(minHeightAboveGround - surfaceToCameraSigned, 0.0);
}

glm::dquat OrbitalNavigator::interpolateRotationDifferential(
    double deltaTime,
    double interpolationTime,
    glm::dquat rotationDiff,
    glm::dvec3 objectPosition,
    glm::dvec3 cameraPosition)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    double maximumDistanceForRotation = glm::length(
        glm::dmat3(modelTransform) * posHandle.centerToReferenceSurface) * _followFocusNodeRotationDistance;
    double distanceToCamera = glm::length(cameraPosition - objectPosition);

    double interpolationSign = glm::sign(maximumDistanceForRotation - distanceToCamera);

    _followRotationInterpolator.setDeltaTime(interpolationSign * deltaTime);
    _followRotationInterpolator.step();

    return glm::slerp(glm::dquat(glm::dvec3(0.0)), rotationDiff, _followRotationInterpolator.value());
}

void OrbitalNavigator::updateCameraStateFromMouseStates(Camera& camera, double deltaTime) {
    if (_focusNode) {
		using namespace glm;
        // Read the current state of the camera and focusnode
        dvec3 camPos = camera.positionVec3();
        dvec3 centerPos = _focusNode->worldPosition();

        // Follow focus nodes movement
        dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;

        CameraRotationDecomposition camRot = decomposeCameraRotation(
            camPos,
            camera.rotationQuaternion(),
            camera.lookUpVectorWorldSpace(),
            camera.viewDirectionWorldSpace());

        // Rotate with the globe
        dmat3 globeStateMatrix = _focusNode->worldRotationMatrix();
        dquat globeRotation = quat_cast(globeStateMatrix);
        dquat focusNodeRotationDiff = _previousFocusNodeRotation * inverse(globeRotation);
        _previousFocusNodeRotation = globeRotation;

        focusNodeRotationDiff = interpolateRotationDifferential(
            deltaTime,
            1.0,
            focusNodeRotationDiff,
            centerPos,
            camPos);

        performRoll(deltaTime, camRot.localRotation);
        if (_rotateToFocusNodeInterpolator.isInterpolating()) {
            interpolateLocalRotation(deltaTime, camRot.localRotation);
        }
        else {
            performLocalRotation(deltaTime, camRot.localRotation);
        }

        performHorizontalTranslation(
            deltaTime,
            centerPos,
            focusNodeRotationDiff,
            camPos,
            camRot.globalRotation);

        followFocusNodeRotation(
            centerPos,
            focusNodeRotationDiff,
            camPos);

        performGlobalRotation(
            centerPos,
            focusNodeRotationDiff,
            camPos,
            camRot.globalRotation);

		performVerticalTranslation(deltaTime, centerPos, camPos);
		performHorizontalRotation(deltaTime, camPos, camRot.globalRotation);
		pushToSurface(_minimumAllowedDistance, centerPos, camPos);

        // Update the camera state
        camera.setPositionVec3(camPos); 
        camera.setRotation(camRot.globalRotation * camRot.localRotation);
        return;
    }
}

bool OrbitalNavigator::followingNodeRotation() const {
    return _followRotationInterpolator.value() >= 1.0;
}

void OrbitalNavigator::setFollowFocusNodeRotationDistance(
    double followFocusNodeRotationDistance)
{
    _followFocusNodeRotationDistance = followFocusNodeRotationDistance;
}

void OrbitalNavigator::setMinimumAllowedDistance(
    double minimumAllowedDistance)
{
    _minimumAllowedDistance = minimumAllowedDistance;
}

void OrbitalNavigator::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
    _mouseStates->updateMouseStatesFromInput(inputState, deltaTime);
}

void OrbitalNavigator::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;

    if (_focusNode != nullptr) {
        _previousFocusNodePosition = _focusNode->worldPosition();
        _previousFocusNodeRotation = glm::quat_cast(_focusNode->worldRotationMatrix());
    }
}

SceneGraphNode* OrbitalNavigator::focusNode() {
    return _focusNode;
}

void OrbitalNavigator::startInterpolateCameraDirection(const Camera& camera) {
    glm::dvec3 camPos = camera.positionVec3();
    glm::dvec3 camDir = glm::normalize(camera.rotationQuaternion() * glm::dvec3(0, 0, -1));
    glm::dvec3 centerPos = _focusNode->worldPosition();
    glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    double angle = glm::angle(camDir, directionToCenter);

    // Minimum is two second. Otherwise proportional to angle
    _rotateToFocusNodeInterpolator.setInterpolationTime(glm::max(angle * 2.0, 2.0));
    _rotateToFocusNodeInterpolator.start();
}

} // namespace interaction
} // namespace openspace
