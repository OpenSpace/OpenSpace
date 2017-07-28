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

#include <openspace/interaction/orbitalnavigator.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    const char* _loggerCat = "OrbitalNavigator";

    static const openspace::properties::Property::PropertyInfo RollFrictionInfo = {
        "RollFriction",
        "Roll Friction",
        "If this is enabled, a small friction is applied to the rolling part of the "
        "camera motion, thus slowing it down within a small time period. If this value "
        "is disabled, the camera will roll forever."
    };

    static const openspace::properties::Property::PropertyInfo RotationalFrictionInfo = {
        "RotationalFriction",
        "Rotational Friction",
        "If this is enabled, a small friction is applied to the rotational part of the "
        "camera motion, thus slowing it down within a small time period. If this value "
        "is disabled, the camera will rotate forever."
    };

    static const openspace::properties::Property::PropertyInfo ZoomFrictionInfo = {
        "ZoomFriction",
        "Zoom Friction",
        "If this is enabled, a small friction is applied to the zoom part of the camera "
        "motion, thus slowing it down within a small time period. If this value is "
        "disabled, the camera will zoom in or out forever."
    };

    static const openspace::properties::Property::PropertyInfo SensitivityInfo = {
        "Sensitivity",
        "Sensitivity",
        "Determines the sensitivity of the camera motion. The lower the sensitivity is "
        "the less impact a mouse mothion will have."
    };

    static const openspace::properties::Property::PropertyInfo FrictionInfo = {
        "Friction",
        "Friction Factor",
        "Determines the factor that is applied if the 'Roll Friction', 'Rotational "
        "Friction', and 'Zoom Friction' values are enabled. The lower this value is, the "
        "faster the camera movements will stop."
    };

    static const openspace::properties::Property::PropertyInfo FollowFocusNodeInfo = {
        "FollowFocusNodeRotationDistance",
        "Follow focus node rotation distance",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MinimumDistanceInfo = {
        "MinimumAllowedDistance",
        "Minimum allowed distance",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace::interaction {

OrbitalNavigator::OrbitalNavigator()
    : properties::PropertyOwner("OrbitalNavigator")
    , _rollFriction(RollFrictionInfo, true)
    , _rotationalFriction(RotationalFrictionInfo, true)
    , _zoomFriction(ZoomFrictionInfo, true)
    , _followFocusNodeRotationDistance(FollowFocusNodeInfo, 2.0f, 0.0f, 10.f)
    , _minimumAllowedDistance(MinimumDistanceInfo, 10.0f, 0.0f, 10000.f)
    , _sensitivity(SensitivityInfo, 20.0f, 1.0f, 50.f)
    , _motionLag(FrictionInfo, 0.5f, 0.f, 1.f)
    , _mouseStates(_sensitivity * pow(10.0,-4), 1 / (_motionLag + 0.0000001))
{
    auto smoothStep = 
        [](double t) {
            double res = 3.0 * t*t  - 2.0 * t*t*t;
            return glm::clamp(res, 0.0, 1.0);
        };
    _followRotationInterpolator.setTransferFunction(smoothStep);

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
    _rotateToFocusNodeInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);

    // Define callback functions for changed properties
    _rollFriction.onChange([&]() {
        _mouseStates.setRotationalFriction(_rollFriction);
    });
    _rotationalFriction.onChange([&]() {
        _mouseStates.setHorizontalFriction(_rotationalFriction);
    });
    _zoomFriction.onChange([&]() {
        _mouseStates.setVerticalFriction(_zoomFriction);
    });
    _sensitivity.onChange([&]() {
        _mouseStates.setSensitivity(_sensitivity * pow(10.0,-4));
    });
    _motionLag.onChange([&]() {
        _mouseStates.setVelocityScaleFactor(1 / (_motionLag + 0.0000001));
    });

    // Add the properties
    addProperty(_rollFriction);
    addProperty(_rotationalFriction);
    addProperty(_zoomFriction);
    addProperty(_followFocusNodeRotationDistance);
    addProperty(_minimumAllowedDistance);
    addProperty(_sensitivity);
    addProperty(_motionLag);
}

OrbitalNavigator::~OrbitalNavigator()
{ }

void OrbitalNavigator::updateMouseStatesFromInput(const InputState& inputState,
                                                  double deltaTime)
{
    _mouseStates.updateMouseStatesFromInput(inputState, deltaTime);
}

void OrbitalNavigator::updateCameraStateFromMouseStates(Camera& camera,
                                                        double deltaTime)
{
    if (_focusNode) {
        // Read the current state of the camera
        glm::dvec3 camPos = camera.positionVec3();
        glm::dvec3 centerPos = _focusNode->worldPosition();

        // Follow focus nodes movement
        glm::dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;

        // Calculate a position handle based on the camera position in world space
        SurfacePositionHandle posHandle = calculateSurfacePositionHandle(camPos);

        // Decompose camera rotation so that we can handle global and local rotation
        // individually. Then we combine them again when finished.
        CameraRotationDecomposition camRot = decomposeCameraRotation(
            camPos,
            camera.rotationQuaternion(),
            camera.lookUpVectorWorldSpace(),
            camera.viewDirectionWorldSpace()
        );

        // Rotate with the object by finding a differential rotation from the previous
        // to the current rotation
        glm::dmat3 objectStateMatrix = _focusNode->worldRotationMatrix();
        glm::dquat objectRotation = glm::quat_cast(objectStateMatrix);
        glm::dquat focusNodeRotationDiff =
            _previousFocusNodeRotation * glm::inverse(objectRotation);
        _previousFocusNodeRotation = objectRotation;

        // Interpolate rotation differential so that the camera rotates with the object
        // only if close enough
        focusNodeRotationDiff = interpolateRotationDifferential(
            deltaTime,
            1.0,
            focusNodeRotationDiff,
            centerPos,
            camPos,
            posHandle
        );

        // Update local rotation
        camRot.localRotation = roll(deltaTime, camRot.localRotation);
        camRot.localRotation = interpolateLocalRotation(deltaTime, camRot.localRotation);
        camRot.localRotation = rotateLocally(deltaTime, camRot.localRotation);

        // Horizontal translation
        camPos = translateHorizontally(
            deltaTime,
            camPos,
            centerPos,
            focusNodeRotationDiff,
            camRot.globalRotation,
            posHandle
        );

        // Horizontal translation by focus node rotation
        camPos = followFocusNodeRotation(
            camPos,
            centerPos,
            focusNodeRotationDiff
        );

        // Recalculate posHandle since horizontal position changed
        posHandle = calculateSurfacePositionHandle(camPos);

        camRot.globalRotation = rotateGlobally(
            camRot.globalRotation,
            centerPos,
            focusNodeRotationDiff,
            camPos,
            posHandle
        );

        // Rotate around the surface out direction
        camRot.globalRotation = rotateHorizontally(
            deltaTime,
            camRot.globalRotation,
            camPos,
            posHandle
        );

        // Perform the vertical movements
        camPos = translateVertically(deltaTime, camPos, centerPos, posHandle);
        camPos = pushToSurface(
            _minimumAllowedDistance,
            camPos,
            centerPos,
            posHandle
        );

        // Update the camera state
        camera.setPositionVec3(camPos); 
        camera.setRotation(camRot.globalRotation * camRot.localRotation);
    }
}

void OrbitalNavigator::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;

    if (_focusNode != nullptr) {
        _previousFocusNodePosition = _focusNode->worldPosition();
        _previousFocusNodeRotation = glm::quat_cast(_focusNode->worldRotationMatrix());
    }
}

void OrbitalNavigator::startInterpolateCameraDirection(const Camera& camera) {
    glm::dvec3 camPos = camera.positionVec3();
    glm::dvec3 camDir = glm::normalize(
        camera.rotationQuaternion() * glm::dvec3(0.0, 0.0, -1.0)
    );
    glm::dvec3 centerPos = _focusNode->worldPosition();
    glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    double angle = glm::angle(camDir, directionToCenter);

    // Minimum is two second. Otherwise proportional to angle
    _rotateToFocusNodeInterpolator.setInterpolationTime(static_cast<float>(
        glm::max(angle * 2.0, 2.0)
    ));
    _rotateToFocusNodeInterpolator.start();
}

bool OrbitalNavigator::followingNodeRotation() const {
    return _followRotationInterpolator.value() >= 1.0;
}

SceneGraphNode* OrbitalNavigator::focusNode() const {
    return _focusNode;
}

OrbitalNavigator::CameraRotationDecomposition
    OrbitalNavigator::decomposeCameraRotation(
        const glm::dvec3& cameraPosition,
        const glm::dquat& cameraRotation,
        const glm::dvec3& cameraLookUp,
        const glm::dvec3& cameraViewDirection)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();
    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));

    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    glm::dvec3 directionFromSurfaceToCameraModelSpace =
        posHandle.referenceSurfaceOutDirection;
    glm::dvec3 directionFromSurfaceToCamera =
        glm::normalize(glm::dmat3(modelTransform) *
        directionFromSurfaceToCameraModelSpace);

    // To avoid problem with lookup in up direction we adjust is with the view direction
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0.0, 0.0, 0.0),
        -directionFromSurfaceToCamera,
        normalize(cameraViewDirection + cameraLookUp));
    glm::dquat globalCameraRotation = glm::normalize(glm::quat_cast(inverse(lookAtMat)));
    glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) * cameraRotation;

    return { localCameraRotation, globalCameraRotation };
}

glm::dquat OrbitalNavigator::roll(double deltaTime,
                            const glm::dquat& localCameraRotation) const
{
    glm::dquat rollQuat = glm::angleAxis(
        _mouseStates.localRollMouseVelocity().x * deltaTime,
        glm::dvec3(0.0, 0.0, 1.0)
    );
    return localCameraRotation * rollQuat;
}

glm::dquat OrbitalNavigator::rotateLocally(double deltaTime,
                                           const glm::dquat& localCameraRotation) const
{
    glm::dvec3 eulerAngles(
        _mouseStates.localRotationMouseVelocity().y,
        _mouseStates.localRotationMouseVelocity().x,
        0.0
    );
    glm::dquat rotationDiff = glm::dquat(eulerAngles * deltaTime);
    return localCameraRotation * rotationDiff;
}

glm::dquat OrbitalNavigator::interpolateLocalRotation(
    double deltaTime,
    const glm::dquat& localCameraRotation)
{
    if (_rotateToFocusNodeInterpolator.isInterpolating()) {
        double t = _rotateToFocusNodeInterpolator.value();
        _rotateToFocusNodeInterpolator.setDeltaTime(static_cast<float>(deltaTime));
        _rotateToFocusNodeInterpolator.step();
        glm::dquat result = glm::slerp(
            localCameraRotation,
            glm::dquat(glm::dvec3(0.0)),
            glm::min(t * _rotateToFocusNodeInterpolator.deltaTimeScaled(), 1.0));
        if (angle(result) < 0.01) {
            _rotateToFocusNodeInterpolator.end();
        }
        return result;
    }
    else {
        return localCameraRotation;
    }
}

glm::dvec3 OrbitalNavigator::translateHorizontally(
    double deltaTime,
    const glm::dvec3& cameraPosition,
    const glm::dvec3& objectPosition,
    const glm::dquat& /*focusNodeRotationDiff*/,
    const glm::dquat& globalCameraRotation,
    const SurfacePositionHandle& positionHandle) const
{
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 outDirection =
        glm::normalize(glm::dmat3(modelTransform) *
        positionHandle.referenceSurfaceOutDirection);

    // Vector logic
    glm::dvec3 posDiff = cameraPosition - objectPosition;
    glm::dvec3 centerToReferenceSurface = glm::dmat3(modelTransform) *
        positionHandle.centerToReferenceSurface;
    glm::dvec3 centerToActualSurfaceModelSpace = positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;
    glm::dvec3 centerToActualSurface = glm::dmat3(modelTransform) *
        centerToActualSurfaceModelSpace;
    glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    double distFromSurfaceToCamera = glm::length(actualSurfaceToCamera);

    // Final values to be used
    double distFromCenterToSurface = length(centerToActualSurface);
    double distFromCenterToCamera = length(posDiff);

    double speedScale =
        distFromCenterToSurface > 0.0 ?
        glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0) :
        1.0;

    // Get rotation in camera space
    glm::dvec3 eulerAngles = glm::dvec3(
        -_mouseStates.globalRotationMouseVelocity().y * deltaTime,
        -_mouseStates.globalRotationMouseVelocity().x * deltaTime,
        0) * speedScale;
    glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

    // Transform to world space
    glm::dquat rotationDiffWorldSpace =
        globalCameraRotation *
        rotationDiffCamSpace *
        glm::inverse(globalCameraRotation);

    // Rotate and find the difference vector
    glm::dvec3 rotationDiffVec3 =
        (distFromCenterToCamera * outDirection)
         * rotationDiffWorldSpace
        - (distFromCenterToCamera * outDirection);

    // Add difference to position
    return cameraPosition + rotationDiffVec3;
}

glm::dvec3 OrbitalNavigator::followFocusNodeRotation(
    const glm::dvec3& cameraPosition,
    const glm::dvec3& objectPosition,
    const glm::dquat& focusNodeRotationDiff) const
{
    glm::dvec3 posDiff = cameraPosition - objectPosition;
    glm::dvec3 rotationDiffVec3AroundCenter =
        posDiff
        * focusNodeRotationDiff
        - (posDiff);
    return cameraPosition + rotationDiffVec3AroundCenter;
}

glm::dquat OrbitalNavigator::rotateGlobally(
    const glm::dquat& globalCameraRotation,
    const glm::dvec3& /*objectPosition*/,
    const glm::dquat& focusNodeRotationDiff,
    const glm::dvec3& /*cameraPosition*/,
    const SurfacePositionHandle& positionHandle) const
{
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 directionFromSurfaceToCamera =
        glm::dmat3(modelTransform) * positionHandle.referenceSurfaceOutDirection;

    glm::dvec3 lookUpWhenFacingSurface =
        glm::inverse(focusNodeRotationDiff) *
        globalCameraRotation * glm::dvec3(0.0, 1.0, 0.0);
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0.0, 0.0, 0.0),
        -directionFromSurfaceToCamera,
        lookUpWhenFacingSurface);
    return glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
}

glm::dvec3 OrbitalNavigator::translateVertically(
    double deltaTime,
    const glm::dvec3& cameraPosition,
    const glm::dvec3& objectPosition,
    const SurfacePositionHandle& positionHandle) const
{
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 posDiff = cameraPosition - objectPosition;

    glm::dvec3 centerToReferenceSurface =
        glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface;
    glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;
    glm::dvec3 centerToActualSurface =
        glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;

    return cameraPosition -
        actualSurfaceToCamera * _mouseStates.truckMovementMouseVelocity().y * deltaTime;
}

glm::dquat OrbitalNavigator::rotateHorizontally(
    double deltaTime,
    const glm::dquat& globalCameraRotation,
    const glm::dvec3& /*cameraPosition*/, 
    const SurfacePositionHandle& positionHandle) const
{
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 directionFromSurfaceToCameraModelSpace =
        positionHandle.referenceSurfaceOutDirection;
    glm::dvec3 directionFromSurfaceToCamera =
      glm::normalize(glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);

    glm::dquat cameraRollRotation =
        glm::angleAxis(
            _mouseStates.globalRollMouseVelocity().x *
            deltaTime, directionFromSurfaceToCamera
        );
    return cameraRollRotation * globalCameraRotation;
}

glm::dvec3 OrbitalNavigator::pushToSurface(
    double minHeightAboveGround,
    const glm::dvec3& cameraPosition,
    const glm::dvec3& objectPosition,
    const SurfacePositionHandle& positionHandle) const
{
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 posDiff = cameraPosition - objectPosition;
    glm::dvec3 referenceSurfaceOutDirection =
        glm::dmat3(modelTransform) * positionHandle.referenceSurfaceOutDirection;
    glm::dvec3 centerToReferenceSurface =
        glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface;
    glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;
    glm::dvec3 centerToActualSurface =
        glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    double surfaceToCameraSigned =
        glm::length(actualSurfaceToCamera) *
        glm::sign(dot(actualSurfaceToCamera, referenceSurfaceOutDirection));

    return cameraPosition + referenceSurfaceOutDirection *
        glm::max(minHeightAboveGround - surfaceToCameraSigned, 0.0);
}

glm::dquat OrbitalNavigator::interpolateRotationDifferential(
    double deltaTime,
    double interpolationTime,
    const glm::dquat& rotationDiff,
    const glm::dvec3& objectPosition,
    const glm::dvec3& cameraPosition,
    const SurfacePositionHandle& positionHandle)
{
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    double maximumDistanceForRotation = glm::length(
            glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface
        ) * _followFocusNodeRotationDistance;
    double distanceToCamera = glm::length(cameraPosition - objectPosition);

    // Interpolate with a negative delta time if distance is too large to follow
    double interpolationSign = glm::sign(maximumDistanceForRotation - distanceToCamera);

    _followRotationInterpolator.setInterpolationTime(static_cast<float>(
        interpolationTime
    ));
    _followRotationInterpolator.setDeltaTime(static_cast<float>(
        interpolationSign * deltaTime
    ));
    _followRotationInterpolator.step();

    return glm::slerp(
        glm::dquat(glm::dvec3(0.0)),
        rotationDiff,
        _followRotationInterpolator.value()
    );
}

SurfacePositionHandle OrbitalNavigator::calculateSurfacePositionHandle(
    const glm::dvec3 cameraPositionWorldSpace)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPositionWorldSpace, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);
    return posHandle;
}

} // namespace openspace::interaction
