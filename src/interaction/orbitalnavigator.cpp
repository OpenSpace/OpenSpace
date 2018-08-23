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

#include <openspace/interaction/orbitalnavigator.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <glm/gtx/vector_angle.hpp>

namespace {
    constexpr openspace::properties::Property::PropertyInfo RollFrictionInfo = {
        "RollFriction",
        "Roll Friction",
        "If this is enabled, a small friction is applied to the rolling part of the "
        "camera motion, thus slowing it down within a small time period. If this value "
        "is disabled, the camera will roll forever."
    };

    constexpr openspace::properties::Property::PropertyInfo RotationalFrictionInfo =
    {
        "RotationalFriction",
        "Rotational Friction",
        "If this is enabled, a small friction is applied to the rotational part of the "
        "camera motion, thus slowing it down within a small time period. If this value "
        "is disabled, the camera will rotate forever."
    };

    constexpr openspace::properties::Property::PropertyInfo ZoomFrictionInfo = {
        "ZoomFriction",
        "Zoom Friction",
        "If this is enabled, a small friction is applied to the zoom part of the camera "
        "motion, thus slowing it down within a small time period. If this value is "
        "disabled, the camera will zoom in or out forever."
    };

    constexpr openspace::properties::Property::PropertyInfo MouseSensitivityInfo = {
        "MouseSensitivity",
        "Mouse Sensitivity",
        "Determines the sensitivity of the camera motion thorugh the mouse. The lower "
        "the sensitivity is the less impact a mouse motion will have."
    };

    constexpr openspace::properties::Property::PropertyInfo JoystickSensitivityInfo = {
        "JoystickSensitivity",
        "Joystick Sensitivity",
        "Determines the sensitivity of the camera motion thorugh a joystick. The lower "
        "the sensitivity is the less impact a joystick motion will have."
    };

    constexpr openspace::properties::Property::PropertyInfo FrictionInfo = {
        "Friction",
        "Friction Factor",
        "Determines the factor that is applied if the 'Roll Friction', 'Rotational "
        "Friction', and 'Zoom Friction' values are enabled. The lower this value is, the "
        "faster the camera movements will stop."
    };

    constexpr openspace::properties::Property::PropertyInfo FollowFocusNodeInfo = {
        "FollowFocusNodeRotationDistance",
        "Follow focus node rotation distance",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo MinimumDistanceInfo = {
        "MinimumAllowedDistance",
        "Minimum allowed distance",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo StereoInterpolationTimeInfo =
    {
        "StereoInterpolationTime",
        "Stereo interpolation time",
        "The time to interpolate to a new stereoscopic depth "
        "when the focus node is changed"
    };

    constexpr openspace::properties::Property::PropertyInfo
        RotateToFocusInterpolationTimeInfo = {
            "RotateToFocusInterpolationTime",
            "Rotate to focus interpolation time",
            "The time to interpolate the camera rotation "
            "when the focus node is changed"
        };

    constexpr openspace::properties::Property::PropertyInfo
        UseAdaptiveStereoscopicDepthInfo = {
            "UseAdaptiveStereoscopicDepth",
            "Adaptive Steroscopic Depth",
            "Dynamically adjust the view scaling based on the distance to the surface of "
            "the focus node. If enabled, view scale will be set to "
            "StereoscopicDepthOfFocusSurface / distance. "
            "If disabled, view scale will be set to 10^StaticViewScaleExponent."
        };

    constexpr openspace::properties::Property::PropertyInfo
        StaticViewScaleExponentInfo = {
            "StaticViewScaleExponent",
            "Static View Scale Exponent",
            "Statically scale the world by 10^StaticViewScaleExponent. "
            "Only used if UseAdaptiveStereoscopicDepthInfo is set to false."
        };

    constexpr openspace::properties::Property::PropertyInfo
        StereoscopicDepthOfFocusSurfaceInfo = {
            "StereoscopicDepthOfFocusSurface",
            "Stereoscopic depth of the surface in focus",
            "Set the stereoscopically perceived distance (in meters) to the surface of "
            "the focus node. "
            "Only used if UseAdaptiveStereoscopicDepthInfo is set to true."
        };
} // namespace

namespace openspace::interaction {

OrbitalNavigator::Friction::Friction()
    : properties::PropertyOwner({ "Friction" })
    , roll(RollFrictionInfo, true)
    , rotational(RotationalFrictionInfo, true)
    , zoom(ZoomFrictionInfo, true)
    , friction(FrictionInfo, 0.5f, 0.f, 1.f)
{
    addProperty(roll);
    addProperty(rotational);
    addProperty(zoom);
    addProperty(friction);
}

OrbitalNavigator::OrbitalNavigator()
    : properties::PropertyOwner({ "OrbitalNavigator" })
    , _followFocusNodeRotationDistance(FollowFocusNodeInfo, 5.0f, 0.0f, 20.f)
    , _minimumAllowedDistance(MinimumDistanceInfo, 10.0f, 0.0f, 10000.f)
    , _mouseSensitivity(MouseSensitivityInfo, 15.0f, 1.0f, 50.f)
    , _joystickSensitivity(JoystickSensitivityInfo, 10.0f, 1.0f, 50.f)
    , _mouseStates(_mouseSensitivity * 0.0001, 1 / (_friction.friction + 0.0000001))
    , _joystickStates(_joystickSensitivity * 0.1, 1 / (_friction.friction + 0.0000001))
    , _useAdaptiveStereoscopicDepth(UseAdaptiveStereoscopicDepthInfo, true)
    , _stereoscopicDepthOfFocusSurface(StereoscopicDepthOfFocusSurfaceInfo, 8, 0.25, 100)
    , _staticViewScaleExponent(StaticViewScaleExponentInfo, 0.f, -30, 10)
    , _rotateToFocusInterpolationTime(RotateToFocusInterpolationTimeInfo, 2.0, 0.0, 10.0)
    , _stereoInterpolationTime(StereoInterpolationTimeInfo, 8.0, 0.0, 10.0)
{
    _followRotationInterpolator.setTransferFunction([](double t) {
        const double res = 3.0 * t*t - 2.0 * t*t*t;
        return glm::clamp(res, 0.0, 1.0);
    });

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
    auto smoothStepDerivedTranferFunction = [](double t) {
        return (6 * (t + t*t) / (1 - 3 * t*t + 2 * t*t*t));
    };
    _rotateToFocusNodeInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);
    _cameraToSurfaceDistanceInterpolator.setTransferFunction(
        smoothStepDerivedTranferFunction
    );

    // Define callback functions for changed properties
    _friction.roll.onChange([&]() {
        _mouseStates.setRotationalFriction(_friction.roll);
        _joystickStates.setRotationalFriction(_friction.roll);
    });
    _friction.rotational.onChange([&]() {
        _mouseStates.setHorizontalFriction(_friction.rotational);
        _joystickStates.setHorizontalFriction(_friction.rotational);
    });
    _friction.zoom.onChange([&]() {
        _mouseStates.setVerticalFriction(_friction.zoom);
        _joystickStates.setVerticalFriction(_friction.zoom);
    });
    _friction.friction.onChange([&]() {
        _mouseStates.setVelocityScaleFactor(1 / (_friction.friction + 0.0000001));
        _joystickStates.setVelocityScaleFactor(1 / (_friction.friction + 0.0000001));
    });

    _mouseSensitivity.onChange([&]() {
        _mouseStates.setSensitivity(_mouseSensitivity * pow(10.0, -4));
    });
    _joystickSensitivity.onChange([&]() {
        _joystickStates.setSensitivity(_joystickSensitivity * pow(10.0, -4));
    });

    addPropertySubOwner(_friction);

    addProperty(_followFocusNodeRotationDistance);
    addProperty(_minimumAllowedDistance);

    addProperty(_useAdaptiveStereoscopicDepth);
    addProperty(_staticViewScaleExponent);
    addProperty(_stereoscopicDepthOfFocusSurface);

    addProperty(_rotateToFocusInterpolationTime);
    addProperty(_stereoInterpolationTime);
    addProperty(_mouseSensitivity);
    addProperty(_joystickSensitivity);
}

void OrbitalNavigator::updateStatesFromInput(const InputState& inputState,
                                             double deltaTime)
{
    _mouseStates.updateStateFromInput(inputState, deltaTime);
    _joystickStates.updateStateFromInput(inputState, deltaTime);
}

void OrbitalNavigator::updateCameraStateFromStates(Camera& camera, double deltaTime) {
    if (_focusNode) {
        // Read the current state of the camera
        glm::dvec3 camPos = camera.positionVec3();
        const glm::dvec3 centerPos = _focusNode->worldPosition();

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
        const glm::dmat3 objectStateMatrix = _focusNode->worldRotationMatrix();
        const glm::dquat objectRotation = glm::quat_cast(objectStateMatrix);
        glm::dquat focusNodeRotationDiff = _previousFocusNodeRotation *
                                           glm::inverse(objectRotation);

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

        if (_useAdaptiveStereoscopicDepth) {
            double targetCameraToSurfaceDistance = glm::length(
                cameraToSurfaceVector(camPos, centerPos, posHandle)
            );
            if (_directlySetStereoDistance) {
                _currentCameraToSurfaceDistance = targetCameraToSurfaceDistance;
                _directlySetStereoDistance = false;
            } else {
                _currentCameraToSurfaceDistance = interpolateCameraToSurfaceDistance(
                    deltaTime,
                    _currentCameraToSurfaceDistance,
                    targetCameraToSurfaceDistance);
            }

            camera.setScaling(
                _stereoscopicDepthOfFocusSurface /
                static_cast<float>(_currentCameraToSurfaceDistance)
            );
        } else {
            camera.setScaling(glm::pow(10.f, _staticViewScaleExponent));
        }
    }
}

glm::dvec3 OrbitalNavigator::cameraToSurfaceVector(const glm::dvec3& camPos,
    const glm::dvec3& centerPos, const SurfacePositionHandle& posHandle)
{
    glm::dmat4 modelTransform = _focusNode->modelTransform();
    glm::dvec3 posDiff = camPos - centerPos;
    glm::dvec3 centerToActualSurfaceModelSpace =
        posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;

    glm::dvec3 centerToActualSurface =
        glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;

    return centerToActualSurface - posDiff;
}

void OrbitalNavigator::setFocusNode(SceneGraphNode* focusNode) {
    if (!_focusNode) {
        _directlySetStereoDistance = true;
    }

    _focusNode = focusNode;

    if (_focusNode) {
        _previousFocusNodePosition = _focusNode->worldPosition();
        _previousFocusNodeRotation = glm::quat_cast(_focusNode->worldRotationMatrix());
    }
}

void OrbitalNavigator::startInterpolateCameraDirection(const Camera& camera) {
    const glm::dvec3 camPos = camera.positionVec3();
    const glm::dvec3 camDir = glm::normalize(
        camera.rotationQuaternion() * glm::dvec3(0.0, 0.0, -1.0)
    );
    const glm::dvec3 centerPos = _focusNode->worldPosition();
    const glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    const double angle = glm::angle(camDir, directionToCenter);

    // Minimum is two second. Otherwise proportional to angle
    _rotateToFocusNodeInterpolator.setInterpolationTime(static_cast<float>(
        glm::max(angle, 1.0) * _rotateToFocusInterpolationTime
    ));
    _rotateToFocusNodeInterpolator.start();

    _cameraToSurfaceDistanceInterpolator.setInterpolationTime(_stereoInterpolationTime);
    _cameraToSurfaceDistanceInterpolator.start();
}

bool OrbitalNavigator::followingNodeRotation() const {
    return _followRotationInterpolator.value() >= 1.0;
}

SceneGraphNode* OrbitalNavigator::focusNode() const {
    return _focusNode;
}

bool OrbitalNavigator::hasRotationalFriction() const {
    return _friction.rotational;
}

bool OrbitalNavigator::hasZoomFriction() const {
    return _friction.zoom;
}

bool OrbitalNavigator::hasRollFriction() const {
    return _friction.roll;
}

OrbitalNavigator::CameraRotationDecomposition OrbitalNavigator::decomposeCameraRotation(
                                                         const glm::dvec3& cameraPosition,
                                                         const glm::dquat& cameraRotation,
                                                           const glm::dvec3& cameraLookUp,
                                                    const glm::dvec3& cameraViewDirection)
{
    const glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    const glm::dmat4 modelTransform = _focusNode->modelTransform();
    const glm::dvec3 cameraPositionModelSpace = glm::dvec3(inverseModelTransform *
                                                glm::dvec4(cameraPosition, 1));

    const SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    const glm::dvec3 directionFromSurfaceToCameraModelSpace =
        posHandle.referenceSurfaceOutDirection;
    const glm::dvec3 directionFromSurfaceToCamera = glm::normalize(
        glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace
    );

    // To avoid problem with lookup in up direction we adjust is with the view direction
    const glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0.0, 0.0, 0.0),
        -directionFromSurfaceToCamera,
        normalize(cameraViewDirection + cameraLookUp)
    );
    const glm::dquat globalCameraRotation = glm::normalize(
        glm::quat_cast(inverse(lookAtMat))
    );
    const glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) *
                                           cameraRotation;

    return { localCameraRotation, globalCameraRotation };
}

glm::dquat OrbitalNavigator::roll(double deltaTime,
                                  const glm::dquat& localCameraRotation) const
{
    const glm::dquat mouseRollQuat = glm::angleAxis(
        _mouseStates.localRollVelocity().x * deltaTime +
        _joystickStates.localRollVelocity().x * deltaTime,
        glm::dvec3(0.0, 0.0, 1.0)
    );
    return localCameraRotation * mouseRollQuat;
}

glm::dquat OrbitalNavigator::rotateLocally(double deltaTime,
                                           const glm::dquat& localCameraRotation) const
{
    const glm::dquat mouseRotationDiff = glm::dquat(glm::dvec3(
        _mouseStates.localRotationVelocity().y,
        _mouseStates.localRotationVelocity().x,
        0.0
    ) * deltaTime);

    const glm::dquat joystickRotationDiff = glm::dquat(glm::dvec3(
        _joystickStates.localRotationVelocity().y,
        _joystickStates.localRotationVelocity().x,
        0.0
    ) * deltaTime);


    return localCameraRotation * joystickRotationDiff * mouseRotationDiff;
}

glm::dquat OrbitalNavigator::interpolateLocalRotation(double deltaTime,
                                                    const glm::dquat& localCameraRotation)
{
    if (_rotateToFocusNodeInterpolator.isInterpolating()) {
        const double t = _rotateToFocusNodeInterpolator.value();
        _rotateToFocusNodeInterpolator.setDeltaTime(static_cast<float>(deltaTime));
        _rotateToFocusNodeInterpolator.step();
        const glm::dquat result = glm::slerp(
            localCameraRotation,
            glm::dquat(glm::dvec3(0.0)),
            glm::min(t * _rotateToFocusNodeInterpolator.deltaTimeScaled(), 1.0));

        // Retrieving the angle of a quaternion uses acos on the w component, which can
        // have numerical instability for values close to 1.0
        constexpr double Epsilon = 1.0e-13;
        if (abs((abs(result.w) - 1.0)) < Epsilon || angle(result) < 0.01) {
            _rotateToFocusNodeInterpolator.end();
        }
        return result;
    }
    else {
        return localCameraRotation;
    }
}

double OrbitalNavigator::interpolateCameraToSurfaceDistance(double deltaTime,
                                                            double currentDistance,
                                                            double targetDistance
) {
    if (!_cameraToSurfaceDistanceInterpolator.isInterpolating()) {
        return targetDistance;
    }

    double t = _cameraToSurfaceDistanceInterpolator.value();
    _cameraToSurfaceDistanceInterpolator.setDeltaTime(static_cast<float>(deltaTime));
    _cameraToSurfaceDistanceInterpolator.step();

    // Interpolate distance logarithmically.
    double result = glm::exp(glm::mix(
        glm::log(currentDistance),
        glm::log(targetDistance),
        glm::min(t * _cameraToSurfaceDistanceInterpolator.deltaTimeScaled(), 1.0))
    );

    double ratio = currentDistance / targetDistance;
    if (glm::abs(ratio - 1.0) < 0.000001) {
        _cameraToSurfaceDistanceInterpolator.end();
    }

    return result;
}

glm::dvec3 OrbitalNavigator::translateHorizontally(double deltaTime,
                                                   const glm::dvec3& cameraPosition,
                                                   const glm::dvec3& objectPosition,
                                              const glm::dquat& /*focusNodeRotationDiff*/,
                                                   const glm::dquat& globalCameraRotation,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _focusNode->modelTransform();

    const glm::dvec3 outDirection = glm::normalize(glm::dmat3(modelTransform) *
                                    positionHandle.referenceSurfaceOutDirection);

    // Vector logic
    const glm::dvec3 posDiff = cameraPosition - objectPosition;
    // glm::dvec3 centerToReferenceSurface = glm::dmat3(modelTransform) *
        // positionHandle.centerToReferenceSurface;
    const glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;

    const glm::dvec3 centerToActualSurface = glm::dmat3(modelTransform) *
                                             centerToActualSurfaceModelSpace;
    const glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    const double distFromSurfaceToCamera = glm::length(actualSurfaceToCamera);

    // Final values to be used
    const double distFromCenterToSurface = length(centerToActualSurface);
    const double distFromCenterToCamera = length(posDiff);

    const double speedScale =
        distFromCenterToSurface > 0.0 ?
        glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0) :
        1.0;

    // Get rotation in camera space
    const glm::dquat mouseRotationDiffCamSpace = glm::dquat(glm::dvec3(
        -_mouseStates.globalRotationVelocity().y * deltaTime,
        -_mouseStates.globalRotationVelocity().x * deltaTime,
        0) * speedScale);

    const glm::dquat joystickRotationDiffCamSpace = glm::dquat(glm::dvec3(
        -_joystickStates.globalRotationVelocity().y * deltaTime,
        -_joystickStates.globalRotationVelocity().x * deltaTime,
        0) * speedScale
    );

    // Transform to world space
    const glm::dquat rotationDiffWorldSpace = globalCameraRotation *
                                        joystickRotationDiffCamSpace *
                                        mouseRotationDiffCamSpace *
                                        glm::inverse(globalCameraRotation);

    // Rotate and find the difference vector
    const glm::dvec3 rotationDiffVec3 = (distFromCenterToCamera * outDirection) *
                                  rotationDiffWorldSpace -
                                  (distFromCenterToCamera * outDirection);

    // Add difference to position
    return cameraPosition + rotationDiffVec3;
}

glm::dvec3 OrbitalNavigator::followFocusNodeRotation(const glm::dvec3& cameraPosition,
                                                     const glm::dvec3& objectPosition,
                                            const glm::dquat& focusNodeRotationDiff) const
{
    const glm::dvec3 posDiff = cameraPosition - objectPosition;
    const glm::dvec3 rotationDiffVec3AroundCenter = posDiff * focusNodeRotationDiff -
                                                    posDiff;
    return cameraPosition + rotationDiffVec3AroundCenter;
}

glm::dquat OrbitalNavigator::rotateGlobally(const glm::dquat& globalCameraRotation,
                                            const glm::dvec3& /*objectPosition*/,
                                            const glm::dquat& focusNodeRotationDiff,
                                            const glm::dvec3& /*cameraPosition*/,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _focusNode->modelTransform();

    const glm::dvec3 directionFromSurfaceToCamera =
        glm::dmat3(modelTransform) * positionHandle.referenceSurfaceOutDirection;

    const glm::dvec3 lookUpWhenFacingSurface = glm::inverse(focusNodeRotationDiff) *
                                         globalCameraRotation * glm::dvec3(0.0, 1.0, 0.0);
    const glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0.0, 0.0, 0.0),
        -directionFromSurfaceToCamera,
        lookUpWhenFacingSurface
    );
    return glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
}

glm::dvec3 OrbitalNavigator::translateVertically(double deltaTime,
                                                 const glm::dvec3& cameraPosition,
                                                 const glm::dvec3& objectPosition,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _focusNode->modelTransform();

    const glm::dvec3 posDiff = cameraPosition - objectPosition;

    // glm::dvec3 centerToReferenceSurface =
        // glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface;
    const glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;

    const glm::dvec3 centerToActualSurface = glm::dmat3(modelTransform) *
                                             centerToActualSurfaceModelSpace;
    const glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;

    const double totalVelocity = _joystickStates.truckMovementVelocity().y +
                                 _mouseStates.truckMovementVelocity().y;

    return cameraPosition - actualSurfaceToCamera * totalVelocity * deltaTime;
}

glm::dquat OrbitalNavigator::rotateHorizontally(double deltaTime,
                                                const glm::dquat& globalCameraRotation,
                                                const glm::dvec3& /*cameraPosition*/,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _focusNode->modelTransform();

    const glm::dvec3 directionFromSurfaceToCameraModelSpace =
        positionHandle.referenceSurfaceOutDirection;
    const glm::dvec3 directionFromSurfaceToCamera = glm::normalize(
        glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace
    );

    const glm::dquat mouseCameraRollRotation = glm::angleAxis(
        _mouseStates.globalRollVelocity().x * deltaTime +
        _joystickStates.globalRollVelocity().x * deltaTime,
        directionFromSurfaceToCamera
    );
    return mouseCameraRollRotation * globalCameraRotation;
}

glm::dvec3 OrbitalNavigator::pushToSurface(double minHeightAboveGround,
                                           const glm::dvec3& cameraPosition,
                                           const glm::dvec3& objectPosition,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _focusNode->modelTransform();

    const glm::dvec3 posDiff = cameraPosition - objectPosition;
    const glm::dvec3 referenceSurfaceOutDirection = glm::dmat3(modelTransform) *
                                              positionHandle.referenceSurfaceOutDirection;
    // glm::dvec3 centerToReferenceSurface =
        // glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface;
    const glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;

    const glm::dvec3 centerToActualSurface = glm::dmat3(modelTransform) *
                                             centerToActualSurfaceModelSpace;
    const glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    const double surfaceToCameraSigned = glm::length(actualSurfaceToCamera) *
        glm::sign(dot(actualSurfaceToCamera, referenceSurfaceOutDirection));

    return cameraPosition + referenceSurfaceOutDirection *
        glm::max(minHeightAboveGround - surfaceToCameraSigned, 0.0);
}

glm::dquat OrbitalNavigator::interpolateRotationDifferential(double deltaTime,
                                                                 double interpolationTime,
                                                           const glm::dquat& rotationDiff,
                                                         const glm::dvec3& objectPosition,
                                                         const glm::dvec3& cameraPosition,
                                              const SurfacePositionHandle& positionHandle)
{
    const glm::dmat4 modelTransform = _focusNode->modelTransform();

    const double maximumDistanceForRotation = glm::length(
        glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface
    ) * _followFocusNodeRotationDistance;
    const double distanceToCamera = glm::length(cameraPosition - objectPosition);

    // Interpolate with a negative delta time if distance is too large to follow
    const double interpolationSign = glm::sign(
        maximumDistanceForRotation - distanceToCamera
    );

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
    const glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPositionWorldSpace, 1));
    const SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    return posHandle;
}

JoystickCameraStates& OrbitalNavigator::joystickStates() {
    return _joystickStates;
}

} // namespace openspace::interaction
