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
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/vector_angle.hpp>

#pragma optimize( "", off )

namespace {
    constexpr const char* _loggerCat = "OrbitalNavigator";

    constexpr const double AngleEpsilon = 1E-7;
    constexpr const double DistanceEpsilon = 1E-7;

    constexpr const openspace::properties::Property::PropertyInfo AnchorInfo = {
        "Anchor",
        "Anchor",
        "The name of the scene graph node that is the origin of the camera interaction. "
        "The camera follows, orbits and dollies towards this node. "
        "Any scene graph node can be the anchor node."
    };

    constexpr const openspace::properties::Property::PropertyInfo AimInfo = {
        "Aim",
        "Aim",
        "The name of the scene graph node that is the aim of the camera. "
        "The camera direction is relative to the vector from the camera position "
        "to this node."
    };

    constexpr const openspace::properties::Property::PropertyInfo
        RetargetAnchorInfo =
    {
        "RetargetAnchor",
        "Retarget Anchor",
        "When triggered, this property starts an interpolation to reset the "
        "camera direction to the anchor node."
    };

    constexpr const openspace::properties::Property::PropertyInfo
        RetargetAimInfo =
    {
        "RetargetAim",
        "Retarget Aim",
        "When triggered, this property starts an interpolation to reset the "
        "camera direction to the aim node."
    };

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

    constexpr openspace::properties::Property::PropertyInfo FollowAnchorNodeInfo = {
        "FollowAnchorNodeRotationDistance",
        "Follow anchor node rotation distance",
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
        "when the anchor node is changed"
    };

    constexpr openspace::properties::Property::PropertyInfo
        RotateToAimInterpolationTimeInfo = {
            "RotateToAimInterpolationTime",
            "Rotate to aim interpolation time",
            "The time to interpolate the camera rotation "
            "when the aim node is changed"
        };

    constexpr openspace::properties::Property::PropertyInfo
        UseAdaptiveStereoscopicDepthInfo = {
            "UseAdaptiveStereoscopicDepth",
            "Adaptive Steroscopic Depth",
            "Dynamically adjust the view scaling based on the distance to the surface of "
            "the anhor and aim nodes. If enabled, view scale will be set to "
            "StereoscopicDepthOfFocusSurface / min(anchorDistance, aimDistance). "
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
            "the closest anchor or aim node. "
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
    , _anchor(AnchorInfo)
    , _aim(AimInfo)
    , _retargetAnchor(RetargetAnchorInfo)
    , _retargetAim(RetargetAimInfo)
    , _followAnchorNodeRotationDistance(FollowAnchorNodeInfo, 5.0f, 0.0f, 20.f)
    , _minimumAllowedDistance(MinimumDistanceInfo, 10.0f, 0.0f, 10000.f)
    , _mouseSensitivity(MouseSensitivityInfo, 15.0f, 1.0f, 50.f)
    , _joystickSensitivity(JoystickSensitivityInfo, 10.0f, 1.0f, 50.f)
    , _useAdaptiveStereoscopicDepth(UseAdaptiveStereoscopicDepthInfo, true)
    , _stereoscopicDepthOfFocusSurface(StereoscopicDepthOfFocusSurfaceInfo, 8, 0.25, 100)
    , _staticViewScaleExponent(StaticViewScaleExponentInfo, 0.f, -30, 10)
    , _rotateInterpolationTime(RotateToAimInterpolationTimeInfo, 2.0, 0.0, 10.0)
    , _stereoInterpolationTime(StereoInterpolationTimeInfo, 8.0, 0.0, 10.0)
    , _mouseStates(_mouseSensitivity * 0.0001, 1 / (_friction.friction + 0.0000001))
    , _joystickStates(_joystickSensitivity * 0.1, 1 / (_friction.friction + 0.0000001))
{

    _anchor.onChange([this]() {
        if (_anchor.value().empty()) {
            return;
        }
        SceneGraphNode* node = sceneGraphNode(_anchor.value());
        if (node) {
            setAnchorNode(node);
        }
        else {
            LERROR(fmt::format(
                "No scenegraph node with identifier {} exists.", _anchor.value()
            ));
        }
    });

    _aim.onChange([this]() {
        if (_aim.value().empty()) {
            return;
        }
        SceneGraphNode* node = sceneGraphNode(_aim.value());
        if (node) {
            setAimNode(node);
        }
        else {
            LERROR(fmt::format(
                "No scenegraph node with identifier {} exists.", _aim.value()
            ));
        }
    });

    _retargetAnchor.onChange([this]() {
        startRetargetAnchor();
    });

    _retargetAim.onChange([this]() {
        startRetargetAim();
    });

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
    _rotateToAnchorInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);
    _rotateToAimInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);
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


    addProperty(_anchor);
    addProperty(_aim);
    addProperty(_retargetAnchor);
    addProperty(_retargetAim);
    addProperty(_followAnchorNodeRotationDistance);
    addProperty(_minimumAllowedDistance);

    addProperty(_useAdaptiveStereoscopicDepth);
    addProperty(_staticViewScaleExponent);
    addProperty(_stereoscopicDepthOfFocusSurface);

    addProperty(_rotateInterpolationTime);
    addProperty(_stereoInterpolationTime);
    addProperty(_mouseSensitivity);
    addProperty(_joystickSensitivity);
}

glm::dvec3 OrbitalNavigator::anchorNodeToCameraVector() const {
    return _camera->positionVec3() - anchorNode()->worldPosition();
}

glm::quat OrbitalNavigator::anchorNodeToCameraRotation() const {
    glm::dmat4 invWorldRotation = glm::dmat4(
        glm::inverse(anchorNode()->worldRotationMatrix())
    );
    return glm::quat(invWorldRotation) * glm::quat(_camera->rotationQuaternion());
}


void OrbitalNavigator::updateStatesFromInput(const InputState& inputState,
                                             double deltaTime)
{
    _mouseStates.updateStateFromInput(inputState, deltaTime);
    _joystickStates.updateStateFromInput(inputState, deltaTime);
}

void OrbitalNavigator::updateCameraStateFromStates(double deltaTime) {
    if (_anchorNode && _aimNode) {
        if (_anchorNode != _aimNode) {

            const glm::dvec3 anchorDisplacement = _anchorNode->worldPosition() - _previousAnchorNodePosition;
            Camera offsetCamera = *_camera;
            offsetCamera.setPositionVec3(_camera->positionVec3() + anchorDisplacement);
            CameraRotationDecomposition decomp = decomposeCameraRotationOrigin(offsetCamera, *_anchorNode);

            const glm::dvec3 prevCameraPosition = _camera->positionVec3();
            const glm::dvec3 prevCameraToAnchor = _previousAnchorNodePosition - prevCameraPosition;
            const glm::dvec3 prevCameraToAim = _previousAimNodePosition - prevCameraPosition;
            const glm::dvec3 prevAnchorToAim = _previousAimNodePosition - _previousAnchorNodePosition;
            const glm::dvec3 newAnchorToAim = _aimNode->worldPosition() - _anchorNode->worldPosition();

            if (glm::length(newAnchorToAim) > DistanceEpsilon) {
                glm::dvec3 newAnchorToProjectedAim = glm::length(prevAnchorToAim) * glm::normalize(newAnchorToAim);

                // Rotation based on projected aim spin around anchor (aim is projected on a sphere around the anchor)
                const double spinRotationAngle = glm::angle(glm::normalize(prevAnchorToAim), glm::normalize(newAnchorToProjectedAim));

                // By default, let the camera follow the anchor
                _camera->setPositionVec3(_anchorNode->worldPosition() - prevCameraToAnchor);

                if (spinRotationAngle > AngleEpsilon) {
                    const glm::dvec3 spinRotationAxis = glm::cross(prevAnchorToAim, newAnchorToProjectedAim);
                    const glm::dquat spinRotation = glm::angleAxis(spinRotationAngle, glm::normalize(spinRotationAxis));

                    const glm::dvec3 newCameraPosition = _anchorNode->worldPosition() - spinRotation * prevCameraToAnchor;

                    _camera->setPositionVec3(newCameraPosition);
                    decomp.globalRotation = spinRotation * decomp.globalRotation;
                }

                const glm::dvec3 projectedAim = _anchorNode->worldPosition() + newAnchorToProjectedAim;

                // Rotation based on increased aim distance from anchor
                const glm::dvec3 intermediateCameraToAnchor = _anchorNode->worldPosition() - _camera->positionVec3();
                const glm::dvec3 intermediateCameraToProjectedAim = projectedAim - _camera->positionVec3();

                double alpha = glm::angle(glm::normalize(intermediateCameraToAnchor), glm::normalize(intermediateCameraToProjectedAim));
                double ratio = glm::sin(alpha) * glm::length(intermediateCameraToAnchor) / glm::length(newAnchorToAim);


                ratio = glm::clamp(ratio, -1.0, 1.0);
                // Equation has no solution if ratio > 1.
                // To avoid a discontinuity in the camera behavior,
                // fade out the distance correction influence when ratio approaches 1.
                double correctionFactor = glm::clamp(1.0 - glm::pow(ratio, 50.0), 0.0, 1.0);
                double delta = glm::asin(ratio);

                if (glm::dot(intermediateCameraToAnchor, newAnchorToAim) <= 0 && // Camera on right half-plane
                    glm::dot(intermediateCameraToProjectedAim, newAnchorToAim) <= 0) // Camera is past aim in right half-plane
                {
                    delta = -glm::asin(ratio) + glm::pi<double>();
                }

                double beta = glm::angle(glm::normalize(-intermediateCameraToAnchor), glm::normalize(newAnchorToProjectedAim));

                const double gamma = glm::pi<double>() - alpha - delta;
                double distanceRotationAngle = correctionFactor * (gamma - beta);

                if (glm::abs(distanceRotationAngle) > AngleEpsilon) {
                    glm::dvec3 distanceRotationAxis = glm::normalize(glm::cross(intermediateCameraToAnchor, newAnchorToProjectedAim));
                    const glm::dquat orbitRotation = glm::angleAxis(distanceRotationAngle, distanceRotationAxis);

                    const glm::dvec3 newCameraPosition = _anchorNode->worldPosition() - orbitRotation * intermediateCameraToAnchor;
                    _camera->setPositionVec3(newCameraPosition);

                    const glm::dquat aimAdjustRotation = glm::angleAxis(distanceRotationAngle, distanceRotationAxis);
                    decomp.globalRotation = aimAdjustRotation * decomp.globalRotation;
                }

                _camera->setRotation(composeCameraRotation(decomp));
            }

            _previousAimNodePosition = _aimNode->worldPosition();
            _previousAimNodeRotation = _aimNode->worldRotationMatrix();
            _previousAnchorNodePosition = _anchorNode->worldPosition();
            _previousAnchorNodeRotation = _anchorNode->worldRotationMatrix();
        }

        // Read the current state of the camera
        glm::dvec3 camPos = _camera->positionVec3();
        const glm::dvec3 anchorPos = _anchorNode->worldPosition();
        const glm::dvec3 aimPos = _aimNode->worldPosition();

        // Follow focus nodes movement
        glm::dvec3 anchorNodeDiff = anchorPos - _previousAnchorNodePosition;
        glm::dvec3 aimNodeDiff = aimPos - _previousAimNodePosition;
        glm::dvec3 relativeAimNodeDiff = aimNodeDiff - anchorNodeDiff;

        _previousAnchorNodePosition = anchorPos;
        _previousAimNodePosition = aimPos;
        camPos += anchorNodeDiff;

        // Calculate a position handle based on the camera position in world space
        SurfacePositionHandle posHandle =
            calculateSurfacePositionHandle(*_anchorNode, camPos);

        // Decompose camera rotation so that we can handle global and local rotation
        // individually. Then we combine them again when finished.
        // Compensate for relative movement of aim node, in order to maintain the old global/local rotation.
        _camera->setPositionVec3(camPos + relativeAimNodeDiff);
        CameraRotationDecomposition camRotAnchor = decomposeCameraRotationSurface(*_camera, *_anchorNode);


        // Rotate with the object by finding a differential rotation from the previous
        // to the current rotation.
        glm::dquat anchorRotation =
            glm::quat_cast(_anchorNode->worldRotationMatrix());

        glm::dquat anchorNodeRotationDiff =
            _previousAnchorNodeRotation * glm::inverse(anchorRotation);

        _previousAnchorNodeRotation = anchorRotation;

        // Interpolate rotation differential so that the camera rotates with the object
        // only if close enough
        anchorNodeRotationDiff = interpolateRotationDifferential(
            deltaTime,
            1.0,
            anchorNodeRotationDiff,
            anchorPos,
            camPos,
            posHandle
        );

        // Update local rotation
        camRotAnchor.localRotation = roll(deltaTime, camRotAnchor.localRotation);

        _camera->setRotation(composeCameraRotation(camRotAnchor));
        _camera->setRotation(interpolateLocalRotation(deltaTime, *_camera));
        camRotAnchor = decomposeCameraRotationSurface(*_camera, *_anchorNode);

        camRotAnchor.localRotation = rotateLocally(deltaTime, camRotAnchor.localRotation);

        // Horizontal translation
        camPos = translateHorizontally(
            deltaTime,
            camPos,
            anchorPos,
            anchorNodeRotationDiff,
            camRotAnchor.globalRotation,
            posHandle
        );

        // Horizontal translation by focus node rotation
        camPos = followAnchorNodeRotation(
            camPos,
            anchorPos,
            anchorNodeRotationDiff
        );

        // Recalculate posHandle since horizontal position changed
        posHandle = calculateSurfacePositionHandle(*_anchorNode, camPos);


        // Rotate globally to keep camera rotation fixed
        // in the rotating reference frame of the anchor object.
        camRotAnchor.globalRotation = rotateGlobally(
            camRotAnchor.globalRotation,
            anchorNodeRotationDiff,
            posHandle
        );


        // Rotate around the surface out direction
        camRotAnchor.globalRotation = rotateHorizontally(
            deltaTime,
            camRotAnchor.globalRotation,
            camPos,
            posHandle
        );

        // Perform the vertical movements
        camPos = translateVertically(deltaTime, camPos, anchorPos, posHandle);
        camPos = pushToSurface(
            _minimumAllowedDistance,
            camPos,
            anchorPos,
            posHandle
        );

        // Update the camera state
        _camera->setPositionVec3(camPos);
        _camera->setRotation(composeCameraRotation(camRotAnchor));

        if (_useAdaptiveStereoscopicDepth) {
            double targetCameraToSurfaceDistance = glm::length(
                cameraToSurfaceVector(camPos, anchorPos, posHandle)
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

            _camera->setScaling(
                _stereoscopicDepthOfFocusSurface /
                static_cast<float>(_currentCameraToSurfaceDistance)
            );
        } else {
            _camera->setScaling(glm::pow(10.f, _staticViewScaleExponent));
        }
    }
}

glm::dquat OrbitalNavigator::composeCameraRotation(
    const CameraRotationDecomposition& decomposition)
{
    return decomposition.globalRotation * decomposition.localRotation;
}

Camera* OrbitalNavigator::camera() const
{
    return _camera;
}

void OrbitalNavigator::setCamera(Camera* camera) {
    _camera = camera;
}

glm::dvec3 OrbitalNavigator::cameraToSurfaceVector(const glm::dvec3& cameraPos,
                                                   const glm::dvec3& centerPos,
                                                   const SurfacePositionHandle& posHandle)
{
    glm::dmat4 modelTransform = _anchorNode->modelTransform();
    glm::dvec3 posDiff = cameraPos - centerPos;
    glm::dvec3 centerToActualSurfaceModelSpace =
        posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;

    glm::dvec3 centerToActualSurface =
        glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;

    return centerToActualSurface - posDiff;
}

void OrbitalNavigator::setFocusNode(SceneGraphNode* focusNode) {
    setAnchorNode(focusNode);
    setAimNode(focusNode);
}

void OrbitalNavigator::setFocusNode(const std::string& focusNode) {
    _anchor.set(focusNode);
    _aim.set(focusNode);
}

void OrbitalNavigator::setAnchorNode(SceneGraphNode* anchorNode) {
    if (!_anchorNode) {
        _directlySetStereoDistance = true;
    }

    _anchorNode = anchorNode;

    if (_anchorNode) {
        _previousAnchorNodePosition = _anchorNode->worldPosition();
        _previousAnchorNodeRotation = glm::quat_cast(_anchorNode->worldRotationMatrix());
    }
}

void OrbitalNavigator::setAimNode(SceneGraphNode* aimNode) {
    _aimNode = aimNode;

    if (_aimNode) {
        _previousAimNodePosition = _aimNode->worldPosition();
        _previousAimNodeRotation = glm::quat_cast(_aimNode->worldRotationMatrix());
    }
}

void OrbitalNavigator::setAnchorNode(const std::string& anchorNode) {
    _anchor.set(anchorNode);
}

void OrbitalNavigator::setAimNode(const std::string& aimNode) {
    _aim.set(aimNode);
}

void OrbitalNavigator::startRetargetAnchor() {
    const glm::dvec3 camPos = _camera->positionVec3();
    const glm::dvec3 camDir = glm::normalize(
        _camera->rotationQuaternion() * glm::dvec3(0.0, 0.0, -1.0)
    );
    const glm::dvec3 centerPos = _anchorNode->worldPosition();
    const glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    const double angle = glm::angle(camDir, directionToCenter);

    // Minimum is two second. Otherwise proportional to angle
    _rotateToAnchorInterpolator.setInterpolationTime(static_cast<float>(
        glm::max(angle, 1.0) * _rotateInterpolationTime
    ));
    _rotateToAnchorInterpolator.start();

    _cameraToSurfaceDistanceInterpolator.setInterpolationTime(_stereoInterpolationTime);
    _cameraToSurfaceDistanceInterpolator.start();
}

void OrbitalNavigator::startRetargetAim() {
    const glm::dvec3 camPos = _camera->positionVec3();
    const glm::dvec3 camDir = glm::normalize(
        _camera->rotationQuaternion() * glm::dvec3(0.0, 0.0, -1.0)
    );
    const glm::dvec3 centerPos = _aimNode->worldPosition();
    const glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    const double angle = glm::angle(camDir, directionToCenter);

    // Minimum is two second. Otherwise proportional to angle
    _rotateToAimInterpolator.setInterpolationTime(static_cast<float>(
        glm::max(angle, 1.0) * _rotateInterpolationTime
        ));
    _rotateToAimInterpolator.start();

    _cameraToSurfaceDistanceInterpolator.setInterpolationTime(_stereoInterpolationTime);
    _cameraToSurfaceDistanceInterpolator.start();
}


float OrbitalNavigator::rotateToAimInterpolationTime() const {
    return _rotateInterpolationTime;
}

void OrbitalNavigator::setRotateInterpolationTime(float durationInSeconds) {
    _rotateInterpolationTime = durationInSeconds;
}

bool OrbitalNavigator::followingNodeRotation() const {
    return _followRotationInterpolator.value() >= 1.0;
}

SceneGraphNode* OrbitalNavigator::anchorNode() const {
    return _anchorNode;
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

OrbitalNavigator::CameraRotationDecomposition OrbitalNavigator::decomposeCameraRotationSurface(
    const Camera& camera, const SceneGraphNode& reference)
{
    glm::dvec3 cameraPosition = camera.positionVec3();
    glm::dquat cameraRotation = camera.rotationQuaternion();
    glm::dvec3 cameraLookUp = camera.lookUpVectorWorldSpace();
    glm::dvec3 cameraViewDirection = camera.viewDirectionWorldSpace();

    const glm::dmat4 inverseModelTransform = reference.inverseModelTransform();
    const glm::dmat4 modelTransform = reference.modelTransform();
    const glm::dvec3 cameraPositionModelSpace = glm::dvec3(inverseModelTransform *
                                                glm::dvec4(cameraPosition, 1));

    const SurfacePositionHandle posHandle =
        reference.calculateSurfacePositionHandle(cameraPositionModelSpace);

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

OrbitalNavigator::CameraRotationDecomposition OrbitalNavigator::decomposeCameraRotationOrigin(
    const Camera& camera, const SceneGraphNode& reference)
{
    glm::dvec3 cameraPosition = camera.positionVec3();
    glm::dquat cameraRotation = camera.rotationQuaternion();
    glm::dvec3 cameraLookUp = camera.lookUpVectorWorldSpace();
    glm::dvec3 cameraViewDirection = camera.viewDirectionWorldSpace();

    // To avoid problem with lookup in up direction we adjust is with the view direction
    const glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0.0, 0.0, 0.0),
        reference.worldPosition() - cameraPosition,
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
                                                      const Camera& camera)
{
    if (_rotateToAnchorInterpolator.isInterpolating()) {
        CameraRotationDecomposition decomp =
            decomposeCameraRotationSurface(camera, *_anchorNode);

        const double t = _rotateToAnchorInterpolator.value();
        _rotateToAnchorInterpolator.setDeltaTime(static_cast<float>(deltaTime));
        _rotateToAnchorInterpolator.step();
        const glm::dquat result = glm::slerp(
            decomp.localRotation,
            glm::dquat(glm::dvec3(0.0)),
            glm::min(t * _rotateToAnchorInterpolator.deltaTimeScaled(), 1.0));

        // Retrieving the angle of a quaternion uses acos on the w component, which can
        // have numerical instability for values close to 1.0
        constexpr double Epsilon = 1.0e-13;
        if (abs((abs(result.w) - 1.0)) < Epsilon || angle(result) < 0.01) {
            _rotateToAnchorInterpolator.end();
        }

        decomp.localRotation = result;
        return composeCameraRotation(decomp);
    }

    if (_rotateToAimInterpolator.isInterpolating()) {
        CameraRotationDecomposition decomp =
            decomposeCameraRotationOrigin(camera, *_aimNode);

        const double t = _rotateToAimInterpolator.value();
        _rotateToAimInterpolator.setDeltaTime(static_cast<float>(deltaTime));
        _rotateToAimInterpolator.step();
        const glm::dquat result = glm::slerp(
            decomp.localRotation,
            glm::dquat(glm::dvec3(0.0)),
            glm::min(t * _rotateToAimInterpolator.deltaTimeScaled(), 1.0));

        // Retrieving the angle of a quaternion uses acos on the w component, which can
        // have numerical instability for values close to 1.0
        constexpr double Epsilon = 1.0e-13;
        if (abs((abs(result.w) - 1.0)) < Epsilon || angle(result) < 0.01) {
            _rotateToAimInterpolator.end();
        }

        decomp.localRotation = result;
        return composeCameraRotation(decomp);
    }
    return camera.rotationQuaternion();
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
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

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
    const glm::dvec3 rotationDiffVec3 = 
        (distFromCenterToCamera * outDirection) * rotationDiffWorldSpace -
        (distFromCenterToCamera * outDirection);

    // Add difference to position
    return cameraPosition + rotationDiffVec3;
}

glm::dvec3 OrbitalNavigator::followAnchorNodeRotation(const glm::dvec3& cameraPosition,
                                                     const glm::dvec3& objectPosition,
                                            const glm::dquat& focusNodeRotationDiff) const
{
    const glm::dvec3 posDiff = cameraPosition - objectPosition;

    const glm::dvec3 rotationDiffVec3AroundCenter =
        posDiff * focusNodeRotationDiff - posDiff;

    return cameraPosition + rotationDiffVec3AroundCenter;
}

glm::dquat OrbitalNavigator::rotateGlobally(const glm::dquat& globalCameraRotation,
                                            const glm::dquat& focusNodeRotationDiff,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

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
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

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
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

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
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

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
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

    const double maximumDistanceForRotation = glm::length(
        glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface
    ) * _followAnchorNodeRotationDistance;
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
                                                SceneGraphNode& node,
                                                const glm::dvec3 cameraPositionWorldSpace)
{
    const glm::dmat4 inverseModelTransform = node.inverseModelTransform();
    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPositionWorldSpace, 1));
    const SurfacePositionHandle posHandle =
        node.calculateSurfacePositionHandle(cameraPositionModelSpace);

    return posHandle;
}

JoystickCameraStates& OrbitalNavigator::joystickStates() {
    return _joystickStates;
}

} // namespace openspace::interaction
