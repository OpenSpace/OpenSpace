/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/camera/camerapose.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/mouseinputstate.h>
#include <openspace/interaction/keyboardinputstate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <openspace/query/query.h>
#include <openspace/engine/globals.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/easing.h>
#include <glm/gtx/rotate_vector.hpp>
#include <glm/gtx/vector_angle.hpp>
#include <cmath>

#include "orbitalnavigator_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "OrbitalNavigator";

    constexpr double AngleEpsilon = 1e-7;
    constexpr double DistanceRatioAimThreshold = 1e-4;

    constexpr std::string_view IdleKeyOrbit = "Orbit";
    constexpr std::string_view IdleKeyOrbitAtConstantLat = "OrbitAtConstantLatitude";
    constexpr std::string_view IdleKeyOrbitAroundUp = "OrbitAroundUp";

    constexpr openspace::properties::Property::PropertyInfo AnchorInfo = {
        "Anchor",
        "Anchor",
        "The name of the scene graph node that is the origin of the camera interaction. "
        "The camera follows, orbits and dollies towards this node. Any scene graph node "
        "can be the anchor node",
        // @VISIBILITY(1.33)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo AimInfo = {
        "Aim",
        "Aim",
        "The name of the scene graph node that is the aim of the camera. The camera "
        "direction is relative to the vector from the camera position to this node",
        // @VISIBILITY(1.67)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RetargetAnchorInfo = {
        "RetargetAnchor",
        "Retarget Anchor",
        "When triggered, this property starts an interpolation to reset the "
        "camera direction to the anchor node",
        // @VISIBILITY(1.33)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RetargetAimInfo = {
        "RetargetAim",
        "Retarget Aim",
        "When triggered, this property starts an interpolation to reset the "
        "camera direction to the aim node",
        // @VISIBILITY(1.67)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RollFrictionInfo = {
        "RollFriction",
        "Roll Friction",
        "If this is enabled, a small friction is applied to the rolling part of the "
        "camera motion, thus slowing it down within a small time period. If this value "
        "is disabled, the camera will roll forever",
        // @VISIBILITY(1.33)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RotationalFrictionInfo = {
        "RotationalFriction",
        "Rotational Friction",
        "If this is enabled, a small friction is applied to the rotational part of the "
        "camera motion, thus slowing it down within a small time period. If this value "
        "is disabled, the camera will rotate forever",
        // @VISIBILITY(1.33)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ZoomFrictionInfo = {
        "ZoomFriction",
        "Zoom Friction",
        "If this is enabled, a small friction is applied to the zoom part of the camera "
        "motion, thus slowing it down within a small time period. If this value is "
        "disabled, the camera will zoom in or out forever",
        // @VISIBILITY(1.33)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo MouseSensitivityInfo = {
        "MouseSensitivity",
        "Mouse Sensitivity",
        "Determines the sensitivity of the camera motion thorugh the mouse. The lower "
        "the sensitivity is the less impact a mouse motion will have",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo JoystickSensitivityInfo = {
        "JoystickSensitivity",
        "Joystick Sensitivity",
        "Determines the sensitivity of the camera motion thorugh a joystick. The lower "
        "the sensitivity is the less impact a joystick motion will have",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo WebsocketSensitivityInfo = {
        "WebsocketSensitivity",
        "Websocket Sensitivity",
        "Determines the sensitivity of the camera motion thorugh a websocket. The lower "
        "the sensitivity is the less impact a webstick motion will have",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FrictionInfo = {
        "Friction",
        "Friction Factor",
        "Determines the factor that is applied if the 'Roll Friction', 'Rotational "
        "Friction', and 'Zoom Friction' values are enabled. The lower this value is, the "
        "faster the camera movements will stop",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FollowAnchorNodeInfo = {
        "FollowAnchorNodeRotation",
        "Follow Anchor Node Rotation",
        "If true, the camera will rotate with the current achor node if within a "
        "certain distance from it. When this happens, the object will appear fixed in "
        "relation to the camera. The distance at which the change happens is controlled "
        "through another property",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FollowAnchorNodeDistanceInfo =
    {
        "FollowAnchorNodeRotationDistance",
        "Follow Anchor Node Rotation Distance",
        "A factor used to determine the distance at which the camera starts rotating "
        "with the anchor node. The actual distance will be computed by multiplying "
        "this factor with the approximate radius of the node",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StereoInterpolationTimeInfo =
    {
        "StereoInterpolationTime",
        "Stereo Interpolation Time",
        "The time to interpolate to a new stereoscopic depth when the anchor node is "
        "changed, in seconds",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        RetargetInterpolationTimeInfo =
    {
        "RetargetAnchorInterpolationTime",
        "Retarget Interpolation Time",
        "The time to interpolate the camera rotation when the anchor or aim node is "
        "changed, in seconds",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FollowRotationInterpTimeInfo =
    {
        "FollowRotationInterpolationTime",
        "Follow Rotation Interpolation Time",
        "The interpolation time when toggling following focus node rotation",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo InvertMouseButtons = {
        "InvertMouseButtons",
        "Invert Left and Right Mouse Buttons",
        "If this value is 'false', the left mouse button causes the camera to rotate "
        "around the object and the right mouse button causes the zooming motion. If this "
        "value is 'true', these two functionalities are reversed",
        // @VISIBILITY(1.33)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        UseAdaptiveStereoscopicDepthInfo =
    {
        "UseAdaptiveStereoscopicDepth",
        "Adaptive Steroscopic Depth",
        "Dynamically adjust the view scaling based on the distance to the surface of "
        "the anchor and aim nodes. If enabled, view scale will be set to "
        "StereoscopicDepthOfFocusSurface / min(anchorDistance, aimDistance). "
        "If disabled, view scale will be set to 10^StaticViewScaleExponent",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StaticViewScaleExponentInfo =
    {
        "StaticViewScaleExponent",
        "Static View Scale Exponent",
        "Statically scale the world by 10^StaticViewScaleExponent. Only used if "
        "UseAdaptiveStereoscopicDepthInfo is set to false",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        StereoscopicDepthOfFocusSurfaceInfo =
    {
        "StereoscopicDepthOfFocusSurface",
        "Stereoscopic Depth of the Surface in Focus",
        "Set the stereoscopically perceived distance (in meters) to the closest point "
        "out of the surface of the anchor and the center of the aim node. Only used if "
        "UseAdaptiveStereoscopicDepthInfo is set to true",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ConstantVelocityFlight = {
        "ConstantVelocityFlight",
        "Constant Velocity Flight",
        "If this value is enabled, the camera motion will not be affected by the "
        "distance of the camera to the surface of a planet. When enabling this setting "
        "consider adjusting the mouse sensitivity to a lower value",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyIdleBehaviorInfo = {
        "ApplyIdleBehavior",
        "Apply Idle Behavior",
        "When set to true, the chosen idle behavior will be applied to the camera, "
        "moving the camera accordingly",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo IdleBehaviorInfo = {
        "IdleBehavior",
        "Idle Behavior",
        "The chosen camera behavior that will be triggered when the idle behavior is "
        "applied. Each option represents a predefined camera behavior",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        ShouldTriggerIdleBehaviorWhenIdleInfo =
    {
        "ShouldTriggerWhenIdle",
        "Should Trigger When Idle",
        "If true, the chosen idle behavior will trigger automatically after a certain "
        "time (see 'IdleWaitTime' property)",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo IdleWaitTimeInfo = {
        "IdleWaitTime",
        "Idle Wait Time",
        "The time (seconds) until idle behavior starts, if no camera interaction "
        "has been performed. Note that friction counts as camera interaction",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IdleBehaviorSpeedInfo = {
        "SpeedFactor",
        "Speed Factor",
        "A factor that can be used to increase or slow down the speed of an applied "
        "idle behavior. A negative value will invert the direction. Note that a speed "
        "of exactly 0 leads to no movement at all",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo InvertIdleBehaviorInfo = {
        "Invert",
        "Invert",
        "If true, the direction of the idle behavior motion will be inverted compared "
        "to the default. For example, the 'Orbit' option rotates to the right per "
        "default, and will rotate to the left when inverted",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AbortOnCameraInteractionInfo =
    {
        "AbortOnCameraInteraction",
        "Abort on Camera Interaction",
        "If set to true, the idle behavior is aborted on camera interaction. If false, "
        "the behavior will be reapplied after the interaction. Examples of camera "
        "interaction are: changing the anchor node, starting a camera path or session "
        "recording playback, or navigating manually using an input device",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo
        IdleBehaviorDampenInterpolationTimeInfo =
    {
        "DampenInterpolationTime",
        "Start/End Dampen Interpolation Time",
        "The time to interpolate to/from full speed when an idle behavior is triggered "
        "or canceled, in seconds",
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo LimitZoomInfo = {
        "LimitZoom",
        "Limit Zoom",
        "Settings to limit the camera from going to close to or too far away from the "
        "current focus"
    };

    constexpr openspace::properties::Property::PropertyInfo
        EnabledMinimumAllowedDistanceInfo =
    {
        "EnabledMinimumAllowedDistance",
        "Enable minimum allowed distance limit",
        "Enables or disables that the camera cannot go closer to an object than "
        "the set minimum allowed distance",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MinimumDistanceInfo = {
        "MinimumAllowedDistance",
        "Minimum Allowed Distance",
        "The limit of how close the camera can get to an object. The distance is given "
        "in meters above the surface",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledMaximumDistanceInfo = {
        "EnableMaximumAllowedDistance",
        "Enable Maximum Allowed Distance limit",
        "Enables or disables that the camera cannot go further away from an object than "
        "the set maximum allowed distance",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaximumDistanceInfo = {
        "MaximumAllowedDistance",
        "Maximum Allowed Distance",
        "The limit of how far away the camera can get from an object. The distance is "
        "given in meters above the surface",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableZoomInfo = {
        "DisableZoom",
        "Disable Zoom",
        "When set to true, disables all vertical navigation based on input. This means "
        "that the camera cannot be moved closer to or further away from the current "
        "anchor node",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableRollInfo = {
        "DisableRoll",
        "Disable Roll",
        "When set to true, disables all rolling camera motions based on input. This "
        "means that the camera cannot be rotated to change the perceived up-direction "
        "of the current anchor node, or rotate the horizon on a planet, for example",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShouldRotateAroundUpInfo = {
        "ShouldRotateAroundUp",
        "Should Rotate Around Up",
        "When set to true, global rotation interactions in the X-direction will lead to "
        "a rotation around the specified up vector instead of just around the object. "
        "The up vector is the local coordinate axis, and can be set to either the X-, Y- "
        "or Z-axis through the 'UpToUseForRotation' property",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UpToUseForRotationInfo = {
        "UpToUseForRotation",
        "Up To Use For Rotation",
        "Specifies the local coordinate axis of the anchor node to use as up direction "
        "when the camera is set to orbit around up. In general, the Z-axis is a good "
        "choice for globes, and the Y-axis is a good choice for models",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    /**
     * Calculates a SurfacePositionHandle given a camera position in world space.
     */
    openspace::SurfacePositionHandle calculateSurfacePositionHandle(
                                                    const openspace::SceneGraphNode& node,
                                               const glm::dvec3& cameraPositionWorldSpace)
    {
        ghoul_assert(
            glm::length(cameraPositionWorldSpace) > 0.0,
            "Cannot have degenerate vector"
        );

        const glm::dmat4 inverseModelTransform = glm::inverse(node.modelTransform());
        const glm::dvec3 cameraPositionModelSpace =
            glm::dvec3(inverseModelTransform * glm::dvec4(cameraPositionWorldSpace, 1.0));
        const openspace::SurfacePositionHandle posHandle =
            node.calculateSurfacePositionHandle(cameraPositionModelSpace);

        return posHandle;
    }

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

OrbitalNavigator::IdleBehavior::IdleBehavior()
    : properties::PropertyOwner({ "IdleBehavior", "Idle Behavior" })
    , apply(ApplyIdleBehaviorInfo, false)
    , shouldTriggerWhenIdle(ShouldTriggerIdleBehaviorWhenIdleInfo, false)
    , idleWaitTime(IdleWaitTimeInfo, 5.f, 0.f, 3600.f)
    , abortOnCameraInteraction(AbortOnCameraInteractionInfo, true)
    , invert(InvertIdleBehaviorInfo, false)
    , speedScaleFactor(IdleBehaviorSpeedInfo, 1.f, -5.f, 5.f)
    , dampenInterpolationTime(IdleBehaviorDampenInterpolationTimeInfo, 0.5f, 0.f, 10.f)
    , defaultBehavior(IdleBehaviorInfo)
{
    addProperty(apply);
    defaultBehavior.addOptions({
        {
            static_cast<int>(Behavior::Orbit),
            std::string(IdleKeyOrbit)
        },
        {
            static_cast<int>(Behavior::OrbitAtConstantLat),
            std::string(IdleKeyOrbitAtConstantLat)
        },
        {
            static_cast<int>(Behavior::OrbitAroundUp),
            std::string(IdleKeyOrbitAroundUp)
        }
    });
    defaultBehavior = static_cast<int>(IdleBehavior::Behavior::Orbit);
    addProperty(defaultBehavior);
    addProperty(shouldTriggerWhenIdle);
    addProperty(idleWaitTime);
    idleWaitTime.setExponent(2.2f);
    addProperty(invert);
    addProperty(speedScaleFactor);
    addProperty(abortOnCameraInteraction);
    addProperty(dampenInterpolationTime);
}

OrbitalNavigator::LimitZoom::LimitZoom()
    : properties::PropertyOwner(LimitZoomInfo)
    , enableZoomInLimit(EnabledMinimumAllowedDistanceInfo, true)
    , minimumAllowedDistance(MinimumDistanceInfo, 10.f, 0.f, 10000.f)
    , enableZoomOutLimit(EnabledMaximumDistanceInfo, false)
    , maximumAllowedDistance(
        MaximumDistanceInfo,
        4e+27f,
        50.f,
        4e+27f
    )
{
    // Min
    addProperty(enableZoomInLimit);
    addProperty(minimumAllowedDistance);

    // Max
    addProperty(enableZoomOutLimit);
    addProperty(maximumAllowedDistance);
    maximumAllowedDistance.setExponent(20.f);
}

OrbitalNavigator::OrbitalNavigator()
    : properties::PropertyOwner({ "OrbitalNavigator", "Orbital Navigator" })
    , _anchor(AnchorInfo)
    , _aim(AimInfo)
    , _retargetAnchor(RetargetAnchorInfo)
    , _retargetAim(RetargetAimInfo)
    , _followAnchorNodeRotation(FollowAnchorNodeInfo, true)
    , _followAnchorNodeRotationDistance(FollowAnchorNodeDistanceInfo, 5.f, 0.f, 20.f)
    , _disableZoom(DisableZoomInfo, false)
    , _disableRoll(DisableRollInfo, false)
    , _mouseSensitivity(MouseSensitivityInfo, 15.f, 1.f, 50.f)
    , _joystickSensitivity(JoystickSensitivityInfo, 10.f, 1.f, 50.f)
    , _websocketSensitivity(WebsocketSensitivityInfo, 5.f, 1.f, 50.f)
    , _useAdaptiveStereoscopicDepth(UseAdaptiveStereoscopicDepthInfo, true)
    , _stereoscopicDepthOfFocusSurface(
        StereoscopicDepthOfFocusSurfaceInfo,
        21500,
        0.25,
        500000
    )
    , _staticViewScaleExponent(StaticViewScaleExponentInfo, 0.f, -30, 10)
    , _constantVelocityFlight(ConstantVelocityFlight, false)
    , _retargetInterpolationTime(RetargetInterpolationTimeInfo, 2.0, 0.0, 10.0)
    , _stereoInterpolationTime(StereoInterpolationTimeInfo, 8.0, 0.0, 10.0)
    , _followRotationInterpolationTime(FollowRotationInterpTimeInfo, 1.0, 0.0, 10.0)
    , _invertMouseButtons(InvertMouseButtons, false)
    , _shouldRotateAroundUp(ShouldRotateAroundUpInfo, false)
    , _upToUseForRotation(UpToUseForRotationInfo)
    , _mouseStates(_mouseSensitivity * 0.0001, 1 / (_friction.friction + 0.0000001))
    , _joystickStates(_joystickSensitivity * 0.1, 1 / (_friction.friction + 0.0000001))
    , _websocketStates(_websocketSensitivity, 1 / (_friction.friction + 0.0000001))
{
    _anchor.onChange([this]() {
        if (_anchor.value().empty()) {
            return;
        }
        SceneGraphNode* node = sceneGraphNode(_anchor.value());
        if (node) {
            const SceneGraphNode* previousAnchor = _anchorNode;
            setAnchorNode(node);
            global::eventEngine->publishEvent<events::EventFocusNodeChanged>(
                previousAnchor,
                node
            );
        }
        else {
            LERROR(fmt::format(
                "No scenegraph node with identifier '{}' exists", _anchor.value()
            ));
        }
    });

    _aim.onChange([this]() {
        if (_aim.value().empty()) {
            setAimNode(nullptr);
            return;
        }
        SceneGraphNode* node = sceneGraphNode(_aim.value());
        if (node) {
            setAimNode(node);
        }
        else {
            LERROR(fmt::format(
                "No scenegraph node with identifier '{}' exists", _aim.value()
            ));
        }
    });

    _retargetAnchor.onChange([this]() {
        startRetargetAnchor();
    });

    _retargetAim.onChange([this]() {
        if (_aimNode && _aimNode != _anchorNode) {
            startRetargetAim();
        }
        else {
            startRetargetAnchor();
        }
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
    // f(t) = d/dt (ln(1 / f_orig(t))) where f_orig is the transfer function that would
    // be used if the interpolation was sinply linear between a start value and an end
    // value instead of current value and end value (0) as we use it when inerpoláting.
    // As an example f_orig(t) = 1 - t yields f(t) = 1 / (1 - t) which results in a linear
    // interpolation from 1 to 0.
    auto smoothStepDerivedTranferFunction = [](double t) {
        return (6 * (t + t*t) / (1 - 3 * t*t + 2 * t*t*t));
    };
    _retargetAnchorInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);
    _retargetAimInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);
    _cameraToSurfaceDistanceInterpolator.setTransferFunction(
        smoothStepDerivedTranferFunction
    );

    // Define callback functions for changed properties
    _friction.roll.onChange([this]() {
        _mouseStates.setRotationalFriction(_friction.roll);
        _joystickStates.setRotationalFriction(_friction.roll);
        _websocketStates.setRotationalFriction(_friction.roll);
    });
    _friction.rotational.onChange([this]() {
        _mouseStates.setHorizontalFriction(_friction.rotational);
        _joystickStates.setHorizontalFriction(_friction.rotational);
        _websocketStates.setHorizontalFriction(_friction.rotational);
    });
    _friction.zoom.onChange([this]() {
        _mouseStates.setVerticalFriction(_friction.zoom);
        _joystickStates.setVerticalFriction(_friction.zoom);
        _websocketStates.setVerticalFriction(_friction.zoom);
    });
    _friction.friction.onChange([this]() {
        _mouseStates.setVelocityScaleFactor(1 / (_friction.friction + 0.0000001));
        _joystickStates.setVelocityScaleFactor(1 / (_friction.friction + 0.0000001));
        _websocketStates.setVelocityScaleFactor(1 / (_friction.friction + 0.0000001));
    });

    _mouseSensitivity.onChange([this]() {
        _mouseStates.setSensitivity(_mouseSensitivity * pow(10.0, -4));
    });
    _joystickSensitivity.onChange([this]() {
        _joystickStates.setSensitivity(_joystickSensitivity * 0.1);
    });
    _websocketSensitivity.onChange([this]() {
        _websocketStates.setSensitivity(_websocketSensitivity);
    });

    addPropertySubOwner(_friction);
    addPropertySubOwner(_idleBehavior);
    addPropertySubOwner(_limitZoom);

    _idleBehaviorDampenInterpolator.setTransferFunction(
        ghoul::quadraticEaseInOut<double>
    );
    _idleBehavior.dampenInterpolationTime.onChange([this]() {
        _idleBehaviorDampenInterpolator.setInterpolationTime(
            _idleBehavior.dampenInterpolationTime
        );
     });
    _idleBehavior.apply.onChange([this]() {
        if (_idleBehavior.apply) {
            // Reset velocities to ensure that abort on interaction works correctly
            resetVelocities();
            _invertIdleBehaviorInterpolation = false;
        }
        else {
            _invertIdleBehaviorInterpolation = true;
        }
        _idleBehaviorDampenInterpolator.start();
        _idleBehaviorDampenInterpolator.setInterpolationTime(
            _idleBehavior.dampenInterpolationTime
        );
    });
    _idleBehavior.shouldTriggerWhenIdle.onChange([this]() {
        _idleBehaviorTriggerTimer = _idleBehavior.idleWaitTime;
    });
    _idleBehavior.idleWaitTime.onChange([this]() {
        _idleBehaviorTriggerTimer = _idleBehavior.idleWaitTime;
    });

    addProperty(_anchor);
    addProperty(_aim);
    addProperty(_retargetAnchor);
    addProperty(_retargetAim);
    addProperty(_followAnchorNodeRotation);
    addProperty(_followAnchorNodeRotationDistance);

    addProperty(_useAdaptiveStereoscopicDepth);
    addProperty(_staticViewScaleExponent);
    addProperty(_constantVelocityFlight);
    _stereoscopicDepthOfFocusSurface.setExponent(3.f);
    addProperty(_stereoscopicDepthOfFocusSurface);

    addProperty(_retargetInterpolationTime);
    addProperty(_stereoInterpolationTime);

    _followRotationInterpolationTime.onChange([this]() {
        _followRotationInterpolator.setInterpolationTime(
            _followRotationInterpolationTime
        );
    });
    _followRotationInterpolator.setInterpolationTime(_followRotationInterpolationTime);
    addProperty(_followRotationInterpolationTime);

    _invertMouseButtons.onChange([this]() {
        _mouseStates.setInvertMouseButton(_invertMouseButtons);
    });
    addProperty(_invertMouseButtons);

    addProperty(_mouseSensitivity);
    addProperty(_joystickSensitivity);
    addProperty(_websocketSensitivity);

    addProperty(_disableZoom);
    _disableZoom.onChange([this]() {
        if (_disableZoom) {
            LWARNING(
                "Zooming has been disabled. No vertical camera motion based on "
                "input will occur until re-enabled. See setting in Orbital Navigator"
            );
        }
        else {
            LINFO("Zooming has been enabled");
        }
    });
    addProperty(_disableRoll);
    _disableRoll.onChange([this]() {
        if (_disableRoll) {
            LWARNING(
                "Camera roll has been disabled. No rolling camera motion based on "
                "input will occur until re-enabled. See setting in Orbital Navigator"
            );
        }
        else {
            LINFO("Camera roll has been enabled");
        }
    });

    addProperty(_shouldRotateAroundUp);
    _upToUseForRotation.addOptions({
        { static_cast<int>(UpDirectionChoice::XAxis), "Local X" },
        { static_cast<int>(UpDirectionChoice::YAxis), "Local Y" },
        { static_cast<int>(UpDirectionChoice::ZAxis), "Local Z" }
    });
    _upToUseForRotation = static_cast<int>(UpDirectionChoice::ZAxis);
    addProperty(_upToUseForRotation);
}

glm::dvec3 OrbitalNavigator::anchorNodeToCameraVector() const {
    return _camera->positionVec3() - anchorNode()->worldPosition();
}

glm::quat OrbitalNavigator::anchorNodeToCameraRotation() const {
    const glm::dmat4 invWorldRotation = glm::dmat4(
        glm::inverse(anchorNode()->worldRotationMatrix())
    );
    return glm::quat(invWorldRotation) * glm::quat(_camera->rotationQuaternion());
}


glm::dvec3 OrbitalNavigator::pushToSurfaceOfAnchor(
                                                  const glm::dvec3& cameraPosition) const
{
    const SurfacePositionHandle posHandle =
        calculateSurfacePositionHandle(*_anchorNode, cameraPosition);

     return pushToSurface(
         cameraPosition,
         _anchorNode->worldPosition(),
         posHandle
    );
}

void OrbitalNavigator::resetVelocities() {
    _mouseStates.resetVelocities();
    _joystickStates.resetVelocities();
    _websocketStates.resetVelocities();
    _scriptStates.resetVelocities();

    if (shouldFollowAnchorRotation(_camera->positionVec3())) {
        _followRotationInterpolator.end();
    }
    else {
        _followRotationInterpolator.start();
    }
}

void OrbitalNavigator::updateStatesFromInput(const MouseInputState& mouseInputState,
                                             const KeyboardInputState& keyboardInputState,
                                             double deltaTime)
{
    _mouseStates.updateStateFromInput(mouseInputState, keyboardInputState, deltaTime);
    _joystickStates.updateStateFromInput(*global::joystickInputStates, deltaTime);
    _websocketStates.updateStateFromInput(*global::websocketInputStates, deltaTime);
    _scriptStates.updateStateFromInput(deltaTime);

    const bool interactionHappened =
        _mouseStates.hasNonZeroVelocities() ||
        _joystickStates.hasNonZeroVelocities() ||
        _websocketStates.hasNonZeroVelocities() ||
        _scriptStates.hasNonZeroVelocities();

    if (interactionHappened) {
        updateOnCameraInteraction();
    }
    else {
        tickIdleBehaviorTimer(deltaTime);
    }

    const bool cameraLocationChanged =
        _mouseStates.hasNonZeroVelocities(true) ||
        _joystickStates.hasNonZeroVelocities(true) ||
        _websocketStates.hasNonZeroVelocities(true) ||
        _scriptStates.hasNonZeroVelocities(true);

    if (cameraLocationChanged && (_movementTimer < 0.f)) {
        global::eventEngine->publishEvent<events::EventCameraMovedPosition>();
        _movementTimer = _idleBehavior.idleWaitTime;
    }
    else if (!cameraLocationChanged) {
        tickMovementTimer(static_cast<float>(deltaTime));
    }
}

void OrbitalNavigator::updateCameraStateFromStates(double deltaTime) {
    if (!_anchorNode) {
        // Bail out if the anchor node is not set
        return;
    }

    const glm::dvec3 anchorPos = _anchorNode->worldPosition();

    const glm::dvec3 prevCameraPosition = _camera->positionVec3();
    const glm::dvec3 anchorDisplacement = _previousAnchorNodePosition.has_value() ?
        (anchorPos - *_previousAnchorNodePosition) :
        glm::dvec3(0.0);

    CameraPose pose = {
        .position = _camera->positionVec3() + anchorDisplacement,
        .rotation = _camera->rotationQuaternion()
    };

    if (glm::length(pose.position) == 0.0) {
        // If the position is 0.0, a lot of the calculations downstairs will fail as we
        // calculate relative offsets from the center of the anchor node
        return;
    }

    const bool hasPreviousPositions =
        _previousAnchorNodePosition.has_value() &&
        _previousAimNodePosition.has_value();

    if (_aimNode && _aimNode != _anchorNode && hasPreviousPositions) {
        const glm::dvec3 aimPos = _aimNode->worldPosition();
        const glm::dvec3 cameraToAnchor =
            *_previousAnchorNodePosition - prevCameraPosition;

        Displacement anchorToAim = Displacement(
            *_previousAimNodePosition - *_previousAnchorNodePosition,
            aimPos - anchorPos
        );

        anchorToAim = interpolateRetargetAim(
            deltaTime,
            pose,
            cameraToAnchor,
            anchorToAim
        );

        pose = followAim(pose, cameraToAnchor, anchorToAim);

        _previousAimNodePosition = _aimNode->worldPosition();
        _previousAnchorNodeRotation = _anchorNode->worldRotationMatrix();
    }

    _previousAnchorNodePosition = _anchorNode->worldPosition();

    // Calculate a position handle based on the camera position in world space
    SurfacePositionHandle posHandle =
        calculateSurfacePositionHandle(*_anchorNode, pose.position);

    // Decompose camera rotation so that we can handle global and local rotation
    // individually. Then we combine them again when finished.
    // Compensate for relative movement of aim node,
    // in order to maintain the old global/local rotation
    CameraRotationDecomposition camRot =
        decomposeCameraRotationSurface(pose, *_anchorNode);

    // Rotate with the object by finding a differential rotation from the previous
    // to the current rotation
    const glm::dquat anchorRotation = glm::quat_cast(_anchorNode->worldRotationMatrix());

    glm::dquat anchorNodeRotationDiff = _previousAnchorNodeRotation.has_value() ?
        *_previousAnchorNodeRotation * glm::inverse(anchorRotation) :
        glm::dquat(1.0, 0.0, 0.0, 0.0);

    _previousAnchorNodeRotation = anchorRotation;

    // Interpolate rotation differential so that the camera rotates with the object
    // only if close enough
    anchorNodeRotationDiff = interpolateRotationDifferential(
        deltaTime,
        pose.position,
        anchorNodeRotationDiff
    );

    // Update local rotation based on user input
    if (!_disableRoll) {
        camRot.localRotation = roll(deltaTime, camRot.localRotation);
    }

    camRot.localRotation = interpolateLocalRotation(deltaTime, camRot.localRotation);
    camRot.localRotation = rotateLocally(deltaTime, camRot.localRotation);

    const double horizontalTranslationSpeedScale =
        rotationSpeedScaleFromCameraHeight(pose.position, posHandle);

    // Rotation around target object's up vector based on user input
    // (one kind of horizontal translation)
    // Affects the position and global rotation
    if (_shouldRotateAroundUp) {
        rotateAroundAnchorUp(
            deltaTime,
            horizontalTranslationSpeedScale,
            pose.position,
            camRot.globalRotation
        );
    }

    // Horizontal translation based on user input
    pose.position = translateHorizontally(
        deltaTime,
        horizontalTranslationSpeedScale,
        pose.position,
        anchorPos,
        camRot.globalRotation,
        posHandle
    );

    // Apply any automatic idle behavior. Note that the idle behavior is aborted if there
    // is no input from interaction. So, it assumes that all the other effects from
    // user input results in no change
    applyIdleBehavior(
        deltaTime,
        pose.position,
        camRot.localRotation,
        camRot.globalRotation
    );

    // Horizontal translation by focus node rotation
    pose.position = followAnchorNodeRotation(
        pose.position,
        anchorPos,
        anchorNodeRotationDiff
    );

    // Recalculate posHandle since horizontal position changed
    posHandle = calculateSurfacePositionHandle(*_anchorNode, pose.position);

    // Rotate globally to keep camera rotation fixed
    // in the rotating reference frame of the anchor object
    camRot.globalRotation = rotateGlobally(
        camRot.globalRotation,
        anchorNodeRotationDiff,
        posHandle
    );

    // Rotate around the surface out direction based on user input
    if (!_disableRoll) {
        camRot.globalRotation = rotateHorizontally(
            deltaTime,
            camRot.globalRotation,
            posHandle
        );
    }

    // Perform the vertical movements based on user input
    if (!_disableZoom) {
        pose.position = translateVertically(
            deltaTime,
            pose.position,
            anchorPos,
            posHandle
        );

        pose.position = pushToSurface(
            pose.position,
            anchorPos,
            posHandle
        );
    }

    pose.rotation = composeCameraRotation(camRot);

    _camera->setPose(pose);
}

void OrbitalNavigator::updateCameraScalingFromAnchor(double deltaTime) {
    if (_useAdaptiveStereoscopicDepth) {
        const glm::dvec3 anchorPos = _anchorNode->worldPosition();
        const glm::dvec3 cameraPos = _camera->positionVec3();

        if (glm::length(cameraPos) == 0.0) {
            // Calculating the surface position fails for (0,0,0) vectors
            return;
        }

        const SurfacePositionHandle posHandle = calculateSurfacePositionHandle(
            *_anchorNode,
            cameraPos
        );

        double targetCameraToSurfaceDistance = glm::length(
            cameraToSurfaceVector(cameraPos, anchorPos, posHandle)
        );

        if (_aimNode) {
            targetCameraToSurfaceDistance = std::min(
                targetCameraToSurfaceDistance,
                glm::distance(cameraPos, _aimNode->worldPosition())
            );
        }

        if (_directlySetStereoDistance) {
            _currentCameraToSurfaceDistance = targetCameraToSurfaceDistance;
            _directlySetStereoDistance = false;
        }
        else {
            _currentCameraToSurfaceDistance = interpolateCameraToSurfaceDistance(
                deltaTime,
                _currentCameraToSurfaceDistance,
                targetCameraToSurfaceDistance
            );
        }

        _camera->setScaling(
            _stereoscopicDepthOfFocusSurface /
            static_cast<float>(_currentCameraToSurfaceDistance)
        );
    }
    else {
        _camera->setScaling(glm::pow(10.f, _staticViewScaleExponent));
    }
}

void OrbitalNavigator::updateOnCameraInteraction() {
    // Disable idle behavior if camera interaction happened
    if (_idleBehavior.apply && _idleBehavior.abortOnCameraInteraction) {
        resetIdleBehavior();
    }
}

void OrbitalNavigator::tickMovementTimer(float deltaTime) {
    _movementTimer -= deltaTime;
}

void OrbitalNavigator::tickIdleBehaviorTimer(double deltaTime) {
    if (!_idleBehavior.shouldTriggerWhenIdle) {
        return;
    }
    if (_idleBehaviorTriggerTimer > 0.f) {
        _idleBehaviorTriggerTimer -= static_cast<float>(deltaTime);
    }
    else {
        triggerIdleBehavior();
    }
}

glm::dquat OrbitalNavigator::composeCameraRotation(
                                   const CameraRotationDecomposition& decomposition) const
{
    return decomposition.globalRotation * decomposition.localRotation;
}

Camera* OrbitalNavigator::camera() const {
    return _camera;
}

void OrbitalNavigator::setCamera(Camera* camera) {
    _camera = camera;
}

glm::dvec3 OrbitalNavigator::cameraToSurfaceVector(const glm::dvec3& cameraPos,
                                                   const glm::dvec3& centerPos,
                                                   const SurfacePositionHandle& posHandle)
{
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();
    const glm::dvec3 posDiff = cameraPos - centerPos;
    const glm::dvec3 centerToActualSurfaceModelSpace =
        posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;

    const glm::dvec3 centerToActualSurface =
        glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;

    return centerToActualSurface - posDiff;
}

void OrbitalNavigator::setFocusNode(const SceneGraphNode* focusNode,
                                    bool resetVelocitiesOnChange)
{
    const SceneGraphNode* previousAnchor = _anchorNode;
    setAnchorNode(focusNode, resetVelocitiesOnChange);
    setAimNode(nullptr);

    global::eventEngine->publishEvent<events::EventFocusNodeChanged>(
        previousAnchor,
        focusNode
    );
}

void OrbitalNavigator::setFocusNode(const std::string& focusNode, bool) {
    _anchor = focusNode;
    _aim = std::string("");
}

void OrbitalNavigator::setAnchorNode(const SceneGraphNode* anchorNode,
                                     bool resetVelocitiesOnChange)
{
    if (!_anchorNode) {
        _directlySetStereoDistance = true;
    }

    const bool changedAnchor = _anchorNode != anchorNode;
    _anchorNode = anchorNode;

    // Need to reset velocities after the actual switch in anchor node,
    // since the reset behavior depends on the anchor node.
    if (changedAnchor && resetVelocitiesOnChange) {
        resetVelocities();
    }

    if (changedAnchor) {
        updateOnCameraInteraction(); // Mark a changed anchor node as a camera interaction
        updatePreviousAnchorState();
    }
}

void OrbitalNavigator::clearPreviousState() {
    _previousAnchorNodePosition = std::nullopt;
    _previousAnchorNodeRotation = std::nullopt;
    _previousAimNodePosition = std::nullopt;
}

void OrbitalNavigator::setAimNode(const SceneGraphNode* aimNode) {
    _retargetAimInterpolator.end();
    _aimNode = aimNode;
    updatePreviousAimState();
}

void OrbitalNavigator::setAnchorNode(const std::string& anchorNode) {
    _anchor = anchorNode;
}

void OrbitalNavigator::setAimNode(const std::string& aimNode) {
    _aim = aimNode;
}

void OrbitalNavigator::updatePreviousAnchorState() {
    if (_anchorNode) {
        _previousAnchorNodePosition = _anchorNode->worldPosition();
        _previousAnchorNodeRotation = glm::quat_cast(_anchorNode->worldRotationMatrix());
    }
    else {
        _previousAnchorNodePosition = std::nullopt;
        _previousAnchorNodeRotation = std::nullopt;
    }
}

void OrbitalNavigator::updatePreviousAimState() {
    if (_aimNode) {
        _previousAimNodePosition = _aimNode->worldPosition();
    }
    else {
        _previousAimNodePosition = std::nullopt;
    }
}

void OrbitalNavigator::updatePreviousStateVariables() {
    updatePreviousAnchorState();
    updatePreviousAimState();
}

void OrbitalNavigator::setMinimumAllowedDistance(float distance) {
    if (_limitZoom.enableZoomOutLimit && distance > _limitZoom.maximumAllowedDistance) {
        LWARNING("Setting minimum allowed distance larger than maximum allowed distance");
    }

    _limitZoom.minimumAllowedDistance = distance;
}

void OrbitalNavigator::setMaximumAllowedDistance(float distance) {
    if (distance < 50.f) {
        LWARNING("Setting maximum allowed distance below 50 meters is not allowed");
        return;
    }

    if (_limitZoom.minimumAllowedDistance > distance) {
        LWARNING(
            "Setting maximum allowed distance smaller than minimum allowed distance"
        );
    }

    _limitZoom.maximumAllowedDistance = distance;
}

void OrbitalNavigator::startRetargetAnchor() {
    if (!_anchorNode) {
        return;
    }
    const glm::dvec3 camPos = _camera->positionVec3();
    const glm::dvec3 camDir = _camera->viewDirectionWorldSpace();

    const glm::dvec3 centerPos = _anchorNode->worldPosition();
    const glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    const double angle = glm::angle(camDir, directionToCenter);

    // Minimum is _rotateInterpolationTime seconds. Otherwise proportional to angle.
    _retargetAnchorInterpolator.setInterpolationTime(static_cast<float>(
        glm::max(angle, 1.0) * _retargetInterpolationTime
    ));
    _retargetAnchorInterpolator.start();

    _cameraToSurfaceDistanceInterpolator.setInterpolationTime(_stereoInterpolationTime);
    _cameraToSurfaceDistanceInterpolator.start();
}

void OrbitalNavigator::startRetargetAim() {
    if (!_aimNode) {
        return;
    }

    const glm::dvec3 camPos = _camera->positionVec3();
    const glm::dvec3 camDir = _camera->viewDirectionWorldSpace();
    const glm::dvec3 centerPos = _aimNode->worldPosition();
    const glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    const double angle = glm::angle(camDir, directionToCenter);

    // Minimum is _rotateInterpolationTime seconds. Otherwise proportional to angle.
    _retargetAimInterpolator.setInterpolationTime(static_cast<float>(
        glm::max(angle, 1.0) * _retargetInterpolationTime
    ));
    _retargetAimInterpolator.start();

    _cameraToSurfaceDistanceInterpolator.setInterpolationTime(_stereoInterpolationTime);
    _cameraToSurfaceDistanceInterpolator.start();
}

float OrbitalNavigator::retargetInterpolationTime() const {
    return _retargetInterpolationTime;
}

void OrbitalNavigator::setRetargetInterpolationTime(float durationInSeconds) {
    _retargetInterpolationTime = durationInSeconds;
}

bool OrbitalNavigator::shouldFollowAnchorRotation(const glm::dvec3& cameraPosition) const
{
    if (!_anchorNode || !_followAnchorNodeRotation || glm::length(cameraPosition) == 0.0)
    {
        return false;
    }

    const glm::dmat4 modelTransform = _anchorNode->modelTransform();
    const glm::dmat4 inverseModelTransform = glm::inverse(modelTransform);
    const glm::dvec3 cameraPositionModelSpace = glm::dvec3(inverseModelTransform *
        glm::dvec4(cameraPosition, 1.0));

    const glm::dvec3 centerToReference =
        glm::normalize(cameraPositionModelSpace) * _anchorNode->boundingSphere();

    const double maximumDistanceForRotation = glm::length(
        glm::dmat3(modelTransform) * centerToReference
    ) * _followAnchorNodeRotationDistance;

    const double distanceToCamera =
        glm::distance(cameraPosition, _anchorNode->worldPosition());
    const bool shouldFollow = distanceToCamera < maximumDistanceForRotation;
    return shouldFollow;
}

bool OrbitalNavigator::followingAnchorRotation() const {
    if (_aimNode != nullptr && _aimNode != _anchorNode) {
        return false;
    }
    return _followRotationInterpolator.value() >= 1.0;
}

const SceneGraphNode* OrbitalNavigator::anchorNode() const {
    return _anchorNode;
}

const SceneGraphNode* OrbitalNavigator::aimNode() const {
    return _aimNode;
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

double OrbitalNavigator::minAllowedDistance() const {
    return _limitZoom.minimumAllowedDistance;
}

double OrbitalNavigator::maxAllowedDistance() const {
    return _limitZoom.maximumAllowedDistance;
}

OrbitalNavigator::CameraRotationDecomposition
    OrbitalNavigator::decomposeCameraRotationSurface(const CameraPose& cameraPose,
                                                     const SceneGraphNode& reference)
{
    const glm::dvec3 cameraUp = cameraPose.rotation * Camera::UpDirectionCameraSpace;
    const glm::dvec3 cameraViewDirection = ghoul::viewDirection(cameraPose.rotation);

    const glm::dmat4 modelTransform = reference.modelTransform();
    const glm::dmat4 inverseModelTransform = glm::inverse(modelTransform);
    const glm::dvec3 cameraPositionModelSpace = glm::dvec3(inverseModelTransform *
                                                glm::dvec4(cameraPose.position, 1));

    const SurfacePositionHandle posHandle =
        reference.calculateSurfacePositionHandle(cameraPositionModelSpace);

    const glm::dvec3 directionFromSurfaceToCameraModelSpace =
        posHandle.referenceSurfaceOutDirection;
    const glm::dvec3 directionFromSurfaceToCamera = glm::normalize(
        glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace
    );

    // To avoid problem with lookup in up direction we adjust is with the view direction
    const glm::dquat globalCameraRotation = ghoul::lookAtQuaternion(
        glm::dvec3(0.0),
        -directionFromSurfaceToCamera,
        normalize(cameraViewDirection + cameraUp)
    );

    const glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) *
        cameraPose.rotation;

    return { localCameraRotation, globalCameraRotation };
}

OrbitalNavigator::CameraRotationDecomposition
OrbitalNavigator::decomposeCameraRotation(const CameraPose& cameraPose,
                                          const glm::dvec3& reference)
{
    const glm::dvec3 cameraUp = cameraPose.rotation * glm::dvec3(0.0, 1.0, 0.0);
    const glm::dvec3 cameraViewDirection = ghoul::viewDirection(cameraPose.rotation);

    // To avoid problem with lookup in up direction we adjust is with the view direction
    const glm::dquat globalCameraRotation = ghoul::lookAtQuaternion(
        glm::dvec3(0.0),
        reference - cameraPose.position,
        normalize(cameraViewDirection + cameraUp)
    );

    const glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) *
        cameraPose.rotation;

    return { localCameraRotation, globalCameraRotation };
}

CameraPose OrbitalNavigator::followAim(CameraPose pose, const glm::dvec3& cameraToAnchor,
                                       const Displacement& anchorToAim)
{
    CameraRotationDecomposition anchorDecomp =
        decomposeCameraRotation(pose, pose.position + cameraToAnchor);

    const glm::dvec3 prevCameraToAim = cameraToAnchor + anchorToAim.first;
    const double distanceRatio =
        glm::length(anchorToAim.second) / glm::length(prevCameraToAim);

    // Make sure that the anchor and aim nodes are numerically distinguishable,
    // otherwise, don't follow the aim.
    if (distanceRatio > DistanceRatioAimThreshold) {
        // Divide the action of following the aim into two actions:
        // 1. Rotating camera around anchor, based on the aim's projection onto a sphere
        //    around the anchor, with radius = distance(camera, anchor)
        // 2. Adjustment of the camera to account for radial displacement of the aim

        // Step 1 (Rotation around anchor based on aim's projection)
        const glm::dvec3 newAnchorToProjectedAim =
            glm::length(anchorToAim.first) * glm::normalize(anchorToAim.second);
        const double spinRotationAngle = glm::angle(
            glm::normalize(anchorToAim.first), glm::normalize(newAnchorToProjectedAim)
        );

        if (spinRotationAngle > AngleEpsilon) {
            const glm::dvec3 spinRotationAxis =
                glm::cross(anchorToAim.first, newAnchorToProjectedAim);

            const glm::dquat spinRotation =
                glm::angleAxis(spinRotationAngle, glm::normalize(spinRotationAxis));

            pose.position =
                _anchorNode->worldPosition() - spinRotation * cameraToAnchor;

            anchorDecomp.globalRotation = spinRotation * anchorDecomp.globalRotation;
        }

        // Step 2 (Adjustment for radial displacement)
        const glm::dvec3 projectedAim =
            _anchorNode->worldPosition() + newAnchorToProjectedAim;

        const glm::dvec3 intermediateCameraToAnchor =
            _anchorNode->worldPosition() - pose.position;

        const glm::dvec3 intermediateCameraToProjectedAim =
            projectedAim - pose.position;

        const double anchorAimAngle = glm::angle(
            glm::normalize(intermediateCameraToAnchor),
            glm::normalize(intermediateCameraToProjectedAim)
        );
        double ratio =
            glm::sin(anchorAimAngle) * glm::length(intermediateCameraToAnchor) /
            glm::length(anchorToAim.second);

        // Equation has no solution if ratio > 1.
        // To avoid a discontinuity in the camera behavior,
        // fade out the distance correction influence when ratio approaches 1.
        // CorrectionFactorExponent = 50.0 is picked arbitrarily,
        // and gives a smooth result.
        ratio = glm::clamp(ratio, -1.0, 1.0);
        const double CorrectionFactorExponent = 50.0;
        const double correctionFactor =
            glm::clamp(1.0 - glm::pow(ratio, CorrectionFactorExponent), 0.0, 1.0);

        // newCameraAnchorAngle has two solutions, depending on whether the camera is
        // in the half-space closest to the anchor or aim.
        double newCameraAnchorAngle = glm::asin(ratio);
        if (glm::dot(intermediateCameraToAnchor, anchorToAim.second) <= 0 &&
            glm::dot(intermediateCameraToProjectedAim, anchorToAim.second) <= 0)
        {
            newCameraAnchorAngle = -glm::asin(ratio) + glm::pi<double>();
        }

        const double prevCameraAimAngle = glm::angle(
            glm::normalize(-intermediateCameraToAnchor),
            glm::normalize(newAnchorToProjectedAim)
        );

        const double newCameraAimAngle =
            glm::pi<double>() - anchorAimAngle - newCameraAnchorAngle;

        const double distanceRotationAngle = correctionFactor *
                                       (newCameraAimAngle - prevCameraAimAngle);

        if (glm::abs(distanceRotationAngle) > AngleEpsilon) {
            const glm::dvec3 distanceRotationAxis = glm::normalize(
                glm::cross(intermediateCameraToAnchor, newAnchorToProjectedAim)
            );
            const glm::dquat orbitRotation =
                glm::angleAxis(distanceRotationAngle, distanceRotationAxis);

            pose.position =
                _anchorNode->worldPosition() -
                orbitRotation * intermediateCameraToAnchor;

            const glm::dquat aimAdjustRotation =
                glm::angleAxis(distanceRotationAngle, distanceRotationAxis);

            anchorDecomp.globalRotation = aimAdjustRotation * anchorDecomp.globalRotation;
        }
        // End of step 2.

        pose.rotation = composeCameraRotation(anchorDecomp);
    }

    return pose;
}

glm::dquat OrbitalNavigator::roll(double deltaTime,
                                  const glm::dquat& localCameraRotation) const
{
    const glm::dquat mouseRollQuat = glm::angleAxis(
        _mouseStates.localRollVelocity().x * deltaTime +
        _joystickStates.localRollVelocity().x * deltaTime +
        _websocketStates.localRollVelocity().x * deltaTime +
        _scriptStates.localRollVelocity().x * deltaTime,
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

    const glm::dquat websocketRotationDiff = glm::dquat(glm::dvec3(
        _websocketStates.localRotationVelocity().y,
        _websocketStates.localRotationVelocity().x,
        0.0
    ) * deltaTime);

    const glm::dquat scriptRotationDiff = glm::dquat(glm::dvec3(
        _scriptStates.localRotationVelocity().y,
        _scriptStates.localRotationVelocity().x,
        0.0
    ) * deltaTime);

    return localCameraRotation * joystickRotationDiff * mouseRotationDiff *
           websocketRotationDiff * scriptRotationDiff;
}

glm::dquat OrbitalNavigator::interpolateLocalRotation(double deltaTime,
                                                   const glm::dquat& localCameraRotation)
{
    if (_retargetAnchorInterpolator.isInterpolating()) {
        const double t = _retargetAnchorInterpolator.value();
        _retargetAnchorInterpolator.setDeltaTime(static_cast<float>(deltaTime));
        _retargetAnchorInterpolator.step();

        const glm::dvec3 localUp =
            localCameraRotation * Camera::UpDirectionCameraSpace;

        const glm::dquat targetRotation = ghoul::lookAtQuaternion(
            glm::dvec3(0.0),
            Camera::ViewDirectionCameraSpace,
            normalize(localUp)
        );

        const glm::dquat result = glm::slerp(
            localCameraRotation,
            targetRotation,
            glm::min(t * _retargetAnchorInterpolator.deltaTimeScaled(), 1.0));

        // Retrieving the angle of a quaternion uses acos on the w component, which can
        // have numerical instability for values close to 1.0
        constexpr double Epsilon = 1.0e-13;
        if (std::fabs((std::fabs(result.w) - 1.0)) < Epsilon || angle(result) < 0.01) {
            _retargetAnchorInterpolator.end();
        }
        return result;
    }
    else {
        return localCameraRotation;
    }
}

OrbitalNavigator::Displacement
OrbitalNavigator::interpolateRetargetAim(double deltaTime, const CameraPose& pose,
                                         const glm::dvec3& prevCameraToAnchor,
                                         Displacement anchorToAim)
{
    if (!_retargetAimInterpolator.isInterpolating()) {
        return anchorToAim;
    }

    const double t = _retargetAimInterpolator.value();
    _retargetAimInterpolator.setDeltaTime(static_cast<float>(deltaTime));
    _retargetAimInterpolator.step();

    const glm::dvec3 prevCameraToAim = prevCameraToAnchor + anchorToAim.first;
    const double aimDistance = glm::length(prevCameraToAim);
    const glm::dquat prevRotation = pose.rotation;

    // Introduce a virtual aim - a position straight ahead of the camera,
    // that should be rotated around the camera, until it reaches the aim node.

    const glm::dvec3 prevCameraToVirtualAim =
        aimDistance * (prevRotation * Camera::ViewDirectionCameraSpace);

    // Max angle: the maximum possible angle between anchor and aim, given that
    // the camera orbits the anchor on a fixed distance.
    const double maxAngle =
        glm::atan(glm::length(anchorToAim.first), glm::length(prevCameraToAnchor));

    // Requested angle: The angle between the vector straight ahead from the
    // camera and the vector from camera to anchor should remain constant, in
    // order for the anchor not to move in screen space.
    const double requestedAngle = glm::angle(
        glm::normalize(prevCameraToVirtualAim),
        glm::normalize(prevCameraToAnchor)
    );

    if (requestedAngle <= maxAngle) {
        const glm::dvec3 aimPos = pose.position + prevCameraToAnchor + anchorToAim.second;
        const CameraRotationDecomposition aimDecomp = decomposeCameraRotation(
            pose,
            aimPos
        );

        const glm::dquat interpolatedRotation = glm::slerp(
            prevRotation,
            aimDecomp.globalRotation,
            glm::min(t * _retargetAimInterpolator.deltaTimeScaled(), 1.0)
        );

        const glm::dvec3 recomputedCameraToVirtualAim =
            aimDistance * (interpolatedRotation * Camera::ViewDirectionCameraSpace);

        return {
            prevCameraToVirtualAim - prevCameraToAnchor,
            recomputedCameraToVirtualAim - prevCameraToAnchor
        };
    }
    else {
        // Bail out.
        // Cannot put aim node in center without moving anchor in screen space.
        // Future work: Rotate as much as possible,
        // or possibly use some other DOF to find solution, like moving the camera.
        _retargetAimInterpolator.end();
    }
    return anchorToAim;
}


double OrbitalNavigator::interpolateCameraToSurfaceDistance(double deltaTime,
                                                            double currentDistance,
                                                            double targetDistance
) {
    if (!_cameraToSurfaceDistanceInterpolator.isInterpolating()) {
        return targetDistance;
    }

    const double t = _cameraToSurfaceDistanceInterpolator.value();
    _cameraToSurfaceDistanceInterpolator.setDeltaTime(static_cast<float>(deltaTime));
    _cameraToSurfaceDistanceInterpolator.step();

    // Interpolate distance logarithmically
    const double result = glm::exp(glm::mix(
        glm::log(currentDistance),
        glm::log(targetDistance),
        glm::min(t * _cameraToSurfaceDistanceInterpolator.deltaTimeScaled(), 1.0))
    );

    const double ratio = currentDistance / targetDistance;
    if (glm::abs(ratio - 1.0) < 0.000001) {
        _cameraToSurfaceDistanceInterpolator.end();
    }

    return result;
}

void OrbitalNavigator::rotateAroundAnchorUp(double deltaTime, double speedScale,
                                            glm::dvec3& cameraPosition,
                                            glm::dquat& globalCameraRotation)
{
    const glm::dvec3 axis = [](UpDirectionChoice upAxis) {
        switch (upAxis) {
            case UpDirectionChoice::XAxis: return glm::dvec3(1.0, 0.0, 0.0);
            case UpDirectionChoice::YAxis: return glm::dvec3(0.0, 1.0, 0.0);
            case UpDirectionChoice::ZAxis: return glm::dvec3(0.0, 0.0, 1.0);
            default:                       throw ghoul::MissingCaseException();
        }
    }(UpDirectionChoice(_upToUseForRotation.value()));

    const double combinedXInput = _mouseStates.globalRotationVelocity().x +
        _joystickStates.globalRotationVelocity().x +
        _websocketStates.globalRotationVelocity().x +
        _scriptStates.globalRotationVelocity().x;

    const double angle = combinedXInput * deltaTime * speedScale;
    orbitAroundAxis(axis, angle, cameraPosition, globalCameraRotation);
}

glm::dvec3 OrbitalNavigator::translateHorizontally(double deltaTime, double speedScale,
                                                   const glm::dvec3& cameraPosition,
                                                   const glm::dvec3& objectPosition,
                                                   const glm::dquat& globalCameraRotation,
                                        const SurfacePositionHandle& positionHandle) const
{
    // If we are orbiting around an up vector, we only want to allow verical
    // movement and not use the x velocity
    const bool useX = !_shouldRotateAroundUp;

    const double angleScale = deltaTime * speedScale;

    // Get rotation in camera space
    const glm::dquat mouseRotationDiffCamSpace = glm::dquat(glm::dvec3(
        -_mouseStates.globalRotationVelocity().y,
        useX ? -_mouseStates.globalRotationVelocity().x : 0.0,
        0.0
    ) * angleScale);

    const glm::dquat joystickRotationDiffCamSpace = glm::dquat(glm::dvec3(
        -_joystickStates.globalRotationVelocity().y,
        useX ? -_joystickStates.globalRotationVelocity().x : 0.0,
        0.0
    ) * angleScale);

    const glm::dquat scriptRotationDiffCamSpace = glm::dquat(glm::dvec3(
        -_scriptStates.globalRotationVelocity().y,
        useX ? -_scriptStates.globalRotationVelocity().x : 0.0,
        0.0
    ) * angleScale);

    const glm::dquat websocketRotationDiffCamSpace = glm::dquat(glm::dvec3(
        -_websocketStates.globalRotationVelocity().y,
        useX ? -_websocketStates.globalRotationVelocity().x : 0.0,
        0.0
    ) * angleScale);

    // Transform to world space
    const glm::dquat rotationDiffWorldSpace = globalCameraRotation *
        joystickRotationDiffCamSpace * mouseRotationDiffCamSpace *
        websocketRotationDiffCamSpace * scriptRotationDiffCamSpace *
        glm::inverse(globalCameraRotation);

    const glm::dmat4 modelTransform = _anchorNode->modelTransform();
    const glm::dvec3 outDirection = glm::normalize(
        glm::dmat3(modelTransform) *
        positionHandle.referenceSurfaceOutDirection
    );

    // Compute the vector to rotate to find the new position
    const double distFromCenterToCamera = glm::length(cameraPosition - objectPosition);
    const glm::dvec3 outVector = distFromCenterToCamera * outDirection;

    // Rotate and find the difference vector
    const glm::dvec3 rotationDiffVec3 = outVector * rotationDiffWorldSpace - outVector;

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

    const glm::dvec3 cameraUpWhenFacingSurface = glm::inverse(focusNodeRotationDiff) *
        globalCameraRotation * Camera::UpDirectionCameraSpace;

    return ghoul::lookAtQuaternion(
        glm::dvec3(0.0),
        -directionFromSurfaceToCamera,
        cameraUpWhenFacingSurface
    );
}

glm::dvec3 OrbitalNavigator::translateVertically(double deltaTime,
                                                 const glm::dvec3& cameraPosition,
                                                 const glm::dvec3& objectPosition,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

    const glm::dvec3 posDiff = cameraPosition - objectPosition;

    const glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;

    const glm::dvec3 centerToActualSurface = glm::dmat3(modelTransform) *
                                             centerToActualSurfaceModelSpace;
    const glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;

    const double totalVelocity = _joystickStates.truckMovementVelocity().y +
                                 _mouseStates.truckMovementVelocity().y +
                                 _websocketStates.truckMovementVelocity().y +
                                 _scriptStates.truckMovementVelocity().y;

    return cameraPosition - actualSurfaceToCamera * totalVelocity * deltaTime;
}

glm::dquat OrbitalNavigator::rotateHorizontally(double deltaTime,
                                                const glm::dquat& globalCameraRotation,
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
        _joystickStates.globalRollVelocity().x * deltaTime +
        _websocketStates.globalRollVelocity().x * deltaTime +
        _scriptStates.globalRollVelocity().x * deltaTime,
        directionFromSurfaceToCamera
    );
    return mouseCameraRollRotation * globalCameraRotation;
}

glm::dvec3 OrbitalNavigator::pushToSurface(const glm::dvec3& cameraPosition,
                                           const glm::dvec3& objectPosition,
                                        const SurfacePositionHandle& positionHandle) const
{
    const double minHeight = _limitZoom.enableZoomInLimit ?
        static_cast<double>(_limitZoom.minimumAllowedDistance) : 0.0;

    const double maxHeight = _limitZoom.enableZoomOutLimit ?
        static_cast<double>(_limitZoom.maximumAllowedDistance) : -1.0;

    if (maxHeight > 0.0 && minHeight > maxHeight) {
        LWARNING("Minimum allowed distance is larger than maximum allowed distance");
    }

    const glm::dmat4 modelTransform = _anchorNode->modelTransform();

    const glm::dvec3 posDiff = cameraPosition - objectPosition;
    const glm::dvec3 referenceSurfaceOutDirection = glm::dmat3(modelTransform) *
                                              positionHandle.referenceSurfaceOutDirection;

    const glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;

    const glm::dvec3 centerToActualSurface = glm::dmat3(modelTransform) *
                                             centerToActualSurfaceModelSpace;
    const glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    const double surfaceToCameraSigned = glm::length(actualSurfaceToCamera) *
        glm::sign(dot(actualSurfaceToCamera, referenceSurfaceOutDirection));

    // Adjustment for if the camera is inside the min distance
    double adjustment =
        std::abs(minHeight) > std::numeric_limits<double>::epsilon() ?
        glm::max(minHeight - surfaceToCameraSigned, 0.0) :
        0.0;

    // Adjustment for if the camera is outside the max distance
    // Only apply if the min adjustment not already applied
    if (maxHeight > 0.0 && std::abs(adjustment) < std::numeric_limits<double>::epsilon())
    {
        adjustment = glm::min(maxHeight - surfaceToCameraSigned, 0.0);
    }

    return cameraPosition + referenceSurfaceOutDirection * adjustment;
}

glm::dquat OrbitalNavigator::interpolateRotationDifferential(double deltaTime,
                                                          const glm::dvec3 cameraPosition,
                                                           const glm::dquat& rotationDiff)
{
    // Interpolate with a negative delta time if distance is too large to follow
    const double interpolationSign =
        shouldFollowAnchorRotation(cameraPosition) ? 1.0 : -1.0;

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

JoystickCameraStates& OrbitalNavigator::joystickStates() {
    return _joystickStates;
}

const JoystickCameraStates& OrbitalNavigator::joystickStates() const {
    return _joystickStates;
}

WebsocketCameraStates& OrbitalNavigator::websocketStates() {
    return _websocketStates;
}

const WebsocketCameraStates& OrbitalNavigator::websocketStates() const {
    return _websocketStates;
}

ScriptCameraStates& OrbitalNavigator::scriptStates() {
    return _scriptStates;
}

const ScriptCameraStates& OrbitalNavigator::scriptStates() const {
    return _scriptStates;
}

void OrbitalNavigator::triggerIdleBehavior(std::string_view choice) {
    const OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
    if (mode != OpenSpaceEngine::Mode::UserControl) {
        LERROR(
            "Could not start idle behavior. The camera is being controlled "
            "by some other part of the system"
        );
        return;
    }

    if (choice.empty()) {
        _idleBehavior.chosenBehavior = std::nullopt;
    }
    else {
        IdleBehavior::Behavior behavior = IdleBehavior::Behavior::Orbit;
        if (choice == IdleKeyOrbit) {
            behavior = IdleBehavior::Behavior::Orbit;
        }
        else if (choice == IdleKeyOrbitAtConstantLat) {
            behavior = IdleBehavior::Behavior::OrbitAtConstantLat;
        }
        else if (choice == IdleKeyOrbitAroundUp) {
            behavior = IdleBehavior::Behavior::OrbitAroundUp;
        }
        else {
            throw ghoul::RuntimeError(fmt::format(
                "No existing IdleBehavior with identifier '{}'", choice
            ));
        }
        _idleBehavior.chosenBehavior = behavior;
    }

    _idleBehavior.apply = true;
}

void OrbitalNavigator::resetIdleBehavior() {
    _idleBehavior.apply = false;
    _idleBehavior.chosenBehavior = std::nullopt;
    // Prevent interpolating stop, to avoid weirdness when changing anchor, etc
    _idleBehaviorDampenInterpolator.setInterpolationTime(0.f);
    _idleBehaviorTriggerTimer = _idleBehavior.idleWaitTime;
}

void OrbitalNavigator::applyIdleBehavior(double deltaTime, glm::dvec3& position,
                                         glm::dquat& localRotation,
                                         glm::dquat& globalRotation)
{
    _idleBehaviorDampenInterpolator.setDeltaTime(static_cast<float>(deltaTime));
    _idleBehaviorDampenInterpolator.step();

    if (!(_idleBehavior.apply || _idleBehaviorDampenInterpolator.isInterpolating())) {
        return;
    }

    const SurfacePositionHandle posHandle = calculateSurfacePositionHandle(
        *_anchorNode,
        position
    );

    // Same speed scale as horizontal translation
    double speedScale = rotationSpeedScaleFromCameraHeight(position, posHandle);

    speedScale *= _idleBehavior.speedScaleFactor;
    speedScale *= 0.05; // without this scaling, the motion is way too fast

    if (_idleBehavior.invert) {
        speedScale *= -1.0;
    }

    // Interpolate so that the start and end are smooth
    const double s = _idleBehaviorDampenInterpolator.value();
    speedScale *= _invertIdleBehaviorInterpolation ? (1.0 - s) : s;

    const double angle = deltaTime * speedScale;

    // Apply the chosen behavior
    const IdleBehavior::Behavior choice = _idleBehavior.chosenBehavior.value_or(
        static_cast<IdleBehavior::Behavior>(_idleBehavior.defaultBehavior.value())
    );

    switch (choice) {
        case IdleBehavior::Behavior::Orbit:
            orbitAnchor(angle, position, globalRotation);
            break;
        case IdleBehavior::Behavior::OrbitAtConstantLat: {
            // Assume that "north" coincides with the local z-direction
            // @TODO (2021-07-09, emmbr) Make each scene graph node aware of its own
            // north/up, so that we can query this information rather than assuming it.
            // The we could also combine this idle behavior with the next
            const glm::dvec3 north = glm::dvec3(0.0, 0.0, 1.0);
            orbitAroundAxis(north, angle, position, globalRotation);
            break;
        }
        case IdleBehavior::Behavior::OrbitAroundUp: {
            // Assume that "up" coincides with the local y-direction
            const glm::dvec3 up = glm::dvec3(0.0, 1.0, 0.0);
            orbitAroundAxis(up, angle, position, globalRotation);
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }
}

void OrbitalNavigator::orbitAnchor(double angle, glm::dvec3& position,
                                   glm::dquat& globalRotation)
{
    ghoul_assert(_anchorNode != nullptr, "Node to orbit must be set");

    // Apply a rotation to the right, in camera space
    // (Maybe we should also let the user decide which direction to rotate?
    // Or provide a few different orbit options)
    const glm::dvec3 eulerAngles = glm::dvec3(0.0, -1.0, 0.0) * angle;
    const glm::dquat rotationDiffCameraSpace = glm::dquat(eulerAngles);

    const glm::dquat rotationDiffWorldSpace = globalRotation *
        rotationDiffCameraSpace *
        glm::inverse(globalRotation);

    // Rotate to find the difference in position
    const glm::dvec3 anchorCenterToCamera = position - _anchorNode->worldPosition();
    const glm::dvec3 rotationDiffVec3 =
        anchorCenterToCamera * rotationDiffWorldSpace - anchorCenterToCamera;

    position += rotationDiffVec3;
}

void OrbitalNavigator::orbitAroundAxis(const glm::dvec3& axis, double angle,
                                       glm::dvec3& position, glm::dquat& globalRotation)
{
    ghoul_assert(_anchorNode != nullptr, "Node to orbit must be set");

    if (glm::abs(angle) < AngleEpsilon) {
        return;
    }

    const glm::dmat4 modelTransform = _anchorNode->modelTransform();
    const glm::dvec3 axisInWorldSpace =
        glm::normalize(glm::dmat3(modelTransform) * glm::normalize(axis));

    // Compute rotation to be applied around the axis
    const glm::dquat spinRotation = glm::angleAxis(angle, axisInWorldSpace);

    // Rotate the position vector from the center to camera and update position
    const glm::dvec3 anchorCenterToCamera = position - _anchorNode->worldPosition();
    const glm::dvec3 rotationDiffVec3 =
        spinRotation * anchorCenterToCamera - anchorCenterToCamera;

    if (glm::length(rotationDiffVec3) == 0.0) {
        return;
    }

    position += rotationDiffVec3;

    // Also apply the rotation to the global rotation, so the camera up vector is
    // rotated around the axis as well
    globalRotation = spinRotation * globalRotation;
}

double OrbitalNavigator::rotationSpeedScaleFromCameraHeight(
                                                         const glm::dvec3& cameraPosition,
                                        const SurfacePositionHandle& positionHandle) const
{
    const glm::dmat4 modelTransform = _anchorNode->modelTransform();
    const glm::dvec3 anchorPos = _anchorNode->worldPosition();

    const glm::dvec3 posDiff = cameraPosition - anchorPos;
    const glm::dvec3 centerToActualSurfaceModelSpace =
        positionHandle.centerToReferenceSurface +
        positionHandle.referenceSurfaceOutDirection * positionHandle.heightToSurface;

    const glm::dvec3 centerToActualSurface =
        glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;

    const glm::dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;

    const double distFromSurfaceToCamera = [&]() {
        if (_constantVelocityFlight) {
            const glm::dvec3 centerToRefSurface =
                glm::dmat3(modelTransform) * positionHandle.centerToReferenceSurface;
            const glm::dvec3 refSurfaceToCamera = posDiff - centerToRefSurface;
            return glm::length(refSurfaceToCamera);
        }
        else {
            return glm::length(actualSurfaceToCamera);
        }
    }();

    const double distFromCenterToSurface = glm::length(centerToActualSurface);

    return distFromCenterToSurface > 0.0 ?
        glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0) :
        1.0;
}

scripting::LuaLibrary OrbitalNavigator::luaLibrary() {
    return {
        "orbitalnavigation",
        {
            codegen::lua::SetRelativeMinDistance,
            codegen::lua::SetRelativeMaxDistance
        }
    };
}

} // namespace openspace::interaction
