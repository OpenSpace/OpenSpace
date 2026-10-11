/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/navigation/orbitalnavigator/idlemotion.h>

#include <openspace/navigation/orbitalnavigator/orbitalnavigator.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/scenegraphnode.h>

namespace {
    using namespace openspace;

    constexpr std::string_view IdleKeyOrbit = "Orbit";
    constexpr std::string_view IdleKeyOrbitAtConstantLat = "OrbitAtConstantLatitude";
    constexpr std::string_view IdleKeyOrbitAroundUp = "OrbitAroundUpVector";

    constexpr double AngleEpsilon = 1e-7;

    constexpr BoolPropertyInfo ApplyInfo = {
        .identifier = "Apply",
        .name = "Apply idle motion",
        .description = "When set to true, the chosen idle motion will be applied to the "
            "camera, moving the camera accordingly.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr OptionPropertyInfo MotionInfo = {
        .identifier = "Motion",
        .name = "Motion",
        .description = "The chosen camera motion that will be triggered when the idle "
            "motion is applied. Each option represents a predefined camera motion.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr BoolPropertyInfo ShouldTriggerWhenIdleInfo = {
        .identifier = "ShouldTriggerWhenIdle",
        .name = "Should trigger when idle",
        .description = "If true, the chosen idle motion will trigger automatically after "
            "a certain time (see 'IdleWaitTime' property).",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr FloatPropertyInfo IdleWaitTimeInfo = {
        .identifier = "IdleWaitTime",
        .name = "Idle wait time",
        .description = "The time (seconds) until idle motion starts, if no camera "
            "interaction has been performed. Note that friction counts as camera "
            "interaction.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 5.f,
            .minMax = { 0.f, 3600.f },
            .stepSize = 1.f
        }
    };

    constexpr FloatPropertyInfo SpeedFactorInfo = {
        .identifier = "SpeedFactor",
        .name = "Speed factor",
        .description = "A factor that can be used to increase or slow down the speed of "
            "an applied idle motion. A negative value will invert the direction. Note "
            "that a speed of exactly 0 leads to no movement at all.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 1.f,
            .minMax = { -5.f, 5.f }
        }
    };

    constexpr BoolPropertyInfo InvertInfo = {
        .identifier = "Invert",
        .name = "Invert direction",
        .description = "If true, the direction of the idle motion motion will be "
            "inverted compared to the default. For example, the 'Orbit' option rotates "
            "to the right per default, and will rotate to the left when inverted.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo AbortOnCameraInteractionInfo = {
        .identifier = "AbortOnCameraInteraction",
        .name = "Abort on camera interaction",
        .description = "If set to true, the idle motion is aborted on camera "
            "interaction. If false, the motion will be reapplied after the interaction. "
            "Examples of camera interaction are: changing the anchor node, starting a "
            "camera path or session recording playback, or navigating manually using an "
            "input device.",
        .visibility = Visibility::User,
        .defaults = {
            .value = true
        }
    };

    constexpr FloatPropertyInfo DampenInterpolationTimeInfo = {
        .identifier = "DampenInterpolationTime",
        .name = "Start/end dampen interpolation time",
        .description = "The time to interpolate to/from full speed when an idle motion "
            "is triggered or canceled, in seconds.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 0.5f,
            .minMax = { 0.f, 10.f }
        }
    };
} // namespace

namespace openspace {

IdleMotion::IdleMotion()
    : PropertyOwner({
        "IdleMotion",
        "Idle Motion",
        "Triggers a chosen type of automatic camera motion, which is aborted when the "
        "user starts navigating."
    })
    , _apply(ApplyInfo)
    , _shouldTriggerWhenIdle(ShouldTriggerWhenIdleInfo)
    , _idleWaitTime(IdleWaitTimeInfo)
    , _abortOnCameraInteraction(AbortOnCameraInteractionInfo)
    , _invert(InvertInfo)
    , _speedScaleFactor(SpeedFactorInfo)
    , _dampenInterpolationTime(DampenInterpolationTimeInfo)
    , _defaultMotion(MotionInfo)
{
    _apply.onChange([this]() {
        if (_apply) {
            // Reset velocities to ensure that abort on interaction works correctly
            global::navigationHandler->orbitalNavigator().resetVelocities();

            _invertInterpolation = false;
        }
        else {
            _invertInterpolation = true;
            resetTriggerTimer();
        }
        _dampenInterpolator.start();
        _dampenInterpolator.setInterpolationTime(_dampenInterpolationTime);
    });
    addProperty(_apply);

    _defaultMotion.addOptions({
        {
            static_cast<int>(Motion::Orbit),
            std::string(IdleKeyOrbit)
        },
        {
            static_cast<int>(Motion::OrbitAtConstantLatitude),
            std::string(IdleKeyOrbitAtConstantLat)
        },
        {
            static_cast<int>(Motion::OrbitAroundUpVector),
            std::string(IdleKeyOrbitAroundUp)
        }
    });
    _defaultMotion = static_cast<int>(IdleMotion::Motion::Orbit);
    addProperty(_defaultMotion);

    _shouldTriggerWhenIdle.onChange([this]() {
        resetTriggerTimer();
    });
    addProperty(_shouldTriggerWhenIdle);

    _idleWaitTime.onChange([this]() {
        resetTriggerTimer();
    });
    _idleWaitTime.setExponent(2.2f);
    addProperty(_idleWaitTime);

    addProperty(_invert);
    addProperty(_speedScaleFactor);
    addProperty(_abortOnCameraInteraction);

    _dampenInterpolationTime.onChange([this]() {
        _dampenInterpolator.setInterpolationTime(_dampenInterpolationTime);
    });
    addProperty(_dampenInterpolationTime);

    _dampenInterpolator.setTransferFunction(quadraticEaseInOut<double>);
}

void IdleMotion::resetIdleMotionOnCamera() {
    if (_apply && _abortOnCameraInteraction) {
        _apply = false;
        _chosenMotion = std::nullopt;
        // Prevent interpolating stop, to avoid weirdness when changing anchor, etc
        _dampenInterpolator.setInterpolationTime(0.f);
        resetTriggerTimer();
    }
}

void IdleMotion::tickIdleMotionTimer(double deltaTime) {
    if (!_shouldTriggerWhenIdle) {
        return;
    }

    if (_triggerTimer > 0.f) {
        _triggerTimer -= static_cast<float>(deltaTime);
    }
    else {
        // If timer is finished, trigger the default motion
        triggerIdleMotion();
    }
}

void IdleMotion::apply(const SceneGraphNode* anchor, double deltaTime, double speedScale,
                       glm::dvec3& position, glm::dquat& globalRotation)
{
    _dampenInterpolator.setDeltaTime(static_cast<float>(deltaTime));
    _dampenInterpolator.step();

    if (!(_apply || _dampenInterpolator.isInterpolating())) {
        return;
    }

    speedScale *= _speedScaleFactor;
    // Without this scaling, the motion is way too fast
    speedScale *= 0.05;

    if (_invert) {
        speedScale *= -1.0;
    }

    // Interpolate so that the start and end are smooth
    const double s = _dampenInterpolator.value();
    speedScale *= _invertInterpolation ? (1.0 - s) : s;

    const double angle = deltaTime * speedScale;

    // Apply the chosen motion
    const IdleMotion::Motion choice = _chosenMotion.value_or(
        static_cast<IdleMotion::Motion>(_defaultMotion.value())
    );

    switch (choice) {
        case IdleMotion::Motion::Orbit:
            orbitAnchor(anchor, angle, position, globalRotation);
            break;
        case IdleMotion::Motion::OrbitAtConstantLatitude: {
            // Assume that "north" coincides with the local z-direction
            // @TODO (2021-07-09, emmbr) Make each scene graph node aware of its own
            // north/up, so that we can query this information rather than assuming it.
            // The we could also combine this idle motion with the next
            constexpr glm::dvec3 north = glm::dvec3(0.0, 0.0, 1.0);
            orbitAroundAxis(anchor, north, angle, position, globalRotation);
            break;
        }
        case IdleMotion::Motion::OrbitAroundUpVector: {
            // Assume that "up" coincides with the local y-direction
            constexpr glm::dvec3 up = glm::dvec3(0.0, 1.0, 0.0);
            orbitAroundAxis(anchor, up, angle, position, globalRotation);
            break;
        }
        default:
            throw MissingCaseException();
    }
}

void IdleMotion::triggerIdleMotion(std::string_view motionKey) {
    if (motionKey.empty()) {
        // Triggers the default motion
        _chosenMotion = std::nullopt;
    }
    else {
        IdleMotion::Motion motion = IdleMotion::Motion::Orbit;
        if (motionKey == IdleKeyOrbit) {
            motion = IdleMotion::Motion::Orbit;
        }
        else if (motionKey == IdleKeyOrbitAtConstantLat) {
            motion = IdleMotion::Motion::OrbitAtConstantLatitude;
        }
        else if (motionKey == IdleKeyOrbitAroundUp) {
            motion = IdleMotion::Motion::OrbitAroundUpVector;
        }
        else {
            throw RuntimeError(std::format(
                "No existing IdleMotion with identifier '{}'", motionKey
            ));
        }
        _chosenMotion = motion;
    }

    _apply = true;
}

void IdleMotion::orbitAnchor(const SceneGraphNode* anchor, double angle,
                             glm::dvec3& position, glm::dquat& globalRotation)
{
    assert_msg(anchor != nullptr, "Node to orbit must be set");

    // Apply a rotation to the right, in camera space
    const glm::dvec3 eulerAngles = glm::dvec3(0.0, -1.0, 0.0) * angle;
    const glm::dquat rotationDiffCameraSpace = glm::dquat(eulerAngles);

    const glm::dquat rotationDiffWorldSpace = globalRotation *
        rotationDiffCameraSpace *
        glm::inverse(globalRotation);

    // Rotate to find the difference in position
    const glm::dvec3 anchorCenterToCamera = position - anchor->worldPosition();
    const glm::dvec3 rotationDiffVec3 =
        anchorCenterToCamera * rotationDiffWorldSpace - anchorCenterToCamera;

    position += rotationDiffVec3;
}

void IdleMotion::orbitAroundAxis(const SceneGraphNode* anchor, const glm::dvec3& axis,
                                 double angle, glm::dvec3& position,
                                 glm::dquat& globalRotation)
{
    assert_msg(anchor != nullptr, "Node to orbit must be set");

    if (std::abs(angle) < AngleEpsilon) {
        return;
    }

    const glm::dmat4 modelTransform = anchor->modelTransform();
    const glm::dvec3 axisInWorldSpace =
        glm::normalize(glm::dmat3(modelTransform) * glm::normalize(axis));

    // Compute rotation to be applied around the axis
    const glm::dquat spinRotation = glm::angleAxis(angle, axisInWorldSpace);

    // Rotate the position vector from the center to camera and update position
    const glm::dvec3 anchorCenterToCamera = position - anchor->worldPosition();
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

void IdleMotion::resetTriggerTimer() {
    _triggerTimer = _idleWaitTime;
}

} // namespace openspace
