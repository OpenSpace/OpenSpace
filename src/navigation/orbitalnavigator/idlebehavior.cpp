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

#include <openspace/navigation/orbitalnavigator/idlebehavior.h>

#include <openspace/navigation/orbitalnavigator/orbitalnavigator.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>

namespace {
    //constexpr std::string_view _loggerCat = "IdleBehavior";

    constexpr std::string_view IdleKeyOrbit = "Orbit";
    constexpr std::string_view IdleKeyOrbitAtConstantLat = "OrbitAtConstantLatitude";
    constexpr std::string_view IdleKeyOrbitAroundUp = "OrbitAroundUp";

    constexpr double AngleEpsilon = 1e-7;

    constexpr openspace::properties::Property::PropertyInfo ApplyIdleBehaviorInfo = {
        "ApplyIdleBehavior",
        "Apply idle behavior",
        "When set to true, the chosen idle behavior will be applied to the camera, "
        "moving the camera accordingly.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo IdleBehaviorInfo = {
        "IdleBehavior",
        "Idle behavior",
        "The chosen camera behavior that will be triggered when the idle behavior is "
        "applied. Each option represents a predefined camera behavior.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        ShouldTriggerIdleBehaviorWhenIdleInfo =
    {
        "ShouldTriggerWhenIdle",
        "Should trigger when idle",
        "If true, the chosen idle behavior will trigger automatically after a certain "
        "time (see 'IdleWaitTime' property).",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo IdleWaitTimeInfo = {
        "IdleWaitTime",
        "Idle wait time",
        "The time (seconds) until idle behavior starts, if no camera interaction "
        "has been performed. Note that friction counts as camera interaction.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IdleBehaviorSpeedInfo = {
        "SpeedFactor",
        "Speed factor",
        "A factor that can be used to increase or slow down the speed of an applied "
        "idle behavior. A negative value will invert the direction. Note that a speed "
        "of exactly 0 leads to no movement at all.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo InvertIdleBehaviorInfo = {
        "Invert",
        "Invert",
        "If true, the direction of the idle behavior motion will be inverted compared "
        "to the default. For example, the 'Orbit' option rotates to the right per "
        "default, and will rotate to the left when inverted.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AbortOnCameraInteractionInfo =
    {
        "AbortOnCameraInteraction",
        "Abort on camera interaction",
        "If set to true, the idle behavior is aborted on camera interaction. If false, "
        "the behavior will be reapplied after the interaction. Examples of camera "
        "interaction are: changing the anchor node, starting a camera path or session "
        "recording playback, or navigating manually using an input device.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo
        IdleBehaviorDampenInterpolationTimeInfo =
    {
        "DampenInterpolationTime",
        "Start/end dampen interpolation time",
        "The time to interpolate to/from full speed when an idle behavior is triggered "
        "or canceled, in seconds.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace::interaction {

IdleBehavior::IdleBehavior()
    : properties::PropertyOwner({
        "IdleBehavior",
        "Idle Behavior",
        "Triggers a chosen type of automatic camera motion, which is aborted when the "
        "user starts navigating."
    })
    , _apply(ApplyIdleBehaviorInfo, false)
    , _shouldTriggerWhenIdle(ShouldTriggerIdleBehaviorWhenIdleInfo, false)
    , _idleWaitTime(IdleWaitTimeInfo, 5.f, 0.f, 3600.f, 1.f)
    , _abortOnCameraInteraction(AbortOnCameraInteractionInfo, true)
    , _invert(InvertIdleBehaviorInfo, false)
    , _speedScaleFactor(IdleBehaviorSpeedInfo, 1.f, -5.f, 5.f)
    , _dampenInterpolationTime(IdleBehaviorDampenInterpolationTimeInfo, 0.5f, 0.f, 10.f)
    , _defaultBehavior(IdleBehaviorInfo)
{
    addProperty(_apply);
    _defaultBehavior.addOptions({
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
    _defaultBehavior = static_cast<int>(IdleBehavior::Behavior::Orbit);
    addProperty(_defaultBehavior);
    addProperty(_shouldTriggerWhenIdle);
    addProperty(_idleWaitTime);
    _idleWaitTime.setExponent(2.2f);
    addProperty(_invert);
    addProperty(_speedScaleFactor);
    addProperty(_abortOnCameraInteraction);
    addProperty(_dampenInterpolationTime);

    _shouldTriggerWhenIdle.onChange([this]() {
        _triggerTimer = _idleWaitTime;
    });
    _idleWaitTime.onChange([this]() {
        _triggerTimer = _idleWaitTime;
    });

    _dampenInterpolator.setTransferFunction(
        ghoul::quadraticEaseInOut<double>
    );

    _dampenInterpolationTime.onChange([this]() {
        _dampenInterpolator.setInterpolationTime(
            _dampenInterpolationTime
        );
    });

    _apply.onChange([this]() {
        if (_apply) {
            // Reset velocities to ensure that abort on interaction works correctly
            global::navigationHandler->orbitalNavigator().resetVelocities(); // TODO: Remove this dependency on orbitalnavigator
            _invertInterpolation = false;
        }
        else {
            _invertInterpolation = true;
        }
        _dampenInterpolator.start();
        _dampenInterpolator.setInterpolationTime(
            _dampenInterpolationTime
        );
    });

    _shouldTriggerWhenIdle.onChange([this]() {
        _triggerTimer = _idleWaitTime;
    });

    _idleWaitTime.onChange([this]() {
        _triggerTimer = _idleWaitTime;
    });
}

void IdleBehavior::resetIdleBehaviorOnCamera() {
    if (_apply && _abortOnCameraInteraction) {
        _apply = false;
        _chosenBehavior = std::nullopt;
        // Prevent interpolating stop, to avoid weirdness when changing anchor, etc
        _dampenInterpolator.setInterpolationTime(0.f);
        _triggerTimer = _idleWaitTime;
    }
}

void IdleBehavior::tickIdleBehaviorTimer(double deltaTime) {
    if (!_shouldTriggerWhenIdle) {
        return;
    }

    if (_triggerTimer > 0.f) {
        _triggerTimer -= static_cast<float>(deltaTime);
    }
    else {
        // If timer is finished, trigger the default behavior
        triggerIdleBehavior();
    }
}

void IdleBehavior::applyIdleBehavior(const SceneGraphNode* anchor, double deltaTime,
                                     double speedScale, glm::dvec3& position,
                                     glm::dquat& globalRotation)
{
    _dampenInterpolator.setDeltaTime(static_cast<float>(deltaTime));
    _dampenInterpolator.step();

    if (!(_apply || _dampenInterpolator.isInterpolating())) {
        return;
    }

    speedScale *= _speedScaleFactor;
    speedScale *= 0.05; // without this scaling, the motion is way too fast

    if (_invert) {
        speedScale *= -1.0;
    }

    // Interpolate so that the start and end are smooth
    const double s = _dampenInterpolator.value();
    speedScale *= _invertInterpolation ? (1.0 - s) : s;

    const double angle = deltaTime * speedScale;

    // Apply the chosen behavior
    const IdleBehavior::Behavior choice = _chosenBehavior.value_or(
        static_cast<IdleBehavior::Behavior>(_defaultBehavior.value())
    );

    switch (choice) {
    case IdleBehavior::Behavior::Orbit:
        orbitAnchor(anchor, angle, position, globalRotation);
        break;
    case IdleBehavior::Behavior::OrbitAtConstantLat: {
        // Assume that "north" coincides with the local z-direction
        // @TODO (2021-07-09, emmbr) Make each scene graph node aware of its own
        // north/up, so that we can query this information rather than assuming it.
        // The we could also combine this idle behavior with the next
        const glm::dvec3 north = glm::dvec3(0.0, 0.0, 1.0);
        orbitAroundAxis(anchor, north, angle, position, globalRotation);
        break;
    }
    case IdleBehavior::Behavior::OrbitAroundUp: {
        // Assume that "up" coincides with the local y-direction
        const glm::dvec3 up = glm::dvec3(0.0, 1.0, 0.0);
        orbitAroundAxis(anchor, up, angle, position, globalRotation);
        break;
    }
    default:
        throw ghoul::MissingCaseException();
    }
}

void IdleBehavior::triggerIdleBehavior(std::string_view choice) {
    if (choice.empty()) {
        // Triggers the default behavior
        _chosenBehavior = std::nullopt;
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
            throw ghoul::RuntimeError(std::format(
                "No existing IdleBehavior with identifier '{}'", choice
            ));
        }
        _chosenBehavior = behavior;
    }

    _apply = true;
}

void IdleBehavior::orbitAnchor(const SceneGraphNode* anchor, double angle,
    glm::dvec3& position, glm::dquat& globalRotation)
{
    ghoul_assert(anchor != nullptr, "Node to orbit must be set");

    // Apply a rotation to the right, in camera space
    // (Maybe we should also let the user decide which direction to rotate?
    // Or provide a few different orbit options)
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

void IdleBehavior::orbitAroundAxis(const SceneGraphNode* anchor, const glm::dvec3& axis,
    double angle, glm::dvec3& position, glm::dquat& globalRotation)
{
    ghoul_assert(anchor != nullptr, "Node to orbit must be set");

    if (glm::abs(angle) < AngleEpsilon) {
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

} // namespace openspace::interaction
