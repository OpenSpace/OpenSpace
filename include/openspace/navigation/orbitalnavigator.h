/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
#define __OPENSPACE_CORE___ORBITALNAVIGATOR___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/interaction/delayedvariable.h>
#include <openspace/interaction/interpolator.h>
#include <openspace/interaction/joystickcamerastates.h>
#include <openspace/interaction/mousecamerastates.h>
#include <openspace/interaction/scriptcamerastates.h>
#include <openspace/interaction/websocketcamerastates.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <ghoul/glm.h>
#include <glm/gtx/quaternion.hpp>
#include <optional>

namespace openspace {
    class SceneGraphNode;
    class Camera;
    struct CameraPose;
    struct SurfacePositionHandle;
} // namespace

namespace openspace::scripting { struct LuaLibrary; }

namespace openspace::interaction {

class MouseInputState;
class KeyboardInputState;

class OrbitalNavigator : public properties::PropertyOwner {
public:
    struct IdleBehavior : public properties::PropertyOwner {
        enum class Behavior {
            Orbit = 0,
            OrbitAtConstantLat,
            OrbitAroundUp
        };

        IdleBehavior();

        properties::BoolProperty apply;
        properties::BoolProperty shouldTriggerWhenIdle;
        properties::FloatProperty idleWaitTime;
        properties::BoolProperty abortOnCameraInteraction;
        properties::BoolProperty invert;
        properties::FloatProperty speedScaleFactor;
        properties::FloatProperty dampenInterpolationTime;

        properties::OptionProperty defaultBehavior;
        std::optional<Behavior> chosenBehavior;
    };

    OrbitalNavigator();

    void updateStatesFromInput(const MouseInputState& mouseInputState,
        const KeyboardInputState& keyboardInputState, double deltaTime);
    void updateCameraStateFromStates(double deltaTime);
    void updateCameraScalingFromAnchor(double deltaTime);
    void resetVelocities();

    /*
     * This function should be called on every camera interaction: for example when
     * navigating using an input device, changing the focus node or starting a path or
     * a session recording playback
     */
    void updateOnCameraInteraction();

    void tickIdleBehaviorTimer(double deltaTime);
    void triggerIdleBehavior(std::string_view choice = "");

    Camera* camera() const;
    void setCamera(Camera* camera);
    void clearPreviousState();

    void setFocusNode(const SceneGraphNode* focusNode,
        bool resetVelocitiesOnChange = true);
    void setFocusNode(const std::string& focusNode, bool resetVelocitiesOnChange = true);
    void setAnchorNode(const std::string& anchorNode);
    void setAimNode(const std::string& aimNode);

    void startRetargetAnchor();
    void startRetargetAim();
    float retargetInterpolationTime() const;
    void setRetargetInterpolationTime(float durationInSeconds);
    void updatePreviousStateVariables();

    void setMinimumAllowedDistance(float distance);
    void setMaximumAllowedDistance(double distance);

    JoystickCameraStates& joystickStates();
    const JoystickCameraStates& joystickStates() const;

    WebsocketCameraStates& websocketStates();
    const WebsocketCameraStates& websocketStates() const;

    ScriptCameraStates& scriptStates();
    const ScriptCameraStates& scriptStates() const;

    bool shouldFollowAnchorRotation(const glm::dvec3& cameraPosition) const;
    bool followingAnchorRotation() const;
    const SceneGraphNode* anchorNode() const;
    const SceneGraphNode* aimNode() const;

    bool hasRotationalFriction() const;
    bool hasZoomFriction() const;
    bool hasRollFriction() const;

    double minAllowedDistance() const;

    glm::dvec3 anchorNodeToCameraVector() const;
    glm::quat anchorNodeToCameraRotation() const;

    /**
     * Compute a camera position that pushed the camera position to
     * a valid position over the anchor node, accounting for the
     * minimal allowed distance
     */
    glm::dvec3 pushToSurfaceOfAnchor(const glm::dvec3& cameraPosition) const;

    /**
    * \return The Lua library that contains all Lua functions available to affect the
    * orbital navigation
    */
    static scripting::LuaLibrary luaLibrary();

private:
    struct CameraRotationDecomposition {
        glm::dquat localRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
        glm::dquat globalRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
    };

    using Displacement = std::pair<glm::dvec3, glm::dvec3>;

    struct Friction : public properties::PropertyOwner {
        Friction();

        properties::BoolProperty roll;
        properties::BoolProperty rotational;
        properties::BoolProperty zoom;

        properties::FloatProperty friction;
    };

    void setAnchorNode(const SceneGraphNode* anchorNode,
        bool resetVelocitiesOnChange = true);
    void setAimNode(const SceneGraphNode* aimNode);

    void updatePreviousAnchorState();
    void updatePreviousAimState();

    Camera* _camera;

    Friction _friction;

    // Anchor: Node to follow and orbit
    properties::StringProperty _anchor;

    // Aim: Node to look at (when camera direction is reset),
    // Empty string means same as anchor.
    // If these are the same node we call it the `focus` node
    properties::StringProperty _aim;

    // Reset camera direction to the anchor node.
    properties::TriggerProperty _retargetAnchor;
    // Reset camera direction to the aim node.
    properties::TriggerProperty _retargetAim;

    properties::BoolProperty _followAnchorNodeRotation;
    properties::FloatProperty _followAnchorNodeRotationDistance;
    properties::BoolProperty _enableMinimumAllowedDistanceLimit;
    properties::FloatProperty _minimumAllowedDistance;

    struct LimitZoomOut : public properties::PropertyOwner {
        LimitZoomOut();

        properties::BoolProperty isEnabled;
        properties::FloatProperty maximumAllowedDistance;
    };

    LimitZoomOut _limitZoomOut;

    properties::FloatProperty _mouseSensitivity;
    properties::FloatProperty _joystickSensitivity;
    properties::FloatProperty _websocketSensitivity;

    properties::BoolProperty _useAdaptiveStereoscopicDepth;
    properties::FloatProperty _stereoscopicDepthOfFocusSurface;
    properties::FloatProperty _staticViewScaleExponent;

    properties::BoolProperty _constantVelocityFlight;

    properties::FloatProperty _retargetInterpolationTime;
    properties::FloatProperty _stereoInterpolationTime;
    properties::FloatProperty _followRotationInterpolationTime;

    properties::BoolProperty _invertMouseButtons;

    MouseCameraStates _mouseStates;
    JoystickCameraStates _joystickStates;
    WebsocketCameraStates _websocketStates;
    ScriptCameraStates _scriptStates;

    const SceneGraphNode* _anchorNode = nullptr;
    const SceneGraphNode* _aimNode = nullptr;

    std::optional<glm::dvec3>_previousAnchorNodePosition;
    std::optional<glm::dquat> _previousAnchorNodeRotation;
    std::optional<glm::dvec3> _previousAimNodePosition;

    double _currentCameraToSurfaceDistance = 0.0;
    bool _directlySetStereoDistance = false;

    Interpolator<double> _retargetAimInterpolator;
    Interpolator<double> _retargetAnchorInterpolator;
    Interpolator<double> _cameraToSurfaceDistanceInterpolator;
    Interpolator<double> _followRotationInterpolator;
    Interpolator<double> _idleBehaviorDampenInterpolator;
    bool _invertIdleBehaviorInterpolation = false;

    IdleBehavior _idleBehavior;
    float _idleBehaviorTriggerTimer = 0.f;

    /**
     * Decomposes the camera's rotation in to a global and a local rotation defined by
     * CameraRotationDecomposition. The global rotation defines the rotation so that the
     * camera points towards the reference node in the direction opposite to the direction
     * out from the surface of the object. The local rotation defines the differential
     * from the global to the current total rotation so that
     * `cameraRotation = globalRotation * localRotation`.
     */
    CameraRotationDecomposition decomposeCameraRotationSurface(const CameraPose pose,
        const SceneGraphNode& reference);

    /**
     * Decomposes the camera's rotation in to a global and a local rotation defined by
     * CameraRotationDecomposition. The global rotation defines the rotation so that the
     * camera points towards the reference position.
     * The local rotation defines the differential from the global to the current total
     * rotation so that `cameraRotation = globalRotation * localRotation`.
     */
    CameraRotationDecomposition decomposeCameraRotation(const CameraPose pose,
        glm::dvec3 reference);

    /**
     * Composes a pair of global and local rotations into a quaternion that can be used
     * as the world rotation for a camera.
     */
    glm::dquat composeCameraRotation(const CameraRotationDecomposition& composition);

    /*
     * Moves and rotates the camera around the anchor node in order to maintain the
     * screen space position of the aim node. Also interpolates to the aim node, when
     * retargeting the aim.
     */
    CameraPose followAim(CameraPose pose, glm::dvec3 cameraToAnchor,
        Displacement anchorToAim);

    /*
     * Perform a camera roll on the local camera rotation
     * \returns a local camera rotation modified with a roll.
     */
    glm::dquat roll(double deltaTime, const glm::dquat& localCameraRotation) const;

    /**
     * Performs rotation around the cameras x and y axes.
     * \returns a local camera rotation modified with two degrees of freedom.
     */
    glm::dquat rotateLocally(double deltaTime,
        const glm::dquat& localCameraRotation) const;

    /**
     * Interpolates the camera rotation based on active interpolators.
     * \returns a new rotation quaternion
     */
    glm::dquat interpolateLocalRotation(double deltaTime,
        const glm::dquat& localCameraRotation);


    Displacement interpolateRetargetAim(double deltaTime, CameraPose pose,
        glm::dvec3 cameraToAnchor, Displacement anchorToAim);

    double interpolateCameraToSurfaceDistance(double deltaTime, double currentDistance,
        double targetDistance);

    /**
     * Translates the horizontal direction. If far from the anchor object, this will
     * result in an orbital rotation around the object. This function does not affect the
     * rotation but only the position.
     *
     * \return a position vector adjusted in the horizontal direction.
     */
    glm::dvec3 translateHorizontally(double deltaTime, const glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition, const glm::dquat& globalCameraRotation,
        const SurfacePositionHandle& positionHandle) const;

    /*
     * Adds rotation to the camera position so that it follows the rotation of the anchor
     * node defined by the differential anchorNodeRotationDiff.
     *
     * \return a position updated with the rotation defined by anchorNodeRotationDiff
     */
    glm::dvec3 followAnchorNodeRotation(const glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition, const glm::dquat& anchorNodeRotationDiff) const;

    /**
     * Updates the global rotation so that it points towards the anchor node.
     *
     * \return a global rotation quaternion defining a rotation towards the anchor node
     */
    glm::dquat rotateGlobally(const glm::dquat& globalCameraRotation,
        const glm::dquat& aimNodeRotationDiff,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Translates the camera position towards or away from the anchor node.
     * \returns a position vector adjusted in the vertical direction.
     */
    glm::dvec3 translateVertically(double deltaTime, const glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Rotates the camera around the out vector of the surface.
     *
     * \return a quaternion adjusted to rotate around the out vector of the surface
     */
    glm::dquat rotateHorizontally(double deltaTime,
        const glm::dquat& globalCameraRotation,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Push the camera out to the surface of the object.
     *
     * \return a position vector adjusted to be at least _minimumAllowedDistance meters
     *         above the actual surface of the object
     */
    glm::dvec3 pushToSurface(const glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Interpolates between rotationDiff and a 0 rotation.
     */
    glm::dquat interpolateRotationDifferential(double deltaTime,
        const glm::dvec3 cameraPosition, const glm::dquat& rotationDiff);

    /**
     * Get the vector from the camera to the surface of the anchor object in world space.
     */
    glm::dvec3 cameraToSurfaceVector(const glm::dvec3& cameraPos,
        const glm::dvec3& centerPos, const SurfacePositionHandle& posHandle);

    /**
     * Calculates a SurfacePositionHandle given a camera position in world space.
     */
    SurfacePositionHandle calculateSurfacePositionHandle(const SceneGraphNode& node,
        const glm::dvec3& cameraPositionWorldSpace) const;

    void resetIdleBehavior();

    /**
     * Apply the currently selected idle behavior to the position and rotations
     */
    void applyIdleBehavior(double deltaTime, glm::dvec3& position,
        glm::dquat& localRotation, glm::dquat& globalRotation);

    /**
     * Orbit the current anchor node, in a right-bound orbit, by updating the position
     * and global rotation of the camera.
     *
     * Used for IdleBehavior::Behavior::Orbit
     *
     * \param deltaTime The time step to use for the motion. Controls the rotation angle
     * \param position The position of the camera. Will be changed by the function
     * \param globalRotation The camera's global rotation. Will be changed by the function
     * \param speedScale A speed scale that controls the speed of the motion
     */
    void orbitAnchor(double deltaTime, glm::dvec3& position,
        glm::dquat& globalRotation, double speedScale);

    /**
     * Orbit the current anchor node, by adding a rotation around the given axis. For
     * example, when the axis is the north vector, the camera will stay on the current
     * latitude band. Note that this creates a rolling motion if the camera's forward
     * vector coincides with the axis, and should be used with care.
     *
     * Used for:
     * IdleBehavior::Behavior::OrbitAtConstantLat (axis = north = z-axis) and
     * IdleBehavior::Behavior::OrbitAroundUp (axis = up = y-axis)
     *
     * \param axis The axis to arbit around, given in model coordinates of the anchor
     * \param deltaTime The time step to use for the motion. Controls the rotation angle
     * \param position The position of the camera. Will be changed by the function
     * \param globalRotation The camera's global rotation. Will be changed by the function
     * \param speedScale A speed scale that controls the speed of the motion
     */
    void orbitAroundAxis(const glm::dvec3 axis, double deltaTime, glm::dvec3& position,
        glm::dquat& globalRotation, double speedScale);
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
