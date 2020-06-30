/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
    struct SurfacePositionHandle;
} // namespace

namespace openspace::interaction {

class InputState;

class OrbitalNavigator : public properties::PropertyOwner {
public:
    OrbitalNavigator();

    void updateStatesFromInput(const InputState& inputState, double deltaTime);
    void updateCameraStateFromStates(double deltaTime);
    void resetVelocities();

    Camera* camera() const;
    void setCamera(Camera* camera);
    void clearPreviousState();

    SceneGraphNode* focusNode() const;
    void setFocusNode(const SceneGraphNode* focusNode);
    void setFocusNode(const std::string& focusNode);
    void setAnchorNode(const std::string& anchorNode);
    void setAimNode(const std::string& aimNode);

    void startRetargetAnchor();
    void startRetargetAim();
    float retargetInterpolationTime() const;
    void setRetargetInterpolationTime(float durationInSeconds);
    void resetNodeMovements();

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

    glm::dvec3 anchorNodeToCameraVector() const;
    glm::quat anchorNodeToCameraRotation() const;

private:
    struct CameraRotationDecomposition {
        glm::dquat localRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
        glm::dquat globalRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
    };

    struct CameraPose {
        glm::dvec3 position = glm::dvec3(0.0);
        glm::dquat rotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
    };

    using Displacement = std::pair<glm::dvec3, glm::dvec3>;

    struct Friction : public properties::PropertyOwner {
        Friction();

        properties::BoolProperty roll;
        properties::BoolProperty rotational;
        properties::BoolProperty zoom;

        properties::FloatProperty friction;
    };

    void setAnchorNode(const SceneGraphNode* anchorNode);
    void setAimNode(const SceneGraphNode* aimNode);

    Camera* _camera;

    Friction _friction;

    // Anchor: Node to follow and orbit.
    properties::StringProperty _anchor;

    // Aim: Node to look at (when camera direction is reset),
    // Empty string means same as anchor.
    // If these are the same node we call it the `focus` node.
    properties::StringProperty _aim;

    // Reset camera direction to the anchor node.
    properties::TriggerProperty _retargetAnchor;
    // Reset camera direction to the aim node.
    properties::TriggerProperty _retargetAim;

    properties::FloatProperty _followAnchorNodeRotationDistance;
    properties::FloatProperty _minimumAllowedDistance;
    properties::FloatProperty _flightDestinationDistance;
    properties::DoubleProperty _flightDestinationFactor;
    properties::BoolProperty _applyLinearFlight;

    properties::FloatProperty _velocitySensitivity;
    properties::FloatProperty _mouseSensitivity;
    properties::FloatProperty _joystickSensitivity;
    properties::FloatProperty _websocketSensitivity;

    properties::BoolProperty _useAdaptiveStereoscopicDepth;
    properties::FloatProperty _stereoscopicDepthOfFocusSurface;
    properties::FloatProperty _staticViewScaleExponent;

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

    /**
     * Decomposes the camera's rotation in to a global and a local rotation defined by
     * CameraRotationDecomposition. The global rotation defines the rotation so that the
     * camera points towards the reference node in the direction opposite to the direction
     * out from the surface of the object. The local rotation defines the differential
     * from the global to the current total rotation so that
     * <code>cameraRotation = globalRotation * localRotation</code>.
     */
    CameraRotationDecomposition decomposeCameraRotationSurface(const CameraPose pose,
        const SceneGraphNode& reference);

    /**
     * Decomposes the camera's rotation in to a global and a local rotation defined by
     * CameraRotationDecomposition. The global rotation defines the rotation so that the
     * camera points towards the reference node's origin.
     * The local rotation defines the differential from the global to the current total
     * rotation so that <code>cameraRotation = globalRotation * localRotation</code>.
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

    /**
    * Moves the camera along a vector, camPosToCenterPosDiff, until it reaches the
    * focusLimit. The velocity of the zooming depend on distFromCameraToFocus and the
    * final frame where the camera stops moving depends on the distance set in the
    * variable focusLimit. The bool determines whether to move/fly towards the focus node
    * or away from it.
    *
    * \return a new position of the camera, closer to the focusLimit than the previous
    *         position
    */
    glm::dvec3 moveCameraAlongVector(const glm::dvec3& camPos,
        double distFromCameraToFocus, const glm::dvec3& camPosToCenterPosDiff,
        double destination) const;

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
     * \return a position vector adjusted to be at least minHeightAboveGround meters
     *         above the actual surface of the object
     */
    glm::dvec3 pushToSurface(double minHeightAboveGround,
        const glm::dvec3& cameraPosition, const glm::dvec3& objectPosition,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Interpolates between rotationDiff and a 0 rotation.
     */
    glm::dquat interpolateRotationDifferential(double deltaTime, double interpolationTime,
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
        const glm::dvec3 cameraPositionWorldSpace);
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
