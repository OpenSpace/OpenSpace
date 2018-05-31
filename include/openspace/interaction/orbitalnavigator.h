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

#ifndef __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
#define __OPENSPACE_CORE___ORBITALNAVIGATOR___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/interaction/delayedvariable.h>
#include <openspace/interaction/interpolator.h>
#include <openspace/interaction/joystickcamerastates.h>
#include <openspace/interaction/mousecamerastates.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/glm.h>
#include <glm/gtx/quaternion.hpp>

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
    void updateCameraStateFromStates(Camera& camera, double deltaTime);

    void setFocusNode(SceneGraphNode* focusNode);
    void startInterpolateCameraDirection(const Camera& camera);

    JoystickCameraStates& joystickStates();

    bool followingNodeRotation() const;
    SceneGraphNode* focusNode() const;

    bool hasRotationalFriction() const;
    bool hasZoomFriction() const;
    bool hasRollFriction() const;

private:
    struct CameraRotationDecomposition {
        glm::dquat localRotation;
        glm::dquat globalRotation;
    };

    struct Friction : public properties::PropertyOwner {
        Friction();

        properties::BoolProperty roll;
        properties::BoolProperty rotational;
        properties::BoolProperty zoom;

        properties::FloatProperty friction;
    };

    Friction _friction;

    properties::FloatProperty _followFocusNodeRotationDistance;
    properties::FloatProperty _minimumAllowedDistance;

    properties::FloatProperty _mouseSensitivity;
    properties::FloatProperty _joystickSensitivity;

    properties::BoolProperty _useAdaptiveStereoscopicDepth;
    properties::FloatProperty _stereoscopicDepthOfFocusSurface;
    properties::FloatProperty _staticViewScaleExponent;

    properties::FloatProperty _rotateToFocusInterpolationTime;
    properties::FloatProperty _stereoInterpolationTime;

    MouseCameraStates _mouseStates;
    JoystickCameraStates _joystickStates;

    SceneGraphNode* _focusNode = nullptr;
    glm::dvec3 _previousFocusNodePosition;
    glm::dquat _previousFocusNodeRotation;
    double _currentCameraToSurfaceDistance;
    bool _directlySetStereoDistance = false;

    Interpolator<double> _rotateToFocusNodeInterpolator;
    Interpolator<double> _cameraToSurfaceDistanceInterpolator;
    Interpolator<double> _followRotationInterpolator;

    /**
     * Decomposes the cameras rotation in to a global and a local rotation defined by
     * CameraRotationDecomposition. The global rotation defines the rotation so that the
     * camera points towards the focus node in the direction opposite to the direction
     * out from the surface of the object. The local rotation defines the differential
     * from the global to the current total rotation so that
     * <code>cameraRotation = globalRotation * localRotation</code>.
     */
    CameraRotationDecomposition decomposeCameraRotation(const glm::dvec3& cameraPosition,
        const glm::dquat& cameraRotation, const glm::dvec3& cameraLookUp,
        const glm::dvec3& cameraViewDirection);

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
     * Interpolates the local rotation towards a 0 rotation.
     * \returns a modified local rotation interpolated towards 0.
     */
    glm::dquat interpolateLocalRotation(double deltaTime,
        const glm::dquat& localCameraRotation);


    double interpolateCameraToSurfaceDistance(double deltaTime,
                                              double currentDistance,
                                              double targetDistance);

    /**
     * Translates the horizontal direction. If far from the focus object, this will
     * result in an orbital rotation around the object. This function does not affect the
     * rotation but only the position.
     * \returns a position vector adjusted in the horizontal direction.
     */
    glm::dvec3 translateHorizontally(double deltaTime, const glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition, const glm::dquat& focusNodeRotationDiff,
        const glm::dquat& globalCameraRotation,
        const SurfacePositionHandle& positionHandle) const;

    /*
     * Adds rotation to the camera position so that it follows the rotation of the focus
     * node defined by the differential focusNodeRotationDiff.
     * \returns a position updated with the rotation defined by focusNodeRotationDiff
     */
    glm::dvec3 followFocusNodeRotation(const glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition, const glm::dquat& focusNodeRotationDiff) const;

    /**
     * Updates the global rotation so that it points towards the focus node.
     * \returns a global rotation quaternion defining a rotation towards the focus node.
     */
    glm::dquat rotateGlobally(const glm::dquat& globalCameraRotation,
        const glm::dvec3& objectPosition, const glm::dquat& focusNodeRotationDiff,
        const glm::dvec3& cameraPosition,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Translates the camera position towards or away from the focus node.
     * \returns a position vector adjusted in the vertical direction.
     */
    glm::dvec3 translateVertically(double deltaTime, const glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Rotates the camera around the out vector of the surface.
     * \returns a quaternion adjusted to rotate around the out vector of the surface.
     */
    glm::dquat rotateHorizontally(double deltaTime,
        const glm::dquat& globalCameraRotation, const glm::dvec3& cameraPosition,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Push the camera out to the surface of the object.
     * \returns a position vector adjusted to be at least minHeightAboveGround meters
     * above the actual surface of the object
     */
    glm::dvec3 pushToSurface(double minHeightAboveGround,
        const glm::dvec3& cameraPosition, const glm::dvec3& objectPosition,
        const SurfacePositionHandle& positionHandle) const;

    /**
     * Interpolates between rotationDiff and a 0 rotation.
     */
    glm::dquat interpolateRotationDifferential(double deltaTime,
        double interpolationTime, const glm::dquat& rotationDiff,
        const glm::dvec3& objectPosition, const glm::dvec3& cameraPosition,
        const SurfacePositionHandle& positionHandle);

    /**
     * Get the vector from the camera to the surface of the focus object in world space.
     */
    glm::dvec3 cameraToSurfaceVector(
        const glm::dvec3& cameraPos,
        const glm::dvec3& centerPos,
        const SurfacePositionHandle& posHandle);

    /**
     * Calculates a SurfacePositionHandle given a camera position in world space.
     */
    SurfacePositionHandle calculateSurfacePositionHandle(
        const glm::dvec3 cameraPositionWorldSpace);
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
