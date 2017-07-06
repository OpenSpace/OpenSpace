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

#ifndef __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
#define __OPENSPACE_CORE___ORBITALNAVIGATOR___H__

#include <openspace/interaction/delayedvariable.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/interaction/interpolator.h>

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <glm/glm.hpp>
#include <glm/gtx/quaternion.hpp>

#include <tuple>

namespace openspace {
namespace interaction {

class OrbitalNavigator : public properties::PropertyOwner 
{
public:
	struct MouseState {
		MouseState(double scaleFactor)
			: velocity(scaleFactor, 1)
			, previousPosition(0.0, 0.0) {}
		void setFriction(double friction) {
			velocity.setFriction(friction);
		}
		void setVelocityScaleFactor(double scaleFactor) {
			velocity.setScaleFactor(scaleFactor);
		}
		glm::dvec2 previousPosition;
		DelayedVariable<glm::dvec2, double> velocity;
	};

    class MouseStates
    {
    public:
        /**
        \param sensitivity
        \param velocityScaleFactor can be set to 60 to remove the inertia of the
        interaction. Lower value will make it harder to move the camera. 
        */
        MouseStates(double sensitivity, double velocityScaleFactor);
        void updateMouseStatesFromInput(const InputState& inputState, double deltaTime);
        void setRotationalFriction(double friction);
        void setHorizontalFriction(double friction);
        void setVerticalFriction(double friction);
        void setSensitivity(double sensitivity);
        void setVelocityScaleFactor(double scaleFactor);

        glm::dvec2 synchedGlobalRotationMouseVelocity();
        glm::dvec2 synchedLocalRotationMouseVelocity();
        glm::dvec2 synchedTruckMovementMouseVelocity();
        glm::dvec2 synchedLocalRollMouseVelocity();
        glm::dvec2 synchedGlobalRollMouseVelocity();

    private:
        double _sensitivity;

        MouseState _globalRotationMouseState;
        MouseState _localRotationMouseState;
        MouseState _truckMovementMouseState;
        MouseState _localRollMouseState;
        MouseState _globalRollMouseState;
    };

    OrbitalNavigator();
    ~OrbitalNavigator();

    void updateMouseStatesFromInput(const InputState& inputState, double deltaTime);
    void updateCameraStateFromMouseStates(Camera& camera, double deltaTime);
    bool followingNodeRotation() const;

    void setFocusNode(SceneGraphNode* focusNode);

    SceneGraphNode* focusNode();
    void startInterpolateCameraDirection(const Camera& camera);

protected:
    struct CameraRotationDecomposition {
        glm::dquat localRotation;
        glm::dquat globalRotation;
    };

    // Properties
    properties::BoolProperty _rotationalFriction;
    properties::BoolProperty _horizontalFriction;
    properties::BoolProperty _verticalFriction;
    properties::FloatProperty _followFocusNodeRotationDistance;
    properties::FloatProperty _minimumAllowedDistance;
    properties::FloatProperty _sensitivity;
    properties::FloatProperty _motionLag;

    MouseStates _mouseStates;

    SceneGraphNode* _focusNode = nullptr;
    glm::dvec3 _previousFocusNodePosition;
    glm::dquat _previousFocusNodeRotation;
    
    Interpolator<double> _rotateToFocusNodeInterpolator;
    Interpolator<double> _followRotationInterpolator;

    CameraRotationDecomposition decomposeCameraRotation(
        glm::dvec3 cameraPosition,
        glm::dquat cameraRotation,
        glm::dvec3 cameraLookUp,
        glm::dvec3 cameraViewDirection);

    void performRoll(double deltaTime, glm::dquat& localCameraRotation);
    void performLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
    void interpolateLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
    void performHorizontalTranslationAndRotation(
        double deltaTime,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition,
        glm::dquat& globalCameraRotation);

    void performHorizontalTranslation(
        double deltaTime,
        glm::dvec3 objectPosition,
        glm::dquat& focusNodeRotationDiff,
        glm::dvec3& cameraPosition,
        glm::dquat& globalCameraRotation);

    void followFocusNodeRotation(
        glm::dvec3 objectPosition,
        glm::dquat& focusNodeRotationDiff,
        glm::dvec3& cameraPosition);

    void performGlobalRotation(
        glm::dvec3 objectPosition,
        glm::dquat& focusNodeRotationDiff,
        glm::dvec3& cameraPosition,
        glm::dquat& globalCameraRotation);

    void performVerticalTranslation(
        double deltaTime,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition);

    void performHorizontalRotation(
        double deltaTime,
        glm::dvec3 cameraPosition,
        glm::dquat& globalCameraRotation);
    void pushToSurface(
        double minHeightAboveGround,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition);
    glm::dquat interpolateRotationDifferential(
        double deltaTime,
        double interpolationTime,
        glm::dquat rotationDiff,
        glm::dvec3 objectPosition,
        glm::dvec3 cameraPosition);
};

} // namespace interaction
} // namespace openspace

#endif // __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
