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
#include <openspace/interaction/mousestate.h>

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <glm/glm.hpp>
#include <glm/gtx/quaternion.hpp>

namespace openspace {

class SceneGraphNode;
class Camera;

namespace globebrowsing {
    class RenderableGlobe;
    class TileIndex;
    class Geodetic2;
    class Geodetic3;
}

namespace interaction {

class OrbitalNavigator : public properties::PropertyOwner 
{
public:
    OrbitalNavigator();
    ~OrbitalNavigator();

    void updateMouseStatesFromInput(const InputState& inputState,
                                    double deltaTime);
    void updateCameraStateFromMouseStates(Camera& camera, double deltaTime);
    void setFocusNode(SceneGraphNode* focusNode);
    void startInterpolateCameraDirection(const Camera& camera);

    bool followingNodeRotation() const;    
    SceneGraphNode* focusNode() const;

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    void goToChunk(Camera& camera, globebrowsing::TileIndex ti, glm::vec2 uv,
                   bool resetCameraDirection);
    void goToGeodetic2(Camera& camera, globebrowsing::Geodetic2 geo2,
                   bool resetCameraDirection);
    
    void goToGeodetic3(Camera& camera,  globebrowsing::Geodetic3 geo3);
#endif

private:
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

    /**
     * Decomposes the cameras rotation in to a global and a local rotation defined by
     * CameraRotationDecomposition. The global rotation defines the rotation so that the
     * camera points towards the focus node in the direction opposite to the direction
     * out from the surface of the object. The local rotation defines the differential
     * from the global to the current total rotation so that
     * <code>cameraRotation = globalRotation * localRotation</code>. 
     */
    CameraRotationDecomposition decomposeCameraRotation(
        const glm::dvec3& cameraPosition,
        const glm::dquat& cameraRotation,
        const glm::dvec3& cameraLookUp,
        const glm::dvec3& cameraViewDirection);

    /*
     * Performs a roll on the local rotation of the camera. The local rotation will be
     * modified to roll around the direction the camera is pointing in.
     */
    void performRoll(double deltaTime, glm::dquat& localCameraRotation);

    /**
     * Performs rotation around the cameras x and y axes. The local rotation will be
     * modified with two degrees of freedom.
     */
    void performLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
    
    /**
     * Interpolates the local rotation towards a 0 rotation.
     */
    void interpolateLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
    
    /**
     * Translates the horizontal direction. If far from the focus object, this will
     * result in an orbital rotation around the object. This function does not affect the
     * rotation but only the position.
     */
    void performHorizontalTranslation(
        double deltaTime,
        glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition,
        const glm::dquat& focusNodeRotationDiff,
        const glm::dquat& globalCameraRotation);

    /*
     * Adds rotation to the camera position so that it follows the rotation of the focus
     * node defined by the differential focusNodeRotationDiff. 
     */
    void followFocusNodeRotation(
        glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition,
        const glm::dquat& focusNodeRotationDiff);

    /**
     * Updates the global rotation so that it points towards the focus node.
     */
    void performGlobalRotation(
        glm::dquat& globalCameraRotation,
        const glm::dvec3& objectPosition,
        const glm::dquat& focusNodeRotationDiff,
        const glm::dvec3& cameraPosition);

    /**
     * Translates the camera position towards or away from the focus node.
     */
    void performVerticalTranslation(
        double deltaTime,
        glm::dvec3& cameraPosition,
        const glm::dvec3& objectPosition);

    /**
     * Rotates the camera around the out vector of the surface.
     */
    void performHorizontalRotation(
        double deltaTime,
        glm::dquat& globalCameraRotation,
        const glm::dvec3& cameraPosition);

    /**
     * Push the camera out to the surface of the object.
     */
    void pushToSurface(
        double minHeightAboveGround,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition);

    /**
     * Interpolates between rotationDiff and a 0 rotation.
     */
    glm::dquat interpolateRotationDifferential(
        double deltaTime,
        double interpolationTime,
        const glm::dquat& rotationDiff,
        const glm::dvec3& objectPosition,
        const glm::dvec3& cameraPosition);

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    globebrowsing::RenderableGlobe* castRenderableToGlobe();
    void resetCameraDirection(Camera& camera,  globebrowsing::Geodetic2 geo2);
#endif
};

} // namespace interaction
} // namespace openspace

#endif // __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
