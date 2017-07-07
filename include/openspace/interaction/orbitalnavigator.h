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

    CameraRotationDecomposition decomposeCameraRotation(
        glm::dvec3 cameraPosition,
        glm::dquat cameraRotation,
        glm::dvec3 cameraLookUp,
        glm::dvec3 cameraViewDirection);

    void performRoll(double deltaTime, glm::dquat& localCameraRotation);
    void performLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
    void interpolateLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
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

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    globebrowsing::RenderableGlobe* castRenderableToGlobe();
    void resetCameraDirection(Camera& camera,  globebrowsing::Geodetic2 geo2);
#endif
};

} // namespace interaction
} // namespace openspace

#endif // __OPENSPACE_CORE___ORBITALNAVIGATOR___H__
