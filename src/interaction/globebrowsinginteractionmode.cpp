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

#include <openspace/interaction/globebrowsinginteractionmode.h>

#include <openspace/scene/scenegraphnode.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#endif

namespace {
    const std::string _loggerCat = "GlobeBrowsingInteractionMode";
}

namespace openspace {
namespace interaction {

GlobeBrowsingInteractionMode::GlobeBrowsingInteractionMode(std::shared_ptr<MouseStates> mouseStates)
    : OrbitalInteractionMode(mouseStates) 
{
     
}

GlobeBrowsingInteractionMode::~GlobeBrowsingInteractionMode() {

}

void GlobeBrowsingInteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    InteractionMode::setFocusNode(focusNode);

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    Renderable* baseRenderable = _focusNode->renderable();
    if (globebrowsing::RenderableGlobe* globe = dynamic_cast<globebrowsing::RenderableGlobe*>(baseRenderable)) {
        _globe = globe;
    }
    else {
        LWARNING("Focus node is not a renderable globe. GlobeBrowsingInteraction is not possible");
        _globe = nullptr;
    }
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
}

void GlobeBrowsingInteractionMode::updateCameraStateFromMouseStates(Camera& camera, double deltaTime) {
    
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    using namespace glm;
    if (_focusNode && _globe) {
        // Declare variables to use in interaction calculations
        // Shrink interaction ellipsoid to enable interaction below height = 0
        double ellipsoidShrinkTerm = _globe->interactionDepthBelowEllipsoid();
        double minHeightAboveGround = _globe->generalProperties().cameraMinHeight;

        // Read the current state of the camera and focusnode
        dvec3 camPos = camera.positionVec3();
        dvec3 centerPos = _focusNode->worldPosition();

        // Follow focus nodes movement
        dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;

        dquat totalRotation = camera.rotationQuaternion();
        dvec3 lookUp = camera.lookUpVectorWorldSpace();
        dvec3 camDirection = camera.viewDirectionWorldSpace();

        // Sampling of height is done in the reference frame of the globe.
        // Hence, the camera position needs to be transformed with the inverse model matrix
        glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
        glm::dmat4 modelTransform = _globe->modelTransform();
        glm::dvec3 cameraPositionModelSpace =
            glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

        dvec3 posDiff = camPos - centerPos;

        dvec3 directionFromSurfaceToCameraModelSpace =
           _globe->ellipsoid().geodeticSurfaceNormal(
                _globe->ellipsoid().cartesianToGeodetic2(cameraPositionModelSpace));
        dvec3 directionFromSurfaceToCamera =
            normalize(dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);
        dvec3 centerToEllipsoidSurface = dmat3(modelTransform)  * (_globe->projectOnEllipsoid(cameraPositionModelSpace) -
            directionFromSurfaceToCameraModelSpace * ellipsoidShrinkTerm);
        dvec3 ellipsoidSurfaceToCamera = camPos - (centerPos + centerToEllipsoidSurface);

        double heightToSurface =
            _globe->getHeight(cameraPositionModelSpace) + ellipsoidShrinkTerm;
        
        double distFromCenterToSurface =
            length(centerToEllipsoidSurface);
        double distFromEllipsoidSurfaceToCamera = length(ellipsoidSurfaceToCamera);
        double distFromCenterToCamera = length(posDiff);
        double distFromSurfaceToCamera =
            distFromEllipsoidSurfaceToCamera - heightToSurface;

        // Create the internal representation of the local and global camera rotations
        dmat4 lookAtMat = lookAt(
            dvec3(0.0, 0.0, 0.0),
            -directionFromSurfaceToCamera,
            normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
        dquat globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        dquat localCameraRotation = inverse(globalCameraRotation) * totalRotation;

        // Rotate with the globe
        dmat3 globeStateMatrix = _focusNode->worldRotationMatrix();
        dquat globeRotation = quat_cast(globeStateMatrix);
        dquat focusNodeRotationDiff = _previousFocusNodeRotation * inverse(globeRotation);
        _previousFocusNodeRotation = globeRotation;

        { // Do local roll
            glm::dquat cameraRollRotation =
                glm::angleAxis(_mouseStates->synchedLocalRollMouseVelocity().x * deltaTime, dvec3(0, 0, 1));
            localCameraRotation = localCameraRotation * cameraRollRotation;
        }
        if(!_rotateToFocusNodeInterpolator.isInterpolating())
        { // Do local rotation
            glm::dvec3 eulerAngles(_mouseStates->synchedLocalRotationMouseVelocity().y, _mouseStates->synchedLocalRotationMouseVelocity().x, 0);
            glm::dquat rotationDiff = glm::dquat(eulerAngles * deltaTime);

            localCameraRotation = localCameraRotation * rotationDiff;
        }
        else
        { // Interpolate local rotation to focus node
            double t = _rotateToFocusNodeInterpolator.value();
            localCameraRotation = slerp(localCameraRotation, dquat(dvec3(0.0)), t);
            _rotateToFocusNodeInterpolator.step(0.002); // Should probably depend on dt
            // This is a fast but ugly solution to slow regaining of control...
            if (t > 0.2) {
                _rotateToFocusNodeInterpolator.end();
            }
        }
        { // Do global rotation (horizontal movement)
            glm::dvec3 eulerAngles = glm::dvec3(
                -_mouseStates->synchedGlobalRotationMouseVelocity().y * deltaTime,
                -_mouseStates->synchedGlobalRotationMouseVelocity().x * deltaTime,
                0) * glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0);
            glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

            glm::dquat rotationDiffWorldSpace =
                globalCameraRotation *
                rotationDiffCamSpace *
                glm::inverse(globalCameraRotation);

            glm::dvec3 rotationDiffVec3 =
                (distFromCenterToCamera * directionFromSurfaceToCamera)
                 * rotationDiffWorldSpace
                - (distFromCenterToCamera * directionFromSurfaceToCamera);

            camPos += rotationDiffVec3;

            posDiff = camPos - centerPos;
            glm::dvec3 rotationDiffVec3AroundCenter =
                posDiff
                * focusNodeRotationDiff
                - (posDiff);
            camPos += rotationDiffVec3AroundCenter;
            
            
            


            cameraPositionModelSpace =
                glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

            directionFromSurfaceToCameraModelSpace =
                _globe->ellipsoid().geodeticSurfaceNormal(
                    _globe->ellipsoid().cartesianToGeodetic2(cameraPositionModelSpace));
            directionFromSurfaceToCamera =
              normalize(dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);
            centerToEllipsoidSurface = dmat3(modelTransform) * (_globe->projectOnEllipsoid(cameraPositionModelSpace) -
                directionFromSurfaceToCameraModelSpace * ellipsoidShrinkTerm);
            ellipsoidSurfaceToCamera = camPos - (centerPos + centerToEllipsoidSurface);


            glm::dvec3 lookUpWhenFacingSurface =
                inverse(focusNodeRotationDiff) * globalCameraRotation * glm::dvec3(camera.lookUpVectorCameraSpace());
            glm::dmat4 lookAtMat = glm::lookAt(
                glm::dvec3(0, 0, 0),
                -directionFromSurfaceToCamera,
                lookUpWhenFacingSurface);
            globalCameraRotation =
                glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            camPos += -directionFromSurfaceToCamera * distFromSurfaceToCamera *
                _mouseStates->synchedTruckMovementMouseVelocity().y * deltaTime;
        }
        { // Roll around ellipsoid normal
            glm::dquat cameraRollRotation =
                glm::angleAxis(_mouseStates->synchedGlobalRollMouseVelocity().x * deltaTime, directionFromSurfaceToCamera);
            globalCameraRotation = cameraRollRotation * globalCameraRotation;
        }
        { // Push up to surface
            ellipsoidSurfaceToCamera = camPos - (centerPos + centerToEllipsoidSurface);

            // Sampling of height is done in the reference frame of the globe.
            // Hence, the camera position needs to be transformed with the inverse model matrix
            glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
            glm::dvec3 cameraPositionModelSpace =
                glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

            distFromEllipsoidSurfaceToCamera = glm::length(ellipsoidSurfaceToCamera);
            double heightToSurface =
                _globe->getHeight(cameraPositionModelSpace) + ellipsoidShrinkTerm;
            double heightToSurfaceAndPadding = heightToSurface + minHeightAboveGround;
            camPos += directionFromSurfaceToCamera *
                glm::max(heightToSurfaceAndPadding - distFromEllipsoidSurfaceToCamera, 0.0);
        }
        
        // Update the camera state
        camera.setPositionVec3(camPos); 
        camera.setRotation(globalCameraRotation * localCameraRotation);
    }
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
}

bool GlobeBrowsingInteractionMode::followingNodeRotation() const {
    return true;
}

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
void GlobeBrowsingInteractionMode::goToChunk(Camera& camera,
    globebrowsing::TileIndex ti, glm::vec2 uv, bool resetCameraDirection) {
    using namespace globebrowsing;
    
    // Camera position in model space
    glm::dvec3 camPos = camera.positionVec3();
    glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
    glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));
    
    GeodeticPatch patch(ti);
    Geodetic2 corner = patch.getCorner(SOUTH_WEST);
    Geodetic2 positionOnPatch = patch.getSize();
    positionOnPatch.lat *= uv.y;
    positionOnPatch.lon *= uv.x;
    Geodetic2 pointPosition = corner + positionOnPatch;
    
    glm::dvec3 positionOnEllipsoid =
        _globe->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);
    
    goToGeodetic3(camera, {pointPosition, altitude});
    
    if (resetCameraDirection) {
        this->resetCameraDirection(camera, pointPosition);
    }
}

void GlobeBrowsingInteractionMode::goToGeodetic2(Camera& camera,
    globebrowsing::Geodetic2 geo2, bool resetCameraDirection) {
    using namespace globebrowsing;
    
    // Camera position in model space
    glm::dvec3 camPos = camera.positionVec3();
    glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
    glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));
        
    glm::dvec3 positionOnEllipsoid =
    _globe->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);
        
    goToGeodetic3(camera, {geo2, altitude});
        
    if (resetCameraDirection) {
        this->resetCameraDirection(camera, geo2);
    }
}
    
void GlobeBrowsingInteractionMode::goToGeodetic3(Camera& camera, globebrowsing::Geodetic3 geo3) {
    glm::dvec3 positionModelSpace = _globe->ellipsoid().cartesianPosition(geo3);
    glm::dmat4 modelTransform = _globe->modelTransform();
    glm::dvec3 positionWorldSpace = modelTransform * glm::dvec4(positionModelSpace, 1.0);
    camera.setPositionVec3(positionWorldSpace);
}
    
void GlobeBrowsingInteractionMode::resetCameraDirection(Camera& camera,  globebrowsing::Geodetic2 geo2) {
    using namespace globebrowsing;
    
    // Camera is described in world space
    glm::dmat4 modelTransform = _globe->modelTransform();
    
    // Lookup vector
    glm::dvec3 positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geo2);
    glm::dvec3 slightlyNorth = _globe->ellipsoid().cartesianSurfacePosition(
        Geodetic2(geo2.lat + 0.001, geo2.lon));
    glm::dvec3 lookUpModelSpace = glm::normalize(slightlyNorth - positionModelSpace);
    glm::dvec3 lookUpWorldSpace = glm::dmat3(modelTransform) * lookUpModelSpace;
    
    // Lookat vector
    glm::dvec3 lookAtWorldSpace = modelTransform * glm::dvec4(positionModelSpace, 1.0);

    // Eye position
    glm::dvec3 eye = camera.positionVec3();
    
    // Matrix
    glm::dmat4 lookAtMatrix = glm::lookAt(
                    eye, lookAtWorldSpace, lookUpWorldSpace);
    
    // Set rotation
    glm::dquat rotation = glm::quat_cast(inverse(lookAtMatrix));
    camera.setRotation(rotation);
}

#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED

} // namespace interaction
} // namespace openspace
