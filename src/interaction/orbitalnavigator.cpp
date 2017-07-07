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

#include <openspace/interaction/orbitalnavigator.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/vector_angle.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/tile/tileindex.h>
#endif

namespace {
    const std::string _loggerCat = "OrbitalNavigator";
}

namespace openspace {
namespace interaction {

OrbitalNavigator::OrbitalNavigator()
    : properties::PropertyOwner("orbitalNavigator")
    , _rotationalFriction("rotationalFriction", "Rotational friction", true)
    , _horizontalFriction("horizontalFriction", "Horizontal friction", true)
    , _verticalFriction("verticalFriction", "Vertical friction", true)
    , _followFocusNodeRotationDistance("followFocusNodeRotationDistance",
        "Follow focus node rotation distance", 2.0f, 0.0f, 10.f)
    , _minimumAllowedDistance("minimumAllowedDistance",
        "Minimum allowed distance", 10.0f, 0.0f, 10000.f)
    , _sensitivity("sensitivity", "Sensitivity", 20.0f, 1.0f, 50.f)
    , _motionLag("motionLag", "Motion lag", 0.5f, 0.f, 1.f)
    , _mouseStates(_sensitivity * pow(10.0,-4), 1 / (_motionLag + 0.0000001))
{
    auto smoothStep = 
        [](double t) {
            double res = 3.0 * t*t  - 2.0 * t*t*t;
            return glm::clamp(res, 0.0, 1.0);
        };
    _followRotationInterpolator.setTransferFunction(smoothStep);

    // The transfer function is used here to get a different interpolation than the one
    // obtained from newValue = lerp(0, currentValue, dt). That one will result in an
    // exponentially decreasing value but we want to be able to control it. Either as
    // a linear interpolation or a smooth step interpolation. Therefore we use
    // newValue = lerp(0, currentValue * f(t) * dt) where f(t) is the transfer function
    // and lerp is a linear iterpolation
    // lerp(endValue, startValue, interpolationParameter).
    //
    // The transfer functions are derived from:
    // f(t) = d/dt ( ln(1 / f_orig(t)) ) where f_orig is the transfer function that would
    // be used if the interpolation was sinply linear between a start value and an end
    // value instead of current value and end value (0) as we use it when inerpoláting.
    // As an example f_orig(t) = 1 - t yields f(t) = 1 / (1 - t) which results in a linear
    // interpolation from 1 to 0.

    auto smoothStepDerivedTranferFunction = 
        [](double t) {
            return (6 * (t + t*t) / (1 - 3 * t*t + 2 * t*t*t));
        };
    auto linearDerivedTranferFunction = 
        [](double t) {
            return 1 / (1 - t);
        };
    _rotateToFocusNodeInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);

    // Define lambda functions for changed properties
    _rotationalFriction.onChange([&]() {
        _mouseStates.setRotationalFriction(_rotationalFriction);
    });
    _horizontalFriction.onChange([&]() {
        _mouseStates.setHorizontalFriction(_horizontalFriction);
    });
    _verticalFriction.onChange([&]() {
        _mouseStates.setVerticalFriction(_verticalFriction);
    });
    _sensitivity.onChange([&]() {
        _mouseStates.setSensitivity(_sensitivity * pow(10.0,-4));
    });
    _motionLag.onChange([&]() {
        _mouseStates.setVelocityScaleFactor(1 / (_motionLag + 0.0000001));
    });

    // Add the properties
    addProperty(_rotationalFriction);
    addProperty(_horizontalFriction);
    addProperty(_verticalFriction);
    addProperty(_followFocusNodeRotationDistance);
    addProperty(_minimumAllowedDistance);
    addProperty(_sensitivity);
    addProperty(_motionLag);
}

OrbitalNavigator::~OrbitalNavigator() {

}

void OrbitalNavigator::updateMouseStatesFromInput(const InputState& inputState,
                                                  double deltaTime)
{
    _mouseStates.updateMouseStatesFromInput(inputState, deltaTime);
}

void OrbitalNavigator::updateCameraStateFromMouseStates(Camera& camera,
                                                        double deltaTime)
{
    if (_focusNode) {
        using namespace glm;
        // Read the current state of the camera and focusnode
        dvec3 camPos = camera.positionVec3();
        dvec3 centerPos = _focusNode->worldPosition();

        // Follow focus nodes movement
        dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;

        CameraRotationDecomposition camRot = decomposeCameraRotation(
            camPos,
            camera.rotationQuaternion(),
            camera.lookUpVectorWorldSpace(),
            camera.viewDirectionWorldSpace());

        // Rotate with the globe
        dmat3 globeStateMatrix = _focusNode->worldRotationMatrix();
        dquat globeRotation = quat_cast(globeStateMatrix);
        dquat focusNodeRotationDiff = _previousFocusNodeRotation * inverse(globeRotation);
        _previousFocusNodeRotation = globeRotation;

        focusNodeRotationDiff = interpolateRotationDifferential(
            deltaTime,
            1.0,
            focusNodeRotationDiff,
            centerPos,
            camPos);

        performRoll(deltaTime, camRot.localRotation);
        if (_rotateToFocusNodeInterpolator.isInterpolating()) {
            interpolateLocalRotation(deltaTime, camRot.localRotation);
        }
        else {
            performLocalRotation(deltaTime, camRot.localRotation);
        }

        performHorizontalTranslation(
            deltaTime,
            centerPos,
            focusNodeRotationDiff,
            camPos,
            camRot.globalRotation);

        followFocusNodeRotation(
            centerPos,
            focusNodeRotationDiff,
            camPos);

        performGlobalRotation(
            centerPos,
            focusNodeRotationDiff,
            camPos,
            camRot.globalRotation);

        performVerticalTranslation(deltaTime, centerPos, camPos);
        performHorizontalRotation(deltaTime, camPos, camRot.globalRotation);
        pushToSurface(_minimumAllowedDistance, centerPos, camPos);

        // Update the camera state
        camera.setPositionVec3(camPos); 
        camera.setRotation(camRot.globalRotation * camRot.localRotation);
        return;
    }
}

void OrbitalNavigator::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;

    if (_focusNode != nullptr) {
        _previousFocusNodePosition = _focusNode->worldPosition();
        _previousFocusNodeRotation = glm::quat_cast(_focusNode->worldRotationMatrix());
    }
}

void OrbitalNavigator::startInterpolateCameraDirection(const Camera& camera) {
    glm::dvec3 camPos = camera.positionVec3();
    glm::dvec3 camDir = glm::normalize(camera.rotationQuaternion() * glm::dvec3(0, 0, -1));
    glm::dvec3 centerPos = _focusNode->worldPosition();
    glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    double angle = glm::angle(camDir, directionToCenter);

    // Minimum is two second. Otherwise proportional to angle
    _rotateToFocusNodeInterpolator.setInterpolationTime(glm::max(angle * 2.0, 2.0));
    _rotateToFocusNodeInterpolator.start();
}

bool OrbitalNavigator::followingNodeRotation() const {
    return _followRotationInterpolator.value() >= 1.0;
}

SceneGraphNode* OrbitalNavigator::focusNode() const {
    return _focusNode;
}

OrbitalNavigator::CameraRotationDecomposition
	OrbitalNavigator::decomposeCameraRotation(
		glm::dvec3 cameraPosition,
		glm::dquat cameraRotation,
		glm::dvec3 cameraLookUp,
		glm::dvec3 cameraViewDirection)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();
    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));

    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

	glm::dvec3 directionFromSurfaceToCameraModelSpace = posHandle.referenceSurfaceOutDirection;
	glm::dvec3 directionFromSurfaceToCamera =
		glm::normalize(glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);

    // Create the internal representation of the local and global camera rotations
	glm::dmat4 lookAtMat = glm::lookAt(
		glm::dvec3(0.0, 0.0, 0.0),
        -directionFromSurfaceToCamera,
        normalize(cameraViewDirection + cameraLookUp)); // To avoid problem with lookup in up direction
	glm::dquat globalCameraRotation = glm::normalize(glm::quat_cast(inverse(lookAtMat)));
	glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) * cameraRotation;

    return { localCameraRotation, globalCameraRotation };
}

void OrbitalNavigator::performRoll(double deltaTime, glm::dquat& localCameraRotation) {
    glm::dquat rollQuat = glm::angleAxis(
        _mouseStates.synchedLocalRollMouseVelocity().x * deltaTime,
        glm::dvec3(0, 0, 1)
    );
    localCameraRotation = localCameraRotation * rollQuat;
}

void OrbitalNavigator::performLocalRotation(double deltaTime, glm::dquat& localCameraRotation) {
    glm::dvec3 eulerAngles(
        _mouseStates.synchedLocalRotationMouseVelocity().y,
        _mouseStates.synchedLocalRotationMouseVelocity().x,
        0
    );
    glm::dquat rotationDiff = glm::dquat(eulerAngles * deltaTime);
	localCameraRotation = localCameraRotation * rotationDiff;
}

void OrbitalNavigator::interpolateLocalRotation(double deltaTime, glm::dquat& localCameraRotation) {
    double t = _rotateToFocusNodeInterpolator.value();
    _rotateToFocusNodeInterpolator.setDeltaTime(deltaTime);
    _rotateToFocusNodeInterpolator.step();
	localCameraRotation = glm::slerp(
        localCameraRotation,
        glm::dquat(glm::dvec3(0.0)),
        glm::min(t * _rotateToFocusNodeInterpolator.deltaTimeScaled(), 1.0));

    if (angle(localCameraRotation) < 0.01) {
        _rotateToFocusNodeInterpolator.end();
    }
}

void OrbitalNavigator::performHorizontalTranslation(
    double deltaTime,
    glm::dvec3 objectPosition,
    glm::dquat& focusNodeRotationDiff,
    glm::dvec3& cameraPosition,
    glm::dquat& globalCameraRotation)
{
	using namespace glm;
    // Uniform variables
    double ellipsoidShrinkTerm = 0;
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    // Get position handle
    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    dvec3 surfaceNormal =
        normalize(dmat3(modelTransform) * posHandle.referenceSurfaceOutDirection);

    dvec3 posDiff = cameraPosition - objectPosition;

    dvec3 centerToReferenceSurface = dmat3(modelTransform) * posHandle.centerToReferenceSurface;
    dvec3 centerToActualSurfaceModelSpace = posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    dvec3 centerToActualSurface = dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    double distFromSurfaceToCamera = glm::length(actualSurfaceToCamera);

	double distFromCenterToSurface = length(centerToActualSurface);
	double distFromCenterToCamera = length(posDiff);

    double speedScale =
        distFromCenterToSurface > 0.0 ?
        glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0) :
        1.0;

    // Get rotation in camera space
    glm::dvec3 eulerAngles = glm::dvec3(
        -_mouseStates.synchedGlobalRotationMouseVelocity().y * deltaTime,
        -_mouseStates.synchedGlobalRotationMouseVelocity().x * deltaTime,
        0) * speedScale;
    glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

    // Transform to world space
    glm::dquat rotationDiffWorldSpace =
        globalCameraRotation *
        rotationDiffCamSpace *
        glm::inverse(globalCameraRotation);

    // Rotate and find the difference vector
    glm::dvec3 rotationDiffVec3 =
        (distFromCenterToCamera * surfaceNormal)
         * rotationDiffWorldSpace
        - (distFromCenterToCamera * surfaceNormal);

    // Add difference to position
    cameraPosition += rotationDiffVec3;
}

void OrbitalNavigator::followFocusNodeRotation(
    glm::dvec3 objectPosition,
    glm::dquat& focusNodeRotationDiff,
    glm::dvec3& cameraPosition)
{
    glm::dvec3 posDiff = cameraPosition - objectPosition;
    glm::dvec3 rotationDiffVec3AroundCenter =
        posDiff
        * focusNodeRotationDiff
        - (posDiff);
    cameraPosition += rotationDiffVec3AroundCenter;
}

void OrbitalNavigator::performGlobalRotation(
    glm::dvec3 objectPosition,
    glm::dquat& focusNodeRotationDiff,
    glm::dvec3& cameraPosition,
    glm::dquat& globalCameraRotation)
{
    using namespace glm;

    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    glm::dvec3 directionFromSurfaceToCameraModelSpace = posHandle.referenceSurfaceOutDirection;
    glm::dvec3 directionFromSurfaceToCamera =
      normalize(dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);

    glm::dvec3 lookUpWhenFacingSurface =
        inverse(focusNodeRotationDiff) * globalCameraRotation * glm::dvec3(0.0, 1.0, 0.0);
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0, 0, 0),
        -directionFromSurfaceToCamera,
        lookUpWhenFacingSurface);
    globalCameraRotation =
        glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
}

void OrbitalNavigator::performVerticalTranslation(
    double deltaTime,
    glm::dvec3 objectPosition,
    glm::dvec3& cameraPosition)
{
	using namespace glm;

    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    dvec3 posDiff = cameraPosition - objectPosition;

    dvec3 centerToReferenceSurface = dmat3(modelTransform) * posHandle.centerToReferenceSurface;
    dvec3 centerToActualSurfaceModelSpace = posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    dvec3 centerToActualSurface = dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;

    cameraPosition += -actualSurfaceToCamera *
        _mouseStates.synchedTruckMovementMouseVelocity().y * deltaTime;
}

void OrbitalNavigator::performHorizontalRotation(
    double deltaTime,
    glm::dvec3 cameraPosition,
    glm::dquat& globalCameraRotation)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    glm::dvec3 directionFromSurfaceToCameraModelSpace = posHandle.referenceSurfaceOutDirection;
    glm::dvec3 directionFromSurfaceToCamera =
      glm::normalize(glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);

    glm::dquat cameraRollRotation =
        glm::angleAxis(_mouseStates.synchedGlobalRollMouseVelocity().x * deltaTime, directionFromSurfaceToCamera);
    globalCameraRotation = cameraRollRotation * globalCameraRotation;
}

void OrbitalNavigator::pushToSurface(
    double minHeightAboveGround,
    glm::dvec3 objectPosition,
    glm::dvec3& cameraPosition)
{
	using namespace glm;
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    dvec3 posDiff = cameraPosition - objectPosition;

	dvec3 referenceSurfaceOutDirection = glm::dmat3(modelTransform) * posHandle.referenceSurfaceOutDirection;
    dvec3 centerToReferenceSurface = glm::dmat3(modelTransform) * posHandle.centerToReferenceSurface;
    dvec3 centerToActualSurfaceModelSpace = posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    dvec3 centerToActualSurface = glm::dmat3(modelTransform) * centerToActualSurfaceModelSpace;
    dvec3 actualSurfaceToCamera = posDiff - centerToActualSurface;
    double surfaceToCameraSigned =
        glm::length(actualSurfaceToCamera) *
        glm::sign(dot(actualSurfaceToCamera, referenceSurfaceOutDirection));

    cameraPosition += referenceSurfaceOutDirection *
        glm::max(minHeightAboveGround - surfaceToCameraSigned, 0.0);
}

glm::dquat OrbitalNavigator::interpolateRotationDifferential(
    double deltaTime,
    double interpolationTime,
    glm::dquat rotationDiff,
    glm::dvec3 objectPosition,
    glm::dvec3 cameraPosition)
{
    glm::dmat4 inverseModelTransform = _focusNode->inverseModelTransform();
    glm::dmat4 modelTransform = _focusNode->modelTransform();

    glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1));
    SurfacePositionHandle posHandle =
        _focusNode->calculateSurfacePositionHandle(cameraPositionModelSpace);

    double maximumDistanceForRotation = glm::length(
        glm::dmat3(modelTransform) * posHandle.centerToReferenceSurface) * _followFocusNodeRotationDistance;
    double distanceToCamera = glm::length(cameraPosition - objectPosition);

    double interpolationSign = glm::sign(maximumDistanceForRotation - distanceToCamera);

    _followRotationInterpolator.setDeltaTime(interpolationSign * deltaTime);
    _followRotationInterpolator.step();

    return glm::slerp(glm::dquat(glm::dvec3(0.0)), rotationDiff, _followRotationInterpolator.value());
}

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
void OrbitalNavigator::goToChunk(
    Camera& camera,
    globebrowsing::TileIndex ti,
    glm::vec2 uv,
    bool resetCameraDirection)
{
    using namespace globebrowsing;
    
    RenderableGlobe* globe = castRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    // Camera position in model space
    glm::dvec3 camPos = camera.positionVec3();
    glm::dmat4 inverseModelTransform = globe->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
    glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));
    
    GeodeticPatch patch(ti);
    Geodetic2 corner = patch.getCorner(SOUTH_WEST);
    Geodetic2 positionOnPatch = patch.getSize();
    positionOnPatch.lat *= uv.y;
    positionOnPatch.lon *= uv.x;
    Geodetic2 pointPosition = corner + positionOnPatch;
    
    glm::dvec3 positionOnEllipsoid =
        globe->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);
    
    goToGeodetic3(camera, {pointPosition, altitude});
    
    if (resetCameraDirection) {
        this->resetCameraDirection(camera, pointPosition);
    }
}

void OrbitalNavigator::goToGeodetic2(Camera& camera,
    globebrowsing::Geodetic2 geo2, bool resetCameraDirection)
{
    using namespace globebrowsing;

    RenderableGlobe* globe = castRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }
    
    // Camera position in model space
    glm::dvec3 camPos = camera.positionVec3();
    glm::dmat4 inverseModelTransform = globe->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
    glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));
        
    glm::dvec3 positionOnEllipsoid =
    globe->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);
        
    goToGeodetic3(camera, {geo2, altitude});
        
    if (resetCameraDirection) {
        this->resetCameraDirection(camera, geo2);
    }
}
    
void OrbitalNavigator::goToGeodetic3(Camera& camera, globebrowsing::Geodetic3 geo3) {
	using namespace globebrowsing;
	
	RenderableGlobe* globe = castRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    glm::dvec3 positionModelSpace = globe->ellipsoid().cartesianPosition(geo3);
    glm::dmat4 modelTransform = globe->modelTransform();
    glm::dvec3 positionWorldSpace = modelTransform * glm::dvec4(positionModelSpace, 1.0);
    camera.setPositionVec3(positionWorldSpace);
}

void OrbitalNavigator::resetCameraDirection(Camera& camera,  globebrowsing::Geodetic2 geo2) {
    using namespace globebrowsing;
    
    RenderableGlobe* globe = castRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    // Camera is described in world space
    glm::dmat4 modelTransform = globe->modelTransform();
    
    // Lookup vector
    glm::dvec3 positionModelSpace = globe->ellipsoid().cartesianSurfacePosition(geo2);
    glm::dvec3 slightlyNorth = globe->ellipsoid().cartesianSurfacePosition(
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

globebrowsing::RenderableGlobe* OrbitalNavigator::castRenderableToGlobe() {
    using namespace globebrowsing;

    Renderable* baseRenderable = _focusNode->renderable();
	if (!baseRenderable) {
		return nullptr;
	}
    if (globebrowsing::RenderableGlobe* globe =
            dynamic_cast<globebrowsing::RenderableGlobe*>(baseRenderable))
    {
        return globe;
    }
    else {
        return nullptr;
    }
}

#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED

} // namespace interaction
} // namespace openspace
