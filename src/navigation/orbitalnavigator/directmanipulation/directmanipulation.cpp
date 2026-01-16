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

#include <openspace/navigation/orbitalnavigator/directmanipulation/directmanipulation.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator/orbitalnavigator.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <utility>

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4310) // cast truncates constant value
#endif // WIN32

#include <glm/gtx/intersect.hpp>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

namespace {
    constexpr std::string_view _loggerCat = "DirectManipulation";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enable direct manipulation",
        "Decides whether the direct manipulation mode should be enabled or not.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        DirectManipulationThresholdInfo =
    {
        "DirectManipulationThreshold",
        "Direct manipulation threshold",
        "This threshold affects the distance from the interaction sphere at which the "
        "direct manipulation interaction mode starts being active. The value is given "
        "as a factor times the interaction sphere.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        DefaultDirectTouchRenderableTypesInfo =
    {
        "DefaultDirectTouchRenderableTypes",
        "Default direct touch renderable types",
        "A list of renderable types that will automatically use the \'direct "
        "manipulation\' scheme when interacted with, keeping the finger on a static "
        "position on the interaction sphere of the object when touching. Good for "
        "relatively spherical objects.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace::interaction {

DirectManipulation::DirectManipulation()
    : properties::PropertyOwner({ "DirectManipulation", "Direct Manipulation" })
    , _enabled(EnabledInfo, true)
    , _directTouchDistanceThreshold(DirectManipulationThresholdInfo, 5.f, 0.f, 10.f)
    , _defaultDirectTouchRenderableTypes(DefaultDirectTouchRenderableTypesInfo)
{
    addProperty(_enabled);
    addProperty(_directTouchDistanceThreshold);

    _defaultDirectTouchRenderableTypes.onChange([this]() {
        _sortedDefaultRenderableTypes.clear();
        for (const std::string& s : _defaultDirectTouchRenderableTypes.value()) {
            ghoul::TemplateFactory<Renderable>* fRenderable =
                FactoryManager::ref().factory<Renderable>();

            if (!fRenderable->hasClass(s)) {
                LWARNING(std::format(
                    "In property 'DefaultDirectTouchRenderableTypes': '{}' is not a "
                    "registered renderable type. Ignoring", s
                ));
                continue;
            }

            _sortedDefaultRenderableTypes.insert(s);
        }
    });
    addProperty(_defaultDirectTouchRenderableTypes);
}

void DirectManipulation::updateStateFromInput() {
    const std::vector<TouchInputHolder>& touchPoints =
        global::interactionHandler->touchInputState().touchPoints();

    if (!_enabled || touchPoints.empty()) {
        // No fingers, no input
        _selectedNodeSurfacePoints.clear();
        return;
    }

    bool isValidNode = isValidDirectTouchNode();
    bool isCloseEnough = isWithinDirectTouchDistance();
    bool isValidTouchPoints = !_selectedNodeSurfacePoints.empty() &&
        touchPoints.size() == _selectedNodeSurfacePoints.size();

    bool shouldApply =  isCloseEnough && isValidNode && isValidTouchPoints;

    if (shouldApply) {
        applyDirectControl(touchPoints);
    }

    if (isCloseEnough && isValidNode) {
        updateNodeSurfacePoints(touchPoints);
    }
}

void DirectManipulation::applyDirectControl(
                                         const std::vector<TouchInputHolder>& touchPoints)
{
    // Find best transform values for the new camera state and store them in param
    std::vector<double> param(6, 0.0);
    bool lmSuccess = _directInputSolver.solve(
        touchPoints,
        _selectedNodeSurfacePoints,
        &param,
        *global::navigationHandler->camera()
    );

    if (!lmSuccess) {
        return;
    }

    int nDof = _directInputSolver.nDof();
    VelocityStates velocities;

    // If good values were found set new camera state
    velocities.orbit = glm::dvec2(param[0], param[1]);
    if (nDof > 2) {
        velocities.zoom = param[2];
        velocities.roll = param[3];
        if (nDof > 4) {
            velocities.roll = 0.0;
            velocities.pan = glm::dvec2(param[4], param[5]);
        }
    }
    stepDirectTouch(velocities);

    global::navigationHandler->orbitalNavigator().resetVelocities();
}

void DirectManipulation::updateNodeSurfacePoints(
                                         const std::vector<TouchInputHolder>& touchPoints)
{
    _selectedNodeSurfacePoints.clear();

    const SceneGraphNode* anchor =
        global::navigationHandler->orbitalNavigator().anchorNode();

    const Camera* camera = global::navigationHandler->camera();

    SceneGraphNode* node = sceneGraphNode(anchor->identifier());

    glm::dquat camToWorldSpace = camera->rotationQuaternion();
    glm::dvec3 camPos = camera->positionVec3();
    std::vector<DirectInputSolver::SelectedBody> surfacePoints;

    for (const TouchInputHolder& inputHolder : touchPoints) {
        // Normalized -1 to 1 coordinates on screen
        const double xCo = 2 * (inputHolder.latestInput().x - 0.5);
        const double yCo = -2 * (inputHolder.latestInput().y - 0.5);

        const glm::dvec3 cursorInWorldSpace = camToWorldSpace *
            glm::dvec3(glm::inverse(camera->projectionMatrix()) *
            glm::dvec4(xCo, yCo, -1.0, 1.0));

        const glm::dvec3 raytrace = glm::normalize(cursorInWorldSpace);
        const size_t id = inputHolder.fingerId();

        // Compute positions on anchor node, by checking if touch input
        // intersect interaction sphere
        double intersectionDist = 0.0;
        const bool intersected = glm::intersectRaySphere(
            camPos,
            raytrace,
            node->worldPosition(),
            node->interactionSphere() * node->interactionSphere(),
            intersectionDist
        );

        if (intersected) {
            glm::dvec3 intersectionPos = camPos + raytrace * intersectionDist;
            glm::dvec3 pointInModelView = glm::inverse(node->worldRotationMatrix()) *
                                            (intersectionPos - node->worldPosition());

            // Note that node is saved as the direct input solver was initially
            // implemented to handle touch contact points on multiple nodes
            surfacePoints.push_back({ id, node, pointInModelView });
        }
    }

    _selectedNodeSurfacePoints = std::move(surfacePoints);
}

// Main update call, calculates the new orientation and position for the camera depending
// on _vel and dt. Called every frame
void DirectManipulation::stepDirectTouch(const VelocityStates& velocities) {
    const SceneGraphNode* anchor =
        global::navigationHandler->orbitalNavigator().anchorNode();

    Camera* camera = global::navigationHandler->camera();

    if (!anchor || !camera) {
        return;
    }

    // Create variables from current state
    glm::dvec3 camPos = camera->positionVec3();
    const glm::dvec3 centerPos = anchor->worldPosition();

    glm::dvec3 directionToCenter = normalize(centerPos - camPos);
    const glm::dvec3 centerToCamera = camPos - centerPos;
    const glm::dvec3 lookUp = camera->lookUpVectorWorldSpace();
    const glm::dvec3 camDirection = camera->viewDirectionWorldSpace();

    // Make a representation of the rotation quaternion with local and global
    // rotations. To avoid problem with lookup in up direction
    const glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0.0, 0.0, 0.0),
        directionToCenter,
        glm::normalize(camDirection + lookUp)
    );
    glm::dquat globalCamRot = glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
    glm::dquat localCamRot = inverse(globalCamRot) * camera->rotationQuaternion();

    {
        // Roll
        const glm::dquat camRollRot = glm::angleAxis(velocities.roll, glm::dvec3(0.0, 0.0, 1.0));
        localCamRot = localCamRot * camRollRot;
    }
    {
        // Panning (local rotation)
        const glm::dvec3 eulerAngles = glm::dvec3(velocities.pan.y, velocities.pan.x, 0.0);
        const glm::dquat rotationDiff = glm::dquat(eulerAngles);
        localCamRot = localCamRot * rotationDiff;
    }
    {
        // Orbit (global rotation)
        const glm::dvec3 eulerAngles(velocities.orbit.y, velocities.orbit.x, 0.0);
        const glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

        const glm::dquat rotationDiffWorldSpace = globalCamRot * rotationDiffCamSpace *
            inverse(globalCamRot);
        const glm::dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace -
            centerToCamera;
        camPos += rotationDiffVec3;

        const glm::dvec3 centerToCam = camPos - centerPos;
        directionToCenter = glm::normalize(-centerToCam);
        const glm::dvec3 lookUpWhenFacingCenter = globalCamRot *
            glm::dvec3(camera->lookUpVectorCameraSpace());

        const glm::dmat4 lookAtMatrix = glm::lookAt(
            glm::dvec3(0.0),
            directionToCenter,
            lookUpWhenFacingCenter
        );
        globalCamRot = glm::normalize(glm::quat_cast(glm::inverse(lookAtMatrix)));
    }
    {
        // Zooming
        double zoomVelocity = velocities.zoom;
        const glm::dvec3 zoomDistanceInc = directionToCenter * zoomVelocity;
        camPos += zoomDistanceInc;
    }

    // Update the camera state
    camera->setPositionVec3(camPos);
    camera->setRotation(globalCamRot * localCamRot);

    // Mark that a camera interaction happened
    global::navigationHandler->orbitalNavigator().markCameraInteraction();
}

bool DirectManipulation::isValidDirectTouchNode() const {
    const SceneGraphNode* anchor =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!anchor || !anchor->renderable()) {
        return false;
    }

    // Check if current anchor is valid for direct touch
    std::string renderableType = std::string(anchor->renderable()->typeAsString());

    bool isDirectTouchRenderable = _sortedDefaultRenderableTypes.find(renderableType) !=
        _sortedDefaultRenderableTypes.end();

    return anchor->supportsDirectInteraction() || isDirectTouchRenderable;
}

bool DirectManipulation::isWithinDirectTouchDistance() const {
    const SceneGraphNode* anchor =
        global::navigationHandler->orbitalNavigator().anchorNode();

    Camera* camera = global::navigationHandler->camera();

    if (!anchor || !camera) {
        return false;
    }

    const double interactionSphere = anchor->interactionSphere();
    const glm::dvec3 centerToCamera = camera->positionVec3() - anchor->worldPosition();

    // Check if camera is within distance for direct manipulation to be applicable
    if (interactionSphere > 0.0) {
        const double distance = std::max(
            length(centerToCamera) - interactionSphere,
            0.0
        );
        const double maxDistance = interactionSphere * _directTouchDistanceThreshold;
        return distance <= maxDistance;
    }

    return false;
}


} // namespace openspace::interaction
