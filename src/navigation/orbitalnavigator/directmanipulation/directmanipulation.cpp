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
#include <openspace/camera/camerapose.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
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

    constexpr openspace::properties::Property::PropertyInfo IsActiveInfo = {
        "IsActive",
        "Is active",
        "True if the direct manipulation interaction scheme is currently being applied.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AllowMouseInputInfo = {
        "AllowMouseInput",
        "Allow mouse input",
        "Decides whether direct manipulation should be applied when using mouse input."
        "Otherwise, it is only applied for touch input.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceThresholdInfo = {
        "DistanceThreshold",
        "Distance threshold factor",
        "This threshold affects the distance from the interaction sphere at which the "
        "direct manipulation interaction mode starts being active. The value is given "
        "as a factor times the interaction sphere.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultRenderableTypesInfo = {
        "DefaultRenderableTypes",
        "Default renderable types",
        "A list of renderable types that will automatically use the direct "
        "manipulation scheme when interacted with, keeping the finger on a static "
        "position on the interaction sphere of the object when touching. Good for "
        "relatively spherical objects.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr std::string_view Description =
        "Direct manipulation is an interaction scheme that allows rotating an object in "
        "a way so that, while touching, each finger will kept on a static position on "
        "the interaction sphere of the object. Per default, the scheme is only active "
        "for touch interaction. Note that the camera has to be within a certain distance "
        "of the object, and that direct manipulation will only be applied for specified "
        "renderable types.";
} // namespace

namespace openspace::interaction {

DirectManipulation::DirectManipulation()
    : properties::PropertyOwner({
        "DirectManipulation",
        "Direct Manipulation",
        std::string(Description)
      })
    , _enabled(EnabledInfo, true)
    , _isActive(IsActiveInfo, false)
    , _allowMouseInput(AllowMouseInputInfo, false)
    , _distanceThreshold(DistanceThresholdInfo, 5.f, 0.f, 10.f)
    , _defaultRenderableTypes(DefaultRenderableTypesInfo)
{
    addProperty(_enabled);

    _isActive.setReadOnly(true);
    addProperty(_isActive);

    addProperty(_allowMouseInput);

    addProperty(_distanceThreshold);

    _defaultRenderableTypes.onChange([this]() {
        _sortedDefaultRenderableTypes.clear();
        for (const std::string& s : _defaultRenderableTypes.value()) {
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
    addProperty(_defaultRenderableTypes);
}

void DirectManipulation::updateCameraFromInput() {
    std::vector<TouchInputHolder> touchInputs =
        global::interactionHandler->touchInputState().touchPoints();

    std::vector<TouchPoint> touchPoints;
    touchPoints.reserve(touchInputs.size());
    for (const TouchInputHolder& input : touchInputs) {
        touchPoints.push_back({
            .id = input.latestInput().fingerId,
            .x = input.latestInput().x,
            .y = input.latestInput().y
        });
    }

    if (_allowMouseInput && touchPoints.empty()) {
        // Translate mouse input to touch input
        const MouseInputState& mouseState =
            global::interactionHandler->mouseInputState();

        // @TODO (emmbr, 2026-01-19): Allow inverting this? In that case we should move
        // the invert setting to the interaction handler? (now it exists in orbital input)
        const bool isPrimaryPressed =
            mouseState.isMouseButtonPressed(MouseButton::Button1);

        if (isPrimaryPressed) {
            glm::dvec2 mousePos = mouseState.mousePosition();
            // Normalize based on screensize
            const glm::ivec2 screenSize = global::windowDelegate->currentWindowSize();
            mousePos.x /= static_cast<double>(screenSize.x);
            mousePos.y /= static_cast<double>(screenSize.y);
            touchPoints.push_back({ .x = mousePos.x, .y = mousePos.y });
        }
    }

    if (!_enabled || touchPoints.empty()) {
        // No fingers, no input
        _selectedNodeSurfacePoints.clear();
        _isActive = false;
        return;
    }

    bool isValidNode = isValidDirectTouchNode();
    bool isCloseEnough = isWithinDirectTouchDistance();
    bool isValidTouchPoints = !_selectedNodeSurfacePoints.empty() &&
        touchPoints.size() == _selectedNodeSurfacePoints.size();

    bool shouldApply =  isCloseEnough && isValidNode && isValidTouchPoints;

    if (shouldApply) {
        applyDirectControl(touchPoints);
        _isActive = true;
    }
    else {
        _isActive = false;
    }

    if (isCloseEnough && isValidNode) {
        updateNodeSurfacePoints(touchPoints);
    }
}

void DirectManipulation::applyDirectControl(const std::vector<TouchPoint>& touchPoints) {
    Camera* camera = global::navigationHandler->camera();
    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();

    if (!anchor || !camera) {
        return;
    }

    // Find best transform values for the new camera state and store them in param
    std::vector<double> param(6, 0.0);
    bool lmSuccess = _directInputSolver.solve(
        touchPoints,
        _selectedNodeSurfacePoints,
        &param,
        *camera
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

    CameraPose pose = cameraPoseFromVelocities(velocities, camera, anchor);

    camera->setPose(pose);

    // Mark that a camera interaction happened and then reset velocities
    global::navigationHandler->orbitalNavigator().markCameraInteraction();
    global::navigationHandler->orbitalNavigator().resetVelocities();
}

void DirectManipulation::updateNodeSurfacePoints(
                                               const std::vector<TouchPoint>& touchPoints)
{
    _selectedNodeSurfacePoints.clear();

    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();
    const Camera* camera = global::navigationHandler->camera();

    const glm::dquat camRotation = camera->rotationQuaternion();
    const glm::dvec3 camPos = camera->positionVec3();

    std::vector<DirectInputSolver::SelectedBody> surfacePoints;

    for (const TouchPoint& touchPoint : touchPoints) {
        const size_t id = touchPoint.id;
        // Normalized -1 to 1 coordinates on screen
        const double xCo = 2.0 * (touchPoint.x - 0.5);
        const double yCo = -2.0 * (touchPoint.y - 0.5);

        const glm::dvec3 cursorInWorldSpace = camRotation *
            glm::dvec3(glm::inverse(camera->projectionMatrix()) *
            glm::dvec4(xCo, yCo, -1.0, 1.0));

        const glm::dvec3 rayDirection = glm::normalize(cursorInWorldSpace);

        // Compute positions on anchor node, by checking if touch input
        // intersect interaction sphere
        double intersectionDist = 0.0;
        const double interactionSphere = anchor->interactionSphere();
        const bool intersected = glm::intersectRaySphere(
            camPos,
            rayDirection,
            anchor->worldPosition(),
            interactionSphere * interactionSphere,
            intersectionDist
        );

        if (intersected) {
            glm::dvec3 intersectionPos = camPos + rayDirection * intersectionDist;
            glm::dvec3 pointInModelView = glm::inverse(anchor->worldRotationMatrix()) *
                                            (intersectionPos - anchor->worldPosition());

            // Note that node is saved as the direct input solver was initially
            // implemented to handle touch contact points on multiple nodes
            surfacePoints.push_back({ id, anchor, pointInModelView });
        }
    }

    _selectedNodeSurfacePoints = std::move(surfacePoints);
}

CameraPose DirectManipulation::cameraPoseFromVelocities(const VelocityStates& velocities,
                                                        const Camera* camera,
                                                        const SceneGraphNode* anchor)
{
    ghoul_assert(camera != nullptr, "Camera must not be null");
    ghoul_assert(anchor != nullptr, "Anchor node must not be null");

    const glm::dvec3 anchorPos = anchor->worldPosition();

    CameraPose pose = camera->pose();

    // Make a representation of the rotation quaternion with local and global rotations
    CameraRotationDecomposition rot = decomposeCameraRotation(pose, anchorPos);

    {
        // Roll (local rotation)
        const glm::dvec3 zAxis = glm::dvec3(0.0, 0.0, 1.0);
        const glm::dquat camRollRot = glm::angleAxis(velocities.roll, zAxis);
        rot.localRotation = rot.localRotation * camRollRot;
    }
    {
        // Panning (local rotation)
        const glm::dvec3 eulerAngles = glm::dvec3(velocities.pan.y, velocities.pan.x, 0.0);
        const glm::dquat rotationDiff = glm::dquat(eulerAngles);
        rot.localRotation = rot.localRotation * rotationDiff;
    }
    {
        // Orbit (global rotation)

        // Rotate position
        const glm::dvec3 eulerAngles = glm::dvec3(velocities.orbit.y, velocities.orbit.x, 0.0);
        const glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);
        const glm::dquat rotationDiffWorldSpace =
            rot.globalRotation * rotationDiffCamSpace * glm::inverse(rot.globalRotation);

        const glm::dvec3 centerToCamera = pose.position - anchorPos;
        const glm::dvec3 rotationDiffVec3 =
            centerToCamera * rotationDiffWorldSpace - centerToCamera;

        pose.position += rotationDiffVec3;

        // Rotate camera to look at center again
        const glm::dvec3 newPositionToCenter = anchorPos - pose.position;
        const glm::dvec3 lookUpWhenFacingCenter = rot.globalRotation *
            glm::dvec3(camera->lookUpVectorCameraSpace());

        rot.globalRotation = ghoul::lookAtQuaternion(
            glm::dvec3(0.0),
            newPositionToCenter,
            lookUpWhenFacingCenter
        );
    }
    {
        // Zooming
        const glm::dvec3 directionToCenter = normalize(anchorPos - pose.position);
        pose.position += directionToCenter * velocities.zoom;
    }

    pose.rotation = composeCameraRotation(rot);

    return pose;
}

bool DirectManipulation::isValidDirectTouchNode() const {
    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();

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
    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();
    const Camera* camera = global::navigationHandler->camera();

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
        const double maxDistance = interactionSphere * _distanceThreshold;
        return distance <= maxDistance;
    }

    return false;
}

} // namespace openspace::interaction
