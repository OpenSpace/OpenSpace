/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/navigation/waypoint.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "Waypoint";

    constexpr float LengthEpsilon = 1e-5f;
    constexpr const char* SunIdentifier = "Sun";
} // namespace

namespace openspace::interaction {

Waypoint::Waypoint(const glm::dvec3& pos, const glm::dquat& rot, std::string ref)
    : _nodeIdentifier(std::move(ref))
{
    _pose = { .position = pos, .rotation = rot };

    const SceneGraphNode* node = sceneGraphNode(_nodeIdentifier);
    if (!node) {
        LERROR(std::format("Could not find node '{}'", _nodeIdentifier));
        return;
    }

    const PathNavigator& navigator = global::navigationHandler->pathNavigator();
    _validBoundingSphere = navigator.findValidBoundingSphere(node);
}

Waypoint::Waypoint(const NavigationState& ns) {
    const SceneGraphNode* anchorNode = sceneGraphNode(ns.anchor);

    if (!anchorNode) {
        LERROR(std::format("Could not find node '{}' to target", ns.anchor));
        return;
    }

    _nodeIdentifier = ns.anchor;

    if (!ns.aim.empty()) {
        const SceneGraphNode* aimNode = sceneGraphNode(ns.aim);
        if (!aimNode) {
            LERROR(std::format("Could not find node '{}' to use as aim", ns.aim));
            return;
        }
        _aimNodeIdentifier = ns.aim;
    }

    const PathNavigator& navigator = global::navigationHandler->pathNavigator();
    _validBoundingSphere = navigator.findValidBoundingSphere(anchorNode);
    _pose = ns.cameraPose();
}

CameraPose Waypoint::pose() const {
    return _pose;
}

glm::dvec3 Waypoint::position() const {
    return _pose.position;
}

glm::dquat Waypoint::rotation() const {
    return _pose.rotation;
}

SceneGraphNode* Waypoint::node() const {
    return sceneGraphNode(_nodeIdentifier);
}

std::string Waypoint::nodeIdentifier() const {
    return _nodeIdentifier;
}

std::optional<std::string> Waypoint::aimIdentifier() const {
    return _aimNodeIdentifier;
}

double Waypoint::validBoundingSphere() const {
    return _validBoundingSphere;
}

Waypoint waypointFromCamera() {
    Camera* camera = global::navigationHandler->camera();
    const glm::dvec3 pos = camera->positionVec3();
    const glm::dquat rot = camera->rotationQuaternion();
    const std::string node = global::navigationHandler->anchorNode()->identifier();
    return Waypoint{ pos, rot, node };
}

// Compute a target position close to the specified target node, using knowledge of
// the start point and a desired distance from the node's center
glm::dvec3 computeGoodStepDirection(const SceneGraphNode* targetNode,
                                    const Waypoint& startPoint)
{
    const glm::dvec3 nodePos = targetNode->worldPosition();
    const SceneGraphNode* sun = sceneGraphNode(SunIdentifier);
    const SceneGraphNode* closeNode = PathNavigator::findNodeNearTarget(targetNode);

    // @TODO (2021-07-09, emmbr): Not nice to depend on a specific scene graph node,
    // as it might not exist. Ideally, each SGN could know about their preferred
    // direction to be viewed from (their "good side"), and then that could be queried
    // and used instead.
    if (closeNode) {
        // If the node is close to another node in the scene, set the direction in a way
        // that minimizes risk of collision
        return glm::normalize(nodePos - closeNode->worldPosition());
    }
    else if (!sun) {
        // Can't compute position from Sun position, so just use any direction. Z will do
        return glm::dvec3(0.0, 0.0, 1.0);
    }
    else if (targetNode->identifier() == SunIdentifier) {
        // Special case for when the target is the Sun, in which we want to avoid a zero
        // vector. The Z axis is chosen to provide an overview of the solar system, and
        // not stay in the orbital plane
        return glm::dvec3(0.0, 0.0, 1.0);
    }
    else {
        // Go to a point that is lit up by the sun, slightly offsetted from sun direction
        const glm::dvec3 sunPos = sun->worldPosition();

        const glm::dvec3 prevPos = startPoint.position();
        const glm::dvec3 targetToPrev = prevPos - nodePos;
        const glm::dvec3 targetToSun = sunPos - nodePos;

        // Check against zero vectors, as this will lead to nan-values from cross product
        if (glm::length(targetToSun) < LengthEpsilon ||
            glm::length(targetToPrev) < LengthEpsilon)
        {
            // Same situation as if sun does not exist. Any direction will do
            return glm::dvec3(0.0, 0.0, 1.0);
        }

        constexpr float defaultPositionOffsetAngle = -30.f; // degrees
        constexpr float angle = glm::radians(defaultPositionOffsetAngle);
        const glm::dvec3 axis = glm::normalize(glm::cross(targetToPrev, targetToSun));
        const glm::dquat offsetRotation = angleAxis(static_cast<double>(angle), axis);

        return glm::normalize(offsetRotation * targetToSun);
    }
}

Waypoint computeWaypointFromNodeInfo(const NodeCameraStateSpec& spec,
                                     const std::optional<Waypoint>& startPoint,
                                     bool useLinear)
{
    const SceneGraphNode* targetNode = sceneGraphNode(spec.identifier);
    if (!targetNode) {
        LERROR(std::format("Could not find target node '{}'", spec.identifier));
        return Waypoint();
    }

    // Use current camera position if no previous point was specified
    const Waypoint prevPoint = startPoint.value_or(waypointFromCamera());

    glm::dvec3 stepDir;
    glm::dvec3 cameraPos;
    if (spec.position.has_value()) {
        // The position in instruction is given in the targetNode's local coordinates.
        // Convert to world coordinates
        cameraPos = glm::dvec3(
            targetNode->modelTransform() * glm::dvec4(*spec.position, 1.0)
        );
    }
    else {
        const PathNavigator& navigator = global::navigationHandler->pathNavigator();

        const double radius = navigator.findValidBoundingSphere(targetNode);
        const double defaultHeight = navigator.defaultArrivalHeight(spec.identifier);
        const double height = spec.height.value_or(defaultHeight);
        const double distanceFromNodeCenter = radius + height;

        if (useLinear) {
            // If linear path, compute position along line form start to end point
            const glm::dvec3 endNodePos = targetNode->worldPosition();
            stepDir = glm::normalize(prevPoint.position() - endNodePos);
        }
        else {
            stepDir = computeGoodStepDirection(targetNode, prevPoint);
        }

        cameraPos = targetNode->worldPosition() + stepDir * distanceFromNodeCenter;
    }

    glm::dvec3 up = global::navigationHandler->camera()->lookUpVectorWorldSpace();
    if (spec.useTargetUpDirection) {
        // @TODO (emmbr 2020-11-17) For now, this is hardcoded to look good for Earth,
        // which is where it matters the most. A better solution would be to make each
        // sgn aware of its own 'up' and query
        up = targetNode->worldRotationMatrix() * glm::dvec3(0.0, 0.0, 1.0);
    }

    // Compute rotation so the camera is looking at the targetted node
    glm::dvec3 lookAtPos = targetNode->worldPosition();

    // Check if we can distinguish between cameraPos and lookAt pos. Otherwise, move it
    // further away
    const glm::dvec3 diff = cameraPos - lookAtPos;
    const double distSquared = glm::dot(diff, diff);
    if (std::isnan(distSquared) || distSquared < LengthEpsilon) {
        const double startToEndDist = glm::length(
            prevPoint.position() - targetNode->worldPosition()
        );
        lookAtPos = cameraPos - stepDir * 0.1 * startToEndDist;
    }

    const glm::dquat targetRot = ghoul::lookAtQuaternion(cameraPos, lookAtPos, up);

    return Waypoint(cameraPos, targetRot, spec.identifier);
}

} // namespace openspace::interaction
