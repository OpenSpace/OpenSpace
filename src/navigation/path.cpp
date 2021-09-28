/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/navigation/path.h>

#include <openspace/camera/camera.h>
#include <openspace/camera/camerapose.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/pathcurve.h>
#include <openspace/navigation/pathcurves/avoidcollisioncurve.h>
#include <openspace/navigation/pathcurves/zoomoutoverviewcurve.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/collisionhelper.h>
#include <openspace/util/universalhelpers.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>

namespace {
    constexpr const char _loggerCat[] = "Path";
    constexpr const float Epsilon = 1e-5f;

    constexpr const char SunIdentifier[] = "Sun";

    // TODO: where should this documentation be?
    // It's nice to have these to interpret the dictionary when creating the path, but
    // maybe it's not really necessary
    struct [[codegen::Dictionary(PathInstruction)]] Parameters {
        enum class TargetType {
            Node,
            NavigationState
        };
        TargetType targetType;

        // The desired duration traversing the specified path segment should take
        std::optional<float> duration;

        // (Node): The target node of the camera path. Not optional for 'Node' instructions
        std::optional<std::string> target;

        // (Node): An optional position in relation to the target node, in model
        // coordinates (meters)
        std::optional<glm::dvec3> position;

        // (Node): An optional height in relation to the target node, in meters
        std::optional<double> height;

        // (Node): If true, the up direction of the node is taken into account when
        // computing the wayopoint for this instruction
        std::optional<bool> useTargetUpDirection;

        // (NavigationState): A navigation state that will be the target
        // of this path segment
        std::optional<ghoul::Dictionary> navigationState
            [[codegen::reference("core_navigation_state")]];

        // A navigation state that determines the start state for the camera path
        std::optional<ghoul::Dictionary> startState
            [[codegen::reference("core_navigation_state")]];
    };
#include "path_codegen.cpp"
} // namespace

namespace openspace::interaction {

Path::Path(Waypoint start, Waypoint end, Type type,
           std::optional<double> duration)
    : _start(start), _end(end), _type(type)
{
    switch (_type) {
        case Type::AvoidCollision:
        case Type::AvoidCollisionWithLookAt:
            _curve = std::make_unique<AvoidCollisionCurve>(_start, _end);
            break;
        case Type::Linear:
            _curve = std::make_unique<LinearCurve>(_start, _end);
            break;
        case Type::ZoomOutOverview:
            _curve = std::make_unique<ZoomOutOverviewCurve>(_start, _end);
            break;
        default:
            LERROR("Could not create curve. Type does not exist!");
            throw ghoul::MissingCaseException();
    }

    // Compute speed factor to match any given duration, by traversing the path and
    // computing how much faster/slower it should be
    _speedFactorFromDuration = 1.0;
    if (duration.has_value()) {
        constexpr const double dt = 0.05; // 20 fps
        while (!hasReachedEnd()) {
            traversePath(dt);
        }

        // We now know how long it took to traverse the path. Use that
        _speedFactorFromDuration = _progressedTime / *duration;
    }

    // Reset playback variables
    _traveledDistance = 0.0;
    _progressedTime = 0.0;
}

Waypoint Path::startPoint() const { return _start; }

Waypoint Path::endPoint() const { return _end; }

double Path::pathLength() const { return _curve->length(); }

std::vector<glm::dvec3> Path::controlPoints() const {
    return _curve->points();
}

CameraPose Path::traversePath(double dt, float speedScale) {
    double speed = speedAlongPath(_traveledDistance);
    speed *= static_cast<double>(speedScale);
    const double displacement =  dt * speed;

    _progressedTime += dt;
    _traveledDistance += displacement;

    return interpolatedPose(_traveledDistance);
}

std::string Path::currentAnchor() const {
    bool pastHalfway = (_traveledDistance / pathLength()) > 0.5;
    return (pastHalfway) ? _end.nodeIdentifier() : _start.nodeIdentifier();
}

bool Path::hasReachedEnd() const {
    return (_traveledDistance / pathLength()) >= 1.0;
}

CameraPose Path::interpolatedPose(double distance) const {
    const double relativeDistance = distance / pathLength();
    CameraPose cs;
    cs.position = _curve->positionAt(relativeDistance);
    cs.rotation = interpolateRotation(relativeDistance);
    return cs;
}

glm::dquat Path::interpolateRotation(double t) const {
    switch (_type) {
        case Type::AvoidCollision:
        case Type::Linear:
            return easedSlerpRotation(t);
        case Type::ZoomOutOverview:
        case Type::AvoidCollisionWithLookAt:
            return lookAtTargetsRotation(t);
        default:
            throw ghoul::MissingCaseException();
    }
}

glm::dquat Path::easedSlerpRotation(double t) const {
    double tScaled = helpers::shiftAndScale(t, 0.1, 0.9);
    tScaled = ghoul::sineEaseInOut(tScaled);
    return glm::slerp(_start.rotation(), _end.rotation(), tScaled);
}

glm::dquat Path::lookAtTargetsRotation(double t) const {
    const double t1 = 0.2;
    const double t2 = 0.8;

    const glm::dvec3 startPos = _curve->positionAt(0.0);
    const glm::dvec3 endPos = _curve->positionAt(1.0);
    const glm::dvec3 startNodePos = _start.node()->worldPosition();
    const glm::dvec3 endNodePos = _end.node()->worldPosition();

    glm::dvec3 lookAtPos;
    if (t < t1) {
        // Compute a position in front of the camera at the start orientation
        const double inFrontDistance = glm::distance(startPos, startNodePos);
        const glm::dvec3 viewDir = ghoul::viewDirection(_start.rotation());
        const glm::dvec3 inFrontOfStart = startPos + inFrontDistance * viewDir;

        const double tScaled = ghoul::cubicEaseInOut(t / t1);
        lookAtPos =
            ghoul::interpolateLinear(tScaled, inFrontOfStart, startNodePos);
    }
    else if (t <= t2) {
        const double tScaled = ghoul::cubicEaseInOut((t - t1) / (t2 - t1));
        lookAtPos = ghoul::interpolateLinear(tScaled, startNodePos, endNodePos);
    }
    else if (t > t2) {
        // Compute a position in front of the camera at the end orientation
        const double inFrontDistance = glm::distance(endPos, endNodePos);
        const glm::dvec3 viewDir = ghoul::viewDirection(_end.rotation());
        const glm::dvec3 inFrontOfEnd = endPos + inFrontDistance * viewDir;

        const double tScaled = ghoul::cubicEaseInOut((t - t2) / (1.0 - t2));
        lookAtPos = ghoul::interpolateLinear(tScaled, endNodePos, inFrontOfEnd);
    }

    // Handle up vector separately
    // @TODO (2021-09-06 emmbr) This actually does not interpolate the up vector of the
    // camera, but just the "hint" up vector for the lookAt. This leads to fast rolling
    // when the up vector gets close to the camera's forward vector. Should be improved
    // so any rolling is spread out over the entire motion instead
    double tUp = ghoul::sineEaseInOut(t);
    glm::dvec3 startUp = _start.rotation() * glm::dvec3(0.0, 1.0, 0.0);
    glm::dvec3 endUp = _end.rotation() * glm::dvec3(0.0, 1.0, 0.0);
    glm::dvec3 up = ghoul::interpolateLinear(tUp, startUp, endUp);

    return ghoul::lookAtQuaternion(_curve->positionAt(t), lookAtPos, up);
}

double Path::speedAlongPath(double traveledDistance) const {
    const glm::dvec3 endNodePos = _end.node()->worldPosition();
    const glm::dvec3 startNodePos = _start.node()->worldPosition();

    const CameraPose prevPose = interpolatedPose(traveledDistance);
    const double distanceToEndNode = glm::distance(prevPose.position, endNodePos);
    const double distanceToStartNode = glm::distance(prevPose.position, startNodePos);

    // Decide which is the closest node
    SceneGraphNode* closestNode = _start.node();
    glm::dvec3 closestPos = startNodePos;

    if (distanceToEndNode < distanceToStartNode) {
        closestPos = endNodePos;
        closestNode = _end.node();
    }

    const double distanceToClosestNode = glm::distance(closestPos, prevPose.position);
    double speed = distanceToClosestNode;

    // Dampen speed in beginning of path
    double startUpDistance = 2.0 * _start.node()->boundingSphere();
    if (startUpDistance < Epsilon) { // zero bounding sphere
        startUpDistance = glm::distance(_start.position(), startNodePos);
    }

    if (traveledDistance < startUpDistance) {
        speed *= traveledDistance / startUpDistance + 0.01;
    }

    // Dampen speed in end of path
    // Note: this leads to problems when the full length of the path is really big
    double closeUpDistance = 2.0 * _end.node()->boundingSphere();
    if (closeUpDistance < Epsilon) { // zero bounding sphere
        closeUpDistance = glm::distance(_end.position(), endNodePos);
    }

    if (traveledDistance > (pathLength() - closeUpDistance)) {
        const double remainingDistance = pathLength() - traveledDistance;
        speed *= remainingDistance / closeUpDistance + 0.01;
    }

    // TODO: also dampen speed based on curvature, or make sure the curve has a rounder shape

    // TODO: check for when path is shorter than the starUpDistance or closeUpDistance variables

    return _speedFactorFromDuration * speed;
}

Waypoint waypointFromCamera() {
    Camera* camera = global::navigationHandler->camera();
    const glm::dvec3 pos = camera->positionVec3();
    const glm::dquat rot = camera->rotationQuaternion();
    const std::string node = global::navigationHandler->anchorNode()->identifier();
    return Waypoint{ pos, rot, node };
}

SceneGraphNode* findNodeNearTarget(const SceneGraphNode* node) {
    const std::vector<SceneGraphNode*>& relevantNodes =
        global::navigationHandler->pathNavigator().relevantNodes();

    for (SceneGraphNode* n : relevantNodes) {
        bool isSame = (n->identifier() == node->identifier());
        // If the nodes are in the very same position, they are probably representing
        // the same object
        isSame |= glm::distance(n->worldPosition(), node->worldPosition()) < Epsilon;

        if (isSame) {
            continue;
        }

        constexpr const float proximityRadiusFactor = 3.f;

        const float bs = static_cast<float>(n->boundingSphere());
        const float proximityRadius = proximityRadiusFactor * bs;
        const glm::dvec3 posInModelCoords =
            glm::inverse(n->modelTransform()) * glm::dvec4(node->worldPosition(), 1.0);

        bool isClose = collision::isPointInsideSphere(
            posInModelCoords,
            glm::dvec3(0.0, 0.0, 0.0),
            proximityRadius
        );

        if (isClose) {
            return n;
        }
    }

    return nullptr;
}

// Compute a target position close to the specified target node, using knowledge of
// the start point and a desired distance from the node's center
glm::dvec3 computeGoodStepDirection(const SceneGraphNode* targetNode,
                                    const Waypoint& startPoint)
{
    const glm::dvec3 nodePos = targetNode->worldPosition();
    const SceneGraphNode* closeNode = findNodeNearTarget(targetNode);
    const SceneGraphNode* sun = sceneGraphNode(SunIdentifier);

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

        constexpr const float defaultPositionOffsetAngle = -30.f; // degrees
        constexpr const float angle = glm::radians(defaultPositionOffsetAngle);
        const glm::dvec3 axis = glm::normalize(glm::cross(targetToPrev, targetToSun));
        const glm::dquat offsetRotation = angleAxis(static_cast<double>(angle), axis);

        return glm::normalize(offsetRotation * targetToSun);
    }
}

struct NodeInfo {
    std::string identifier;
    std::optional<glm::dvec3> position;
    std::optional<double> height;
    bool useTargetUpDirection;
};

Waypoint computeWaypointFromNodeInfo(const NodeInfo& info, const Waypoint& startPoint) {
    const SceneGraphNode* targetNode = sceneGraphNode(info.identifier);
    if (!targetNode) {
        LERROR(fmt::format("Could not find target node '{}'", info.identifier));
        return Waypoint();
    }

    glm::dvec3 targetPos = glm::dvec3(0.0);
    if (info.position.has_value()) {
        // The position in instruction is given in the targetNode's local coordinates.
        // Convert to world coordinates
        targetPos = glm::dvec3(targetNode->modelTransform() * glm::dvec4(*info.position, 1.0));
    }
    else {
        const double radius = Waypoint::findValidBoundingSphere(targetNode);
        const double defaultHeight = 2.0 * radius;
        const double height = info.height.value_or(defaultHeight);
        const double distanceFromNodeCenter = radius + height;

        const glm::dvec3 stepDir = computeGoodStepDirection(targetNode, startPoint);
        targetPos = targetNode->worldPosition() + stepDir * distanceFromNodeCenter;
    }

    glm::dvec3 up = global::navigationHandler->camera()->lookUpVectorWorldSpace();
    if (info.useTargetUpDirection) {
        // @TODO (emmbr 2020-11-17) For now, this is hardcoded to look good for Earth,
        // which is where it matters the most. A better solution would be to make each
        // sgn aware of its own 'up' and query
        up = targetNode->worldRotationMatrix() * glm::dvec3(0.0, 0.0, 1.0);
    }

    // Compute rotation so the camera is looking at the targetted node
    const glm::dvec3 lookAtPos = targetNode->worldPosition();
    const glm::dquat targetRot = ghoul::lookAtQuaternion(targetPos, lookAtPos, up);

    return Waypoint(targetPos, targetRot, info.identifier);
}

Path createPathFromDictionary(const ghoul::Dictionary& dictionary, Path::Type type) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    const std::optional<float> duration = p.duration;

    bool hasStart = p.startState.has_value();
    const Waypoint startPoint = hasStart ?
        Waypoint(NavigationState(p.startState.value())) :
        waypointFromCamera();

    // TODO: also handle curve type here

    std::vector<Waypoint> waypoints;
    switch (p.targetType) {
        case Parameters::TargetType::NavigationState: {
            if (!p.navigationState.has_value()) {
                throw ghoul::RuntimeError("A navigation state is required");
            }

            const NavigationState navigationState =
                NavigationState(p.navigationState.value());

            waypoints = { Waypoint(navigationState) };
            break;
        }
        case Parameters::TargetType::Node: {
            if (!p.target.has_value()) {
                throw ghoul::RuntimeError("A target node is required");
            }

            const std::string nodeIdentifier = p.target.value();
            const SceneGraphNode* targetNode = sceneGraphNode(nodeIdentifier);

            if (!targetNode) {
                throw ghoul::RuntimeError(fmt::format(
                    "Could not find target node '{}'", nodeIdentifier
                ));
            }

            NodeInfo info {
                nodeIdentifier,
                p.position,
                p.height,
                p.useTargetUpDirection.value_or(false)
            };

            waypoints = { computeWaypointFromNodeInfo(info, startPoint) };
            break;
        }
        default: {
            LERROR(fmt::format("Uknown camera path target type: {}", p.targetType));
            throw ghoul::MissingCaseException();
        }
    }

    // @TODO (emmbr) Allow for an instruction to represent a list of multiple waypoints
    const Waypoint waypointToAdd = waypoints[0];

    return Path(startPoint, waypointToAdd, type, duration);
}

} // namespace openspace::interaction
