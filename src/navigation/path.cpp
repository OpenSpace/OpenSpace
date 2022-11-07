/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/universalhelpers.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>
#include <glm/ext/quaternion_relational.hpp>

namespace {
    constexpr std::string_view _loggerCat = "Path";
    constexpr float LengthEpsilon = 1e-5f;

    constexpr const char* SunIdentifier = "Sun";

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

        // (Node): The target node of the camera path. Not optional for 'Node'
        // instructions
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

        enum class [[codegen::map(openspace::interaction::Path::Type)]] PathType {
            AvoidCollision,
            ZoomOutOverview,
            Linear,
            AvoidCollisionWithLookAt
        };
        // The type of the created path. Affects the shape of the resulting path
        std::optional<PathType> pathType;
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
            LERROR("Could not create curve. Type does not exist");
            throw ghoul::MissingCaseException();
    }

    _prevPose = _start.pose();

    // Compute speed factor to match any given duration, by traversing the path and
    // computing how much faster/slower it should be
    _speedFactorFromDuration = 1.0;
    if (duration.has_value()) {
        constexpr double dt = 0.05; // 20 fps
        while (!hasReachedEnd()) {
            traversePath(dt);
        }

        // We now know how long it took to traverse the path. Use that
        _speedFactorFromDuration = _progressedTime / *duration;
        resetPlaybackVariables();
    }
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
    double displacement = dt * speed;

    const double prevDistance = _traveledDistance;

    _progressedTime += dt;
    _traveledDistance += displacement;

    CameraPose newPose;

    if (_type == Type::Linear) {
        // Special handling of linear paths, so that it can be used when we are
        // traversing very large distances without introducing precision problems
        newPose = linearInterpolatedPose(_traveledDistance, displacement);
    }
    else {
        if (std::abs(prevDistance - _traveledDistance) < LengthEpsilon) {
            // The distaces are too large, so we are not making progress because of
            // insufficient precision
            _shouldQuit = true;
            LWARNING("Quit camera path prematurely due to insufficient precision");
        }

        newPose = interpolatedPose(_traveledDistance);
    }

    _prevPose = newPose;
    return newPose;
}

void Path::quitPath() {
    _traveledDistance = pathLength();
    _shouldQuit = true;
}

std::string Path::currentAnchor() const {
    bool pastHalfway = (_traveledDistance / pathLength()) > 0.5;
    return (pastHalfway) ? _end.nodeIdentifier() : _start.nodeIdentifier();
}

bool Path::hasReachedEnd() const {
    if (_shouldQuit) {
        return true;
    }

    // @TODO (emmbr, 2022-11-07) Handle linear paths separately, as they might 
    // abort prematurely due to the "isPositionFinished" condition

    bool isPositionFinished = (_traveledDistance / pathLength()) >= 1.0;

    constexpr double RotationEpsilon = 0.0001;
    bool isRotationFinished = ghoul::isSameOrientation(
        _prevPose.rotation,
        _end.rotation(),
        RotationEpsilon
    );

    return isPositionFinished && isRotationFinished;
}

void Path::resetPlaybackVariables() {
    _prevPose = _start.pose();
    _traveledDistance = 0.0;
    _progressedTime = 0.0;
    _shouldQuit = false;
}

CameraPose Path::linearInterpolatedPose(double distance, double displacement) {
    ghoul_assert(_type == Type::Linear, "Path type must be linear");
    const double relativeDistance = distance / pathLength();
    const glm::dvec3 prevPosToEnd = _prevPose.position - _end.position();
    const double remainingDistance = glm::length(prevPosToEnd);
    CameraPose pose;

    // Actual displacement may not be bigger than remaining distance
    if (displacement > remainingDistance) {
        _traveledDistance = pathLength();
        pose.position = _end.position();
    }
    else {
        // Just move along line from the current position to the target
        const glm::dvec3 lineDir = glm::normalize(prevPosToEnd);
        pose.position = _prevPose.position - displacement * lineDir;
    }

    pose.rotation = linearPathRotation(relativeDistance);
    return pose;
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
            return easedSlerpRotation(t);
        case Type::Linear:
            // @TODO (2022-03-29, emmbr) Fix so that rendering the rotation of linear
            // paths works again. I.e. openspace.debugging.renderCameraPath
            return linearPathRotation(t);
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

glm::dquat Path::linearPathRotation(double t) const {
    const glm::dvec3 a = ghoul::viewDirection(_start.rotation());
    const glm::dvec3 b = ghoul::viewDirection(_end.rotation());
    const double angle = std::acos(glm::dot(a, b)); // assumes length 1.0 for a & b

    // Seconds per pi angles. Per default, it takes 5 seconds to turn 90 degrees
    double factor = 5.0 / glm::half_pi<double>();
    factor *= global::navigationHandler->pathNavigator().linearRotationSpeedFactor();

    double turnDuration = std::max(angle * factor, 1.0); // Always at least 1 second
    const double time = glm::clamp(_progressedTime / turnDuration, 0.0, 1.0);
    return easedSlerpRotation(time);

    // @TODO (2022-03-18, emmbr) Leaving this for now, as something similar might have to
    // be implemented for navigation states. But should be removed/reimplemented

    //const glm::dvec3 endNodePos = _end.node()->worldPosition();
    //const glm::dvec3 endUp = _end.rotation() * glm::dvec3(0.0, 1.0, 0.0);

    //const double tHalf = 0.5;
    //if (t < tHalf) {
    //    // Interpolate to look at target
    //    const glm::dvec3 halfWayPosition = _curve->positionAt(tHalf);
    //    const glm::dquat q = ghoul::lookAtQuaternion(halfWayPosition, endNodePos, endUp);

    //    const double tScaled = ghoul::sineEaseInOut(t / tHalf);
    //    return glm::slerp(_start.rotation(), q, tScaled);
    //}

    //// This distance is guaranteed to be strictly decreasing for linear paths
    //const double distanceToEnd = glm::distance(_prevPose.position, _end.position());

    //// Determine the distance at which to start interpolating to the target rotation.
    //// The magic numbers here are just randomly picked constants, set to make the
    //// resulting rotation look ok-ish
    //double closingUpDistance = 10.0 * _end.validBoundingSphere();
    //if (pathLength() < 2.0 * closingUpDistance) {
    //    closingUpDistance = 0.2 * pathLength();
    //}

    //if (distanceToEnd < closingUpDistance) {
    //    // Interpolate to target rotation
    //    const double tScaled = ghoul::sineEaseInOut(1.0 - distanceToEnd / closingUpDistance);

    //    // Compute a position in front of the camera at the end orientation
    //    const double inFrontDistance = glm::distance(_end.position(), endNodePos);
    //    const glm::dvec3 viewDir = ghoul::viewDirection(_end.rotation());
    //    const glm::dvec3 inFrontOfEnd = _end.position() + inFrontDistance * viewDir;
    //    const glm::dvec3 lookAtPos = ghoul::interpolateLinear(tScaled, endNodePos, inFrontOfEnd);
    //    return ghoul::lookAtQuaternion(_prevPose.position, lookAtPos, endUp);
    //}

    //// Keep looking at the end node
    //return ghoul::lookAtQuaternion(_prevPose.position, endNodePos, endUp);
}

glm::dquat Path::lookAtTargetsRotation(double t) const {
    t = glm::clamp(t, 0.0, 1.0);
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

        const double tScaled = glm::clamp(t / t1, 0.0, 1.0);
        const double tEased = ghoul::cubicEaseInOut(tScaled);
        lookAtPos =
            ghoul::interpolateLinear(tEased, inFrontOfStart, startNodePos);
    }
    else if (t <= t2) {
        const double tScaled = glm::clamp((t - t1) / (t2 - t1), 0.0, 1.0);
        const double tEased = ghoul::cubicEaseInOut(tScaled);
        lookAtPos = ghoul::interpolateLinear(tEased, startNodePos, endNodePos);
    }
    else {
        // (t > t2)
        // Compute a position in front of the camera at the end orientation
        const double inFrontDistance = glm::distance(endPos, endNodePos);
        const glm::dvec3 viewDir = ghoul::viewDirection(_end.rotation());
        const glm::dvec3 inFrontOfEnd = endPos + inFrontDistance * viewDir;

        const double tScaled = glm::clamp((t - t2) / (1.0 - t2), 0.0, 1.0);
        const double tEased = ghoul::cubicEaseInOut(tScaled);
        lookAtPos = ghoul::interpolateLinear(tEased, endNodePos, inFrontOfEnd);
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

    // Set speed based on distance to closest node
    const double distanceToEndNode = glm::distance(_prevPose.position, endNodePos);
    const double distanceToStartNode = glm::distance(_prevPose.position, startNodePos);
    bool isCloserToEnd = distanceToEndNode < distanceToStartNode;

    const glm::dvec3 closestPos = isCloserToEnd ? endNodePos : startNodePos;
    const double distanceToClosestNode = glm::distance(closestPos, _prevPose.position);

    const double speed = distanceToClosestNode;

    // Dampen at the start and end
    constexpr double DampenDistanceFactor = 3.0;
    double startUpDistance = DampenDistanceFactor * _start.validBoundingSphere();
    double closeUpDistance = DampenDistanceFactor * _end.validBoundingSphere();

    // Kind of an ugly workaround to make damping behave over very long paths, and/or
    // where the target nodes might have large bounding spheres. The current max is set
    // based on the order of magnitude of the solar system, which ofc is very specific to
    // our space content...
    // @TODO (2022-03-22, emmbr) Come up with a better more general solution
    constexpr double MaxDistance = 1E12;
    startUpDistance = glm::min(MaxDistance, startUpDistance);
    closeUpDistance = glm::min(MaxDistance, closeUpDistance);

    if (pathLength() < startUpDistance + closeUpDistance) {
        startUpDistance = 0.4 * pathLength(); // a little less than half
        closeUpDistance = startUpDistance;
    }

    double dampeningFactor = 1.0;
    if (traveledDistance < startUpDistance) {
        dampeningFactor = traveledDistance / startUpDistance;
    }
    else if (_type == Type::Linear) {
        // Dampen at end of linear path is handled separately, as we can use the
        // current position to scompute the remaining distance rather than the
        // path length minus travels distance. This is more suitable for long paths
        const double remainingDistance = glm::distance(
            _prevPose.position,
            _end.position()
        );
        if (remainingDistance < closeUpDistance) {
            dampeningFactor = remainingDistance / closeUpDistance;
        }
    }
    else if (traveledDistance > (pathLength() - closeUpDistance)) {
        const double remainingDistance = pathLength() - traveledDistance;
        dampeningFactor = remainingDistance / closeUpDistance;
    }
    dampeningFactor = glm::clamp(dampeningFactor, 0.0, 1.0);
    dampeningFactor = ghoul::sineEaseOut(dampeningFactor);

    // Prevent multiplying with 0 (and hence a speed of 0.0 => no movement)
    dampeningFactor += 0.01;

    // TODO: also dampen speed based on curvature, or make sure the curve has a rounder
    //       shape

    return _speedFactorFromDuration * speed * dampeningFactor;
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

struct NodeInfo {
    std::string identifier;
    std::optional<glm::dvec3> position;
    std::optional<double> height;
    bool useTargetUpDirection;
};

Waypoint computeWaypointFromNodeInfo(const NodeInfo& info, const Waypoint& startPoint,
                                     Path::Type type)
{
    const SceneGraphNode* targetNode = sceneGraphNode(info.identifier);
    if (!targetNode) {
        LERROR(fmt::format("Could not find target node '{}'", info.identifier));
        return Waypoint();
    }

    glm::dvec3 stepDir;
    glm::dvec3 targetPos;
    if (info.position.has_value()) {
        // The position in instruction is given in the targetNode's local coordinates.
        // Convert to world coordinates
        targetPos = glm::dvec3(
            targetNode->modelTransform() * glm::dvec4(*info.position, 1.0)
        );
    }
    else {
        const PathNavigator& navigator = global::navigationHandler->pathNavigator();

        const double radius = navigator.findValidBoundingSphere(targetNode);
        const double defaultHeight = radius * navigator.arrivalDistanceFactor();
        const double height = info.height.value_or(defaultHeight);
        const double distanceFromNodeCenter = radius + height;

        if (type == Path::Type::Linear) {
            // If linear path, compute position along line form start to end point
            glm::dvec3 endNodePos = targetNode->worldPosition();
            stepDir = glm::normalize(startPoint.position() - endNodePos);
        }
        else {
            stepDir = computeGoodStepDirection(targetNode, startPoint);
        }

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
    glm::dvec3 lookAtPos = targetNode->worldPosition();

    // Check if we can distinguish between targetpos and lookAt pos. Otherwise, move it further away
    const glm::dvec3 diff = targetPos - lookAtPos;
    double distSquared = glm::dot(diff, diff);
    if (std::isnan(distSquared) || distSquared < LengthEpsilon) {
        double startToEndDist = glm::length(
            startPoint.position() - targetNode->worldPosition()
        );
        lookAtPos = targetPos - stepDir * 0.1 * startToEndDist;
    }

    const glm::dquat targetRot = ghoul::lookAtQuaternion(targetPos, lookAtPos, up);

    return Waypoint(targetPos, targetRot, info.identifier);
}

void checkVisibilityAndShowMessage(const SceneGraphNode* node) {
    auto isEnabled = [](const Renderable* r) {
        std::any propertyValueAny = r->property("Enabled")->get();
        return std::any_cast<bool>(propertyValueAny);
    };

    // Show some info related to the visiblity of the object
    const Renderable* renderable = node->renderable();
    if (!renderable) {
        // Check if any of the children are visible, if it has children
        bool foundVisible = false;
        for (const SceneGraphNode* child : node->children()) {
            const Renderable* cr = child->renderable();
            if (cr && isEnabled(cr)) {
                foundVisible = true;
                break;
            }
        }

        if (!foundVisible) {
            LINFO("Creating path to a node without a renderable or visible child nodes");
        }
    }
    else if (!isEnabled(renderable)) {
        LINFO("Creating path to disabled object");
    }
}

Path createPathFromDictionary(const ghoul::Dictionary& dictionary,
                              std::optional<Path::Type> forceType)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    const std::optional<float> duration = p.duration;

    Path::Type type;
    if (forceType.has_value()) {
        type = *forceType;
    }
    else if (p.pathType.has_value()) {
        type = codegen::map<Path::Type>(*p.pathType);
    }
    else {
        type = global::navigationHandler->pathNavigator().defaultPathType();
    }

    bool hasStart = p.startState.has_value();
    const Waypoint startPoint = hasStart ?
        Waypoint(NavigationState(p.startState.value())) :
        waypointFromCamera();

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

            double startToTargetCenterDistance = glm::distance(
                startPoint.position(),
                targetNode->worldPosition()
            );

            // Use a linear path if camera start is within the bounding sphere
            const PathNavigator& navigator = global::navigationHandler->pathNavigator();
            const double bs = navigator.findValidBoundingSphere(targetNode);
            bool withinTargetBoundingSphere = startToTargetCenterDistance < bs;
            if (withinTargetBoundingSphere) {
                LINFO(
                    "Camera is within the bounding sphere of the target node. "
                    "Using linear path"
                );
                type = Path::Type::Linear;
            }

            waypoints = { computeWaypointFromNodeInfo(info, startPoint, type) };
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }

    // @TODO (emmbr) Allow for an instruction to represent a list of multiple waypoints
    const Waypoint waypointToAdd = waypoints[0];

    if (glm::distance(startPoint.position(), waypointToAdd.position()) < LengthEpsilon) {
        LINFO("Already at the requested target");
        throw PathCurve::TooShortPathError("Path too short");
    }

    checkVisibilityAndShowMessage(waypointToAdd.node());

    try {
        return Path(startPoint, waypointToAdd, type, duration);
    }
    catch (const PathCurve::TooShortPathError&) {
        LINFO("Already at the requested target");
        // Rethrow e, so the pathnavigator can handle it as well
        throw;
    }
    catch (const PathCurve::InsufficientPrecisionError&) {
        // There wasn't enough precision to represent the full curve in world
        // coordinates. For now, use a linear path instead. It uses another
        // method of interpolation that isn't as sensitive to these kinds of problems

        LINFO(
            "Switching to a linear path, to avoid problems with precision due to "
            "immense path length"
        );

        return createPathFromDictionary(dictionary, Path::Type::Linear);

        // @TODO (emmbr26, 2022-02-15): later on we want to improve this case, so that
        // interpolation is not a problem. One suggestion to solve this would be using
        // two identical paths and switch the direction of interpolation halfway through.
        // That should give us sufficient precision at both ends of the path
    }
}

} // namespace openspace::interaction
