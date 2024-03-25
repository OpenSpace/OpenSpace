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

    // A PathInstruction is a table describing the specification for a camera path.
    // It is used as an input to the `openspace.pathnavigation.createPath` function.
    //
    // There are two types of paths that can be created, as specified by the required
    // TargetType parameter: 'Node' or 'NavigationState'. The difference is what kind
    // of target the path is created for, a scene graph node or a specific navigation
    // state for the camera.
    //
    // Depending on the type, the parameters that can be specified are a bit different.
    // A 'NavigationState' already contains all details for the camera position, so no
    // other details may be specified. For a 'Node' instruction, only a 'Target' node is
    // required, but a 'Height' or 'Position' may also be specified. If both a position
    // and height is specified, the height value will be ignored.
    //
    // For 'Node' paths it is also possible to specify whether the target camera state
    // at the end of the flight should take the up direction of the target node into
    // account. Note that for this to give an effect on the path, rolling motions have
    // to be enabled.
    struct [[codegen::Dictionary(PathInstruction)]] Parameters {
        // The type of the instruction. Decides what other parameters are
        // handled/available
        enum class TargetType {
            Node,
            NavigationState
        };
        TargetType targetType;

        // The desired duration traversing the specified path segment should take
        std::optional<float> duration;

        // (Node): The target node of the camera path. Not optional for 'Node'
        // type instructions
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
        // of the resulting path
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

documentation::Documentation Path::Documentation() {
    return codegen::doc<Parameters>("core_path_instruction");
}

Path::Path(Waypoint start, Waypoint end, Type type, std::optional<float> duration)
    : _start(std::move(start))
    , _end(std::move(end))
    , _type(type)
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
    }

    _prevPose = _start.pose();

    // Estimate how long time the camera path will take to traverse
    constexpr double dt = 0.05; // 20 fps
    while (!hasReachedEnd()) {
        traversePath(dt);
    }
    float estimatedDuration = _progressedTime;
    resetPlaybackVariables();

    // We now know how long it took to traverse the path. Use that to compute the
    // speed factor to match any given duration
    _speedFactorFromDuration = 1.0;
    if (duration.has_value()) {
        if (*duration > 0.0) {
            _speedFactorFromDuration = estimatedDuration / *duration;
            estimatedDuration = *duration;
        }
        else {
            // A duration of zero means infinite speed. Handle this explicity
            _speedFactorFromDuration = std::numeric_limits<float>::infinity();
            estimatedDuration = 0.f;
        }
    }
    _expectedDuration = estimatedDuration;
}

Waypoint Path::startPoint() const {
    return _start;
}

Waypoint Path::endPoint() const {
    return _end;
}

double Path::pathLength() const {
    return _curve->length();
}

double Path::remainingDistance() const {
    return pathLength() - _traveledDistance;
}

float Path::estimatedRemainingTime(float speedScale) const {
    return _expectedDuration / speedScale - _progressedTime;
}

std::vector<glm::dvec3> Path::controlPoints() const {
    return _curve->points();
}

CameraPose Path::traversePath(double dt, float speedScale) {
    if (std::isinf(_speedFactorFromDuration)) {
        _shouldQuit = true;
        _prevPose = _start.pose();
        _traveledDistance = pathLength();
        return _end.pose();
    }

    double speed = speedAlongPath(_traveledDistance);
    speed *= static_cast<double>(speedScale);
    const double displacement = dt * speed;

    const double prevDistance = _traveledDistance;

    _progressedTime += static_cast<float>(dt);
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
    const bool pastHalfway = (_traveledDistance / pathLength()) > 0.5;
    return (pastHalfway) ? _end.nodeIdentifier() : _start.nodeIdentifier();
}

bool Path::hasReachedEnd() const {
    if (_shouldQuit) {
        return true;
    }

    // @TODO (emmbr, 2022-11-07) Handle linear paths separately, as they might
    // abort prematurely due to the "isPositionFinished" condition

    const bool isPositionFinished = (_traveledDistance / pathLength()) >= 1.0;

    constexpr double RotationEpsilon = 0.0001;
    const bool isRotationFinished = ghoul::isSameOrientation(
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

        const double newRemainingDistance = glm::length(pose.position - _end.position());
        const double diff = remainingDistance - newRemainingDistance;
        // Avoid remaining distances close to zero, or even negative
        if (relativeDistance > 0.5 && diff < LengthEpsilon) {
            // The positions are too large, so we are not making progress because of
            // insufficient precision
            LWARNING("Quit camera path prematurely due to insufficient precision");
            _shouldQuit = true;
            return _prevPose;
        }
    }

    pose.rotation = linearPathRotation(relativeDistance);

    if (glm::any(glm::isnan(pose.rotation)) || glm::any(glm::isnan(pose.position))) {
        // This should not happen, but guard for it anyways
        _shouldQuit = true;
        return _prevPose;
    }

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

glm::dquat Path::linearPathRotation(double) const {
    const glm::dvec3 a = ghoul::viewDirection(_start.rotation());
    const glm::dvec3 b = ghoul::viewDirection(_end.rotation());
    const double angle = std::acos(glm::dot(a, b)); // assumes length 1.0 for a & b

    // Seconds per pi angles. Per default, it takes 5 seconds to turn 90 degrees
    double factor = 5.0 / glm::half_pi<double>();
    factor *= global::navigationHandler->pathNavigator().linearRotationSpeedFactor();

    const double turnDuration = std::max(angle * factor, 1.0); // Always at least 1 second
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
    const double tUp = ghoul::sineEaseInOut(t);
    const glm::dvec3 startUp = _start.rotation() * glm::dvec3(0.0, 1.0, 0.0);
    const glm::dvec3 endUp = _end.rotation() * glm::dvec3(0.0, 1.0, 0.0);
    const glm::dvec3 up = ghoul::interpolateLinear(tUp, startUp, endUp);

    return ghoul::lookAtQuaternion(_curve->positionAt(t), lookAtPos, up);
}

double Path::speedAlongPath(double traveledDistance) const {
    const glm::dvec3 endNodePos = _end.node()->worldPosition();
    const glm::dvec3 startNodePos = _start.node()->worldPosition();

    // Set speed based on distance to closest node
    const double distanceToEndNode = glm::distance(_prevPose.position, endNodePos);
    const double distanceToStartNode = glm::distance(_prevPose.position, startNodePos);
    const bool isCloserToEnd = (distanceToEndNode < distanceToStartNode);

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

    Path::Type type = Path::Type::Linear;
    if (forceType.has_value()) {
        type = *forceType;
    }
    else if (p.pathType.has_value()) {
        type = codegen::map<Path::Type>(*p.pathType);
    }
    else {
        type = global::navigationHandler->pathNavigator().defaultPathType();
    }

    const bool hasStart = p.startState.has_value();
    const Waypoint startPoint = hasStart ?
        Waypoint(NavigationState(p.startState.value())) :
        interaction::waypointFromCamera();

    std::vector<Waypoint> waypoints;
    switch (p.targetType) {
        case Parameters::TargetType::NavigationState: {
            if (!p.navigationState.has_value()) {
                throw ghoul::RuntimeError("A navigation state is required");
            }

            const NavigationState navigationState =
                NavigationState(p.navigationState.value());

            const SceneGraphNode* targetNode = sceneGraphNode(navigationState.anchor);
            if (!targetNode) {
                throw ghoul::RuntimeError(std::format(
                    "Could not find anchor node '{}' in provided navigation state",
                    navigationState.anchor
                ));
            }

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
                throw ghoul::RuntimeError(std::format(
                    "Could not find target node '{}'", nodeIdentifier
                ));
            }

            const interaction::NodeCameraStateSpec info {
                nodeIdentifier,
                p.position,
                p.height,
                p.useTargetUpDirection.value_or(false)
            };

            const double startToTargetCenterDistance = glm::distance(
                startPoint.position(),
                targetNode->worldPosition()
            );

            // Use a linear path if camera start is within the bounding sphere
            const PathNavigator& navigator = global::navigationHandler->pathNavigator();
            const double bs = navigator.findValidBoundingSphere(targetNode);
            const bool withinTargetBoundingSphere = (startToTargetCenterDistance < bs);
            if (withinTargetBoundingSphere) {
                LINFO(
                    "Camera is within the bounding sphere of the target node. "
                    "Using linear path"
                );
                type = Path::Type::Linear;
            }

            const bool isLinear = type == Path::Type::Linear;
            waypoints = { computeWaypointFromNodeInfo(info, startPoint, isLinear) };
            break;
        }
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
            "immense path length or precision problems"
        );

        return createPathFromDictionary(dictionary, Path::Type::Linear);

        // @TODO (emmbr26, 2022-02-15): later on we want to improve this case, so that
        // interpolation is not a problem. One suggestion to solve this would be using
        // two identical paths and switch the direction of interpolation halfway through.
        // That should give us sufficient precision at both ends of the path
    }
}

} // namespace openspace::interaction
