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

#include <openspace/navigation/pathcurves/avoidcollisioncurve.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/navigation/waypoint.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/collisionhelper.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "AvoidCollisionCurve";

    constexpr const double CloseToNodeThresholdRadiusMultiplier = 5.0;
    constexpr const double AvoidCollisionDistanceRadiusMultiplier = 3.0;
    constexpr const double CollisionBufferSizeRadiusMultiplier = 1.0;
    constexpr const int MaxAvoidCollisionSteps = 10;

    constexpr const double Epsilon = 1e-5;
} // namespace

namespace openspace::interaction {

AvoidCollisionCurve::AvoidCollisionCurve(const Waypoint& start, const Waypoint& end) {
    _relevantNodes = global::navigationHandler->pathNavigator().relevantNodes();

    if (!start.node() || !end.node()) { // guard, but should never happen
        LERROR("Something went wrong. The start or end node does not exist");
        return;
    }

    const glm::dvec3 startNodeCenter = start.node()->worldPosition();
    const glm::dvec3 endNodeCenter = end.node()->worldPosition();
    const double startNodeRadius = start.validBoundingSphere();
    const double endNodeRadius = end.validBoundingSphere();
    const glm::dvec3 startViewDir = ghoul::viewDirection(start.rotation());

    // Add control points for a catmull-rom spline, first and last will not be intersected
    _points.push_back(start.position());
    _points.push_back(start.position());

    // Add an extra point to first go backwards if starting close to planet
    glm::dvec3 nodeToStart = start.position() - startNodeCenter;
    const double distanceToStartNode = glm::length(nodeToStart);

    // Note that the factor 2.0 is arbitrarily chosen to look ok.
    // @TODO: (2022-02-27, emmbr) Should be unified to a "getting close to object sphere"
    // that can be used in multiple cases when creating paths more cleverly later on
    const double closeToNodeThresholdFactor = glm::max(
        CloseToNodeThresholdRadiusMultiplier,
        2.0 * global::navigationHandler->pathNavigator().arrivalDistanceFactor()
    );

    if (distanceToStartNode < closeToNodeThresholdFactor * startNodeRadius) {
        const double distance = startNodeRadius;
        glm::dvec3 newPos = start.position() + distance * glm::normalize(nodeToStart);
        _points.push_back(newPos);
    }

    const glm::dvec3 startToEnd = end.position() - start.position();

    // Add point for moving out if the end state is far away and in opposite direction.
    // This helps with avoiding fast rotation in the center of the path
    const double maxRadius = std::max(startNodeRadius, endNodeRadius);
    bool nodesAreDifferent = start.nodeIdentifier() != end.nodeIdentifier();
    if (glm::length(startToEnd) >  0.5 * maxRadius && nodesAreDifferent) {
        double cosAngleToTarget = glm::dot(normalize(-startViewDir), normalize(startToEnd));
        bool targetInOppositeDirection = cosAngleToTarget > 0.7;

        if (targetInOppositeDirection) {
            const glm::dquat midleRot = glm::slerp(start.rotation(), end.rotation(), 0.5);
            const glm::dvec3 middleViewDir = ghoul::viewDirection(midleRot);
            const double stepOutDistance = 0.4 * glm::length(startToEnd);

            glm::dvec3 newPos = start.position() + 0.2 * startToEnd -
                stepOutDistance * glm::normalize(middleViewDir);

            _points.push_back(newPos);
        }
    }

    // Add an extra point to approach target
    const glm::dvec3 nodeToEnd = end.position() - endNodeCenter;
    const double distanceToEndNode = glm::length(nodeToEnd);

    if (distanceToEndNode < closeToNodeThresholdFactor * endNodeRadius) {
        const double distance = endNodeRadius;
        glm::dvec3 newPos = end.position() + distance * glm::normalize(nodeToEnd);
        _points.push_back(newPos);
    }

    _points.push_back(end.position());
    _points.push_back(end.position());

    // Create extra points to avoid collision
    removeCollisions();

    initializeParameterData();
}

// Try to reduce the risk of collision by approximating the curve with linear segments.
// If a collision happens, create a new point for the path to go through, in an attempt to
// avoid that collision
void AvoidCollisionCurve::removeCollisions(int step) {
    if (step > MaxAvoidCollisionSteps) {
        return;
    }

    const int nSegments = static_cast<int>(_points.size() - 3);
    for (int i = 0; i < nSegments; ++i) {
        const glm::dvec3 lineStart = _points[i + 1];
        const glm::dvec3 lineEnd = _points[i + 2];

        if (glm::distance(lineEnd, lineStart) - Epsilon < 0.0) {
            continue; // Start and end position are the same. Go to next segment
        }

        for (SceneGraphNode* node : _relevantNodes) {
            // Do collision check in relative coordinates, to avoid huge numbers
            const glm::dmat4 modelTransform = node->modelTransform();
            glm::dvec3 p1 = glm::inverse(modelTransform) * glm::dvec4(lineStart, 1.0);
            glm::dvec3 p2 = glm::inverse(modelTransform) * glm::dvec4(lineEnd, 1.0);

            // Sphere to check for collision
            double radius = node->boundingSphere();
            glm::dvec3 center = glm::dvec3(0.0, 0.0, 0.0);

            // Add a buffer to avoid passing too close to the node.
            // Dont't add it if any point is inside the buffer
            double buffer = CollisionBufferSizeRadiusMultiplier * radius;
            bool p1IsInside = collision::isPointInsideSphere(p1, center, radius + buffer);
            bool p2IsInside = collision::isPointInsideSphere(p2, center, radius + buffer);
            if (!p1IsInside && !p2IsInside) {
                radius += buffer;
            }

            glm::dvec3 intersectionPointModelCoords;
            bool collision = collision::lineSphereIntersection(
                p1,
                p2,
                center,
                radius,
                intersectionPointModelCoords
            );

            if (!collision) {
                continue;
            }

            glm::dvec3 collisionPoint = modelTransform *
                glm::dvec4(intersectionPointModelCoords, 1.0);

            // before collision response, make sure none of the points are inside the node
            bool isStartInsideNode = collision::isPointInsideSphere(p1, center, radius);
            bool isEndInsideNode = collision::isPointInsideSphere(p2, center, radius);
            if (isStartInsideNode || isEndInsideNode) {
                LWARNING(fmt::format(
                    "Something went wrong! "
                    "At least one point in the path is inside node: {}",
                    node->identifier()
                ));
                break;
            }

            // To avoid collision, take a step in an orhtogonal direction of the
            // collision point and add a new point
            const glm::dvec3 lineDirection = glm::normalize(lineEnd - lineStart);
            const glm::dvec3 nodeCenter = node->worldPosition();
            const glm::dvec3 collisionPointToCenter = nodeCenter - collisionPoint;

            const glm::dvec3 parallell = glm::proj(collisionPointToCenter, lineDirection);
            const glm::dvec3 orthogonal = collisionPointToCenter - parallell;

            const double avoidCollisionDistance =
                AvoidCollisionDistanceRadiusMultiplier * radius;

            glm::dvec3 extraKnot = collisionPoint -
                avoidCollisionDistance * glm::normalize(orthogonal);

            _points.insert(_points.begin() + i + 2, extraKnot);

            step++;
            removeCollisions(step);
            break;
        }
    }
}

} // namespace openspace::interaction
