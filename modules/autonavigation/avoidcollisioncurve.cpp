/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2019                                                               *
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

#include <modules/autonavigation/avoidcollisioncurve.h>

#include <modules/autonavigation/autonavigationmodule.h>
#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/waypoint.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "AvoidCollisionCurve";
    const double Epsilon = 1E-7;

    const double CloseToNodeThresholdFactor = 3.0; 
    const double AvoidCollisionDistanceFactor = 3.0;
    const double CollisionBufferSizeFactor = 1.0;
    const int MaxAvoidCollisionSteps = 10;

} // namespace

namespace openspace::autonavigation {

AvoidCollisionCurve::AvoidCollisionCurve(const Waypoint& start, const Waypoint& end) {
    AutoNavigationModule* module = global::moduleEngine.module<AutoNavigationModule>();
    _relevantNodes = module->AutoNavigationHandler().relevantNodes();

    glm::dvec3 startNodeCenter = start.node()->worldPosition();
    glm::dvec3 endNodeCenter = end.node()->worldPosition();
    double startNodeRadius = start.nodeDetails.validBoundingSphere;
    glm::dvec3 startViewDirection = glm::normalize(
        start.rotation() * glm::dvec3(0.0, 0.0, -1.0)
    );

    // Add control points for a catmull-rom spline, first and last will not be intersected
    _points.push_back(start.position());
    _points.push_back(start.position());

    // Add an extra point to first go backwards if starting close to planet
    glm::dvec3 nodeCenterToStart = start.position() - startNodeCenter;
    double distanceToStartNode = glm::length(nodeCenterToStart);
    if (distanceToStartNode < CloseToNodeThresholdFactor * startNodeRadius) {
        double distance = startNodeRadius;
        glm::dvec3 newPos = start.position() + distance * glm::normalize(nodeCenterToStart);
        _points.push_back(newPos);
    }

    // Add point for moving out if the end state is in opposite direction
    //TODO: determine if its best to compare to end orientation or position 
    glm::dvec3 startToEnd = end.position() - start.position();
    double cosStartAngle = glm::dot(normalize(-startViewDirection), normalize(startToEnd));

    // TODO: reduce magical numbers / create constants
    bool targetInOppositeDirection = cosStartAngle > 0.7;
    if (targetInOppositeDirection) {
        glm::dquat middleRotation = glm::slerp(start.rotation(), end.rotation(), 0.5); 
        glm::dvec3 middleViewDir = glm::normalize(middleRotation * glm::dvec3(0.0, 0.0, -1.0));
        double distance = 0.4 * glm::length(startToEnd);

        glm::dvec3 newPosition = start.position() + 0.2 * startToEnd - distance * middleViewDir;
        _points.push_back(newPosition);
    }

    _points.push_back(end.position());
    _points.push_back(end.position());

    // Create extra points to avoid collision 
    removeCollisions();

    _nrSegments = _points.size() - 3;

    initParameterIntervals();
}

// Interpolate a list of control points and knot times
glm::dvec3 AvoidCollisionCurve::interpolate(double u) {
    if (u < Epsilon)
        return _points[1];
    if (u > (1.0 - Epsilon))
        return *(_points.end() - 2);

    // compute current segment, by first finding iterator to the first value that is larger than t 
    std::vector<double>::iterator segmentEndIt =
        std::lower_bound(_parameterIntervals.begin(), _parameterIntervals.end(), u);
    unsigned int idx = (segmentEndIt - 1) - _parameterIntervals.begin();

    double segmentStart = _parameterIntervals[idx];
    double segmentDuration = (_parameterIntervals[idx + 1] - _parameterIntervals[idx]);
    double uSegment = (u - segmentStart) / segmentDuration;

    return interpolation::catmullRom(
        uSegment, 
        _points[idx], 
        _points[idx + 1], 
        _points[idx + 2], 
        _points[idx + 3], 
        1.0
    );
}

// Try to reduce the risk of collision by approximating the curve with linear segments. 
// If a collision happens, create a new point for the path to go through, in an attempt to
// avoid that collision
void AvoidCollisionCurve::removeCollisions(int step) {
    int nrSegments = _points.size() - 3;

    if (step > MaxAvoidCollisionSteps) 
        return; // TODO: handle better / present warning if early out

    // go through all segments and check for collisions
    for (int i = 0; i < nrSegments; ++i) {
        const glm::dvec3 lineStart = _points[i + 1];
        const glm::dvec3 lineEnd = _points[i + 2];

        for (SceneGraphNode* node : _relevantNodes) {
            // do collision check in relative coordinates, to avoid huge numbers 
            const glm::dmat4 modelTransform = node->modelTransform();
            glm::dvec3 p1 = glm::inverse(modelTransform) * glm::dvec4(lineStart, 1.0);
            glm::dvec3 p2 = glm::inverse(modelTransform) * glm::dvec4(lineEnd, 1.0);

            // sphere to check for collision
            double radius = node->boundingSphere();
            glm::dvec3 center = glm::dvec3(0.0, 0.0, 0.0);

            // add a buffer, so we don't pass too close to the node
            // dont't add it if any point is inside the buffer
            double buffer = CollisionBufferSizeFactor * node->boundingSphere();
            bool p1IsInside = helpers::isPointInsideSphere(p1, center, radius + buffer);
            bool p2IsInside = helpers::isPointInsideSphere(p2, center, radius + buffer);
            if (!p1IsInside && !p2IsInside) {
                radius += buffer;
            }
           
            glm::dvec3 intersectionPoint;
            bool collision = helpers::lineSphereIntersection(p1, p2, center, radius, intersectionPoint);
            glm::dvec3 intersectionPointWorld = modelTransform * glm::dvec4(intersectionPoint, 1.0);

            if (collision) {
                LINFO(fmt::format("Collision with node: {}!", node->identifier())); // TODO: remove

                // before we response to the collision, make sure none of the points are inside the node
                bool isStartInsideNode = helpers::isPointInsideSphere(p1, center, radius);
                bool isEndInsideNode = helpers::isPointInsideSphere(p2, center, radius);
                if (isStartInsideNode || isEndInsideNode) {
                    LWARNING(fmt::format(
                        "Something went wrong! At least one point in the path is inside node: {}", 
                        node->identifier()
                    ));
                    break;
                }

                // to avoid collision, take a step in an orhtogonal direction of the 
                // collision point and add a new point
                glm::dvec3 lineDirection = glm::normalize(lineEnd - lineStart);
                glm::dvec3 nodeCenter = node->worldPosition();
                glm::dvec3 collisionPointToCenter = nodeCenter - intersectionPointWorld;
                glm::dvec3 parallell = glm::proj(collisionPointToCenter, lineDirection);
                glm::dvec3 orthogonal = collisionPointToCenter - parallell;

                double avoidCollisionDistance = AvoidCollisionDistanceFactor * radius;
                glm::dvec3 extraKnot = intersectionPointWorld - avoidCollisionDistance * glm::normalize(orthogonal);
                _points.insert(_points.begin() + i + 2, extraKnot);

                removeCollisions(++step);
                break;
            }
        }
    }
}

} // namespace openspace::autonavigation
