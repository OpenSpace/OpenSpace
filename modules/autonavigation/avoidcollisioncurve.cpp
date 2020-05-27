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

    const double CloseToNodeThresholdFactor = 5.0; 
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
    double endNodeRadius = end.nodeDetails.validBoundingSphere;
    glm::dvec3 startViewDirection = glm::normalize(
        start.rotation() * glm::dvec3(0.0, 0.0, -1.0)
    );

    // Add control points for a catmull-rom spline, first and last will not be intersected
    _points.push_back(start.position());
    _points.push_back(start.position());

    // Add an extra point to first go backwards if starting close to planet
    glm::dvec3 nodeToStart = start.position() - startNodeCenter;
    double distanceToStartNode = glm::length(nodeToStart);

    if (distanceToStartNode < CloseToNodeThresholdFactor * startNodeRadius) {
        double distance = startNodeRadius;
        glm::dvec3 newPos = start.position() + distance * glm::normalize(nodeToStart);
        _points.push_back(newPos);
    }

    // Add point for moving out if the end state is in opposite direction
    //TODO: determine if its best to compare to end orientation or position 
    glm::dvec3 startToEnd = end.position() - start.position();
    double cosAngleToTarget = glm::dot(normalize(-startViewDirection), normalize(startToEnd));
    bool targetInOppositeDirection = cosAngleToTarget > 0.7;

    // TODO: reduce magical numbers / create constants
    if (targetInOppositeDirection) {
        glm::dquat midleRotation = glm::slerp(start.rotation(), end.rotation(), 0.5); 
        glm::dvec3 middleViewDirection = midleRotation * glm::dvec3(0.0, 0.0, -1.0);
        double stepOutdistance = 0.4 * glm::length(startToEnd);

        glm::dvec3 newPosition = start.position() + 0.2 * startToEnd - 
                                 stepOutdistance * glm::normalize(middleViewDirection);

        _points.push_back(newPosition);
    }

    // Add an extra point to approach target 
    glm::dvec3 nodeToEnd = end.position() - endNodeCenter;
    double distanceToEndNode = glm::length(nodeToEnd);

    if (distanceToEndNode < CloseToNodeThresholdFactor * endNodeRadius) {
        double distance = endNodeRadius;
        glm::dvec3 newPos = end.position() + distance * glm::normalize(nodeToEnd);
        _points.push_back(newPos);
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

    std::vector<double>::iterator segmentEndIt =
        std::lower_bound(_parameterIntervals.begin(), _parameterIntervals.end(), u);
    unsigned int index = (segmentEndIt - 1) - _parameterIntervals.begin();

    double segmentStart = _parameterIntervals[index];
    double segmentDuration = (_parameterIntervals[index + 1] - _parameterIntervals[index]);
    double uSegment = (u - segmentStart) / segmentDuration;

    return interpolation::catmullRom(
        uSegment, 
        _points[index], 
        _points[index + 1], 
        _points[index + 2], 
        _points[index + 3], 
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
           
            glm::dvec3 intersectionPointModelCoords;
            bool collision = helpers::lineSphereIntersection(
                p1, 
                p2, 
                center, 
                radius, 
                intersectionPointModelCoords
            );

            if (collision) {
                //LINFO(fmt::format("Collision with node: {}!", node->identifier()));
                glm::dvec3 collisionPoint = modelTransform *
                    glm::dvec4(intersectionPointModelCoords, 1.0);

                // before collision response, make sure none of the points are inside the node
                bool isStartInsideNode = helpers::isPointInsideSphere(p1, center, radius);
                bool isEndInsideNode = helpers::isPointInsideSphere(p2, center, radius);
                if (isStartInsideNode || isEndInsideNode) {
                    LWARNING(fmt::format(
                        "Something went wrong! "
                        "At least one point in the path is inside node: {}",
                        node->identifier()
                    ));
                    break;
                }

                // to avoid collision, take a step in an orhtogonal direction of the 
                // collision point and add a new point
                glm::dvec3 lineDirection = glm::normalize(lineEnd - lineStart);
                glm::dvec3 nodeCenter = node->worldPosition();
                glm::dvec3 collisionPointToCenter = nodeCenter - collisionPoint;
                glm::dvec3 parallell = glm::proj(collisionPointToCenter, lineDirection);
                glm::dvec3 orthogonal = collisionPointToCenter - parallell;

                double avoidCollisionDistance = AvoidCollisionDistanceFactor * radius;
                glm::dvec3 extraKnot = collisionPoint -
                    avoidCollisionDistance * glm::normalize(orthogonal);
                _points.insert(_points.begin() + i + 2, extraKnot);

                removeCollisions(++step);
                break;
            }
        }
    }
}

} // namespace openspace::autonavigation
