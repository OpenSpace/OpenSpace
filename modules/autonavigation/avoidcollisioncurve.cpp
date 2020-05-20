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

#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/waypoint.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "AvoidCollisionCurve";
    const double Epsilon = 1E-7;

    // TODO: move this list somewhere else
    const std::vector<std::string> RelevantTags{
        "planet_solarSystem",
        "moon_solarSystem"
        // TODO
    };

} // namespace

namespace openspace::autonavigation {

AvoidCollisionCurve::AvoidCollisionCurve(const Waypoint& start, const Waypoint& end) {
    double thresholdFactor = 3.0;

    glm::dvec3 startNodePos = start.node()->worldPosition();
    glm::dvec3 endNodePos = end.node()->worldPosition();
    double startNodeRadius = start.nodeDetails.validBoundingSphere;
    glm::dvec3 startViewDir = glm::normalize(start.rotation() * glm::dvec3(0.0, 0.0, -1.0));

    // Add control points for a catmull-rom spline, first and last will no be intersected
    // TODO: test other first and last control points, ex positive or negative view dir
    _points.push_back(start.position());
    _points.push_back(start.position());

    // Add an extra point to first go backwards if starting close to planet
    double distanceToStartNode = glm::length(start.position() - startNodePos);
    if (distanceToStartNode < thresholdFactor * startNodeRadius) {
        double distance = startNodeRadius;

        glm::dvec3 newPos = start.position() - distance * startViewDir;
        _points.push_back(newPos);
    }

    // Add point for moving out if the end state is in opposite direction
    //TODO: determine if its best to compare to end orientation or position 
    glm::dvec3 startToEnd = end.position() - start.position();
    double cosStartAngle = glm::dot(normalize(-startViewDir), normalize(startToEnd));
    if (cosStartAngle > 0.7) {
        glm::dquat middleRotation = glm::slerp(start.rotation(), end.rotation(), 0.5); // OBS! Rotation method is not necessarily slerp
        glm::dvec3 middleViewDir = glm::normalize(middleRotation * glm::dvec3(0.0, 0.0, -1.0));
        double distance = 0.4 * glm::length(startToEnd);

        glm::dvec3 newPos = start.position() + 0.2 * startToEnd - distance * middleViewDir;
        _points.push_back(newPos);
    }

    // TODO: Add a point to approach straigt towards a specific pose near planet
    // TODO: Calculate nice end pose if not defined

    _points.push_back(end.position());
    _points.push_back(end.position());

    std::vector<SceneGraphNode*> relevantNodes = findRelevantNodes();

    // Create extra points to avoid collision
    removeCollisions(relevantNodes);

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

std::vector<SceneGraphNode*> AvoidCollisionCurve::findRelevantNodes() {
    // TODO: make more efficient
    const std::vector<SceneGraphNode*>& allNodes =
        global::renderEngine.scene()->allSceneGraphNodes();

    auto isRelevant = [](const SceneGraphNode* node) {
        const std::vector<std::string> tags = node->tags();
        auto result = std::find_first_of(RelevantTags.begin(), RelevantTags.end(), tags.begin(), tags.end());

        // does not match any tags => not interesting
        if (result == RelevantTags.end()) {
            return false;
        }

        return node->renderable() && (node->boundingSphere() > 0.0);
    };

    std::vector<SceneGraphNode*> result{};
    std::copy_if(allNodes.begin(), allNodes.end(), std::back_inserter(result), isRelevant);

    return result;
}

void AvoidCollisionCurve::removeCollisions(std::vector<SceneGraphNode*>& relevantNodes, int step) {
    int nrSegments = _points.size() - 3;
    int maxSteps = 10;

    // TODO: handle better / present warning if early out
    if (step > maxSteps) return;

    // go through all segments and check for collisions
    for (int i = 0; i < nrSegments; ++i) {
        const glm::dvec3 lineStart = _points[i + 1];
        const glm::dvec3 lineEnd = _points[i + 2];

        for (SceneGraphNode* node : relevantNodes) {
            // do collision check in relative coordinates, to avoid huge numbers 
            const glm::dmat4 modelTransform = node->modelTransform();
            glm::dvec3 p1 = glm::inverse(modelTransform) * glm::dvec4(lineStart, 1.0);
            glm::dvec3 p2 = glm::inverse(modelTransform) * glm::dvec4(lineEnd, 1.0);

            // sphere to check for collision
            double bs = node->boundingSphere();
            double radius = bs; 
            // TODO: add a buffer (i.e. use a little larger sphere), when we have 
            // behaviour for leaving a target. Don't want to pass too close to objects
            glm::dvec3 center = glm::dvec3(0.0, 0.0, 0.0);
            glm::dvec3 intersectionPoint;

            bool collision = helpers::lineSphereIntersection(p1, p2, center, radius, intersectionPoint);

            // convert back to world coordinates 
            glm::dvec3 intersectionPointWorld = modelTransform * glm::dvec4(intersectionPoint, 1.0);

            if (collision) {
                LINFO(fmt::format("Collision with node: {}!", node->identifier()));

                // to avoid collision, take a step in an orhtogonal direction of the 
                // collision point and add a new point
                glm::dvec3 lineDir = glm::normalize(lineEnd - lineStart);
                glm::dvec3 nodePos = node->worldPosition();
                glm::dvec3 collisionPointToCenter = nodePos - intersectionPointWorld;
                glm::dvec3 parallell = glm::proj(collisionPointToCenter, lineDir);
                glm::dvec3 orthogonal = collisionPointToCenter - parallell;

                double avoidCollisionDistance = 3.0 * radius;
                glm::dvec3 extraKnot = intersectionPointWorld - avoidCollisionDistance * glm::normalize(orthogonal);
                _points.insert(_points.begin() + i + 2, extraKnot);

                // TODO: maybe make this more efficient, and make sure that we have a base case. 
                removeCollisions(relevantNodes, ++step);
                break;
            }
        }
    }
}

} // namespace openspace::autonavigation
