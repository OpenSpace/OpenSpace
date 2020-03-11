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
    // default rotation interpolation
    _rotationInterpolator = RotationInterpolator{ start, end, this, Slerp };

    _points.push_back(start.position());

    // Add a point to first go straight out if starting close to planet
    double thresholdFactor = 3.0;
    glm::dvec3 startNodePos = start.node()->worldPosition();
    double startNodeRadius = start.nodeDetails.validBoundingSphere;
    glm::dvec3 startNodeToStartPos = start.position() - startNodePos;

    if (glm::length(startNodeToStartPos) < thresholdFactor * startNodeRadius) {
        glm::dvec3 viewDir = glm::normalize(start.rotation() * glm::dvec3(0.0, 0.0, -1.0));
        double distance = startNodeRadius;

        glm::dvec3 newPos = start.position() - distance * viewDir;
        _points.push_back(newPos);
    }

    // TODO: Add a point to approach straigt towards a specific pose near planet
    // TODO: Calculate nice end pose if not defined

    _points.push_back(end.position());

    std::vector<SceneGraphNode*> relevantNodes = findRelevantNodes();

    // Create extra points to avoid collision
    removeCollisions(relevantNodes);

    int nrSegments = _points.size() - 1;

    // default values for the curve parameter - equally spaced
    for (double t = 0.0; t <= 1.0; t += 1.0 / (double)nrSegments) {
        _parameterIntervals.push_back(t);
    }

    _length = arcLength(1.0);

    initParameterIntervals();
}

// Interpolate a list of control points and knot times
glm::dvec3 AvoidCollisionCurve::positionAt(double u) {
    return interpolatePoints(u);
}

std::vector<SceneGraphNode*> AvoidCollisionCurve::findRelevantNodes() {
    // TODO: make more efficient
    const std::vector<SceneGraphNode*>& allNodes =
        global::renderEngine.scene()->allSceneGraphNodes();

    auto isRelevant = [](const SceneGraphNode* node) {
        // does the node match any of the relevant tags?
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
    int nrSegments = _points.size() - 1;
    int maxSteps = 10;

    // TODO: handle better / present warning if early out
    if (step > maxSteps) return;

    // go through all segments and check for collisions
    for (int i = 0; i < nrSegments; ++i) {
        const glm::dvec3 lineStart = _points[i];
        const glm::dvec3 lineEnd = _points[i + 1];

        for (SceneGraphNode* node : relevantNodes) {
            // do collision check in relative coordinates, to avoid huge numbers 
            const glm::dmat4 modelTransform = node->modelTransform();
            glm::dvec3 p1 = glm::inverse(modelTransform) * glm::dvec4(lineStart, 1.0);
            glm::dvec3 p2 = glm::inverse(modelTransform) * glm::dvec4(lineEnd, 1.0);

            // sphere to check for collision
            double bs = node->boundingSphere();
            double radius = bs; 
            // TODO: add a buffer (i.e. sue a little larger sphere), when we have 
            // behaviour for leaving a target. Don't want to pass too close to objects
            glm::dvec3 center = glm::dvec3(0.0, 0.0, 0.0);
            glm::dvec3 point;

            bool collision = helpers::lineSphereIntersection(p1, p2, center, radius, point);

            // convert back to world coordinates 
            glm::dvec3 pointWorld = modelTransform * glm::dvec4(point, 1.0);

            if (collision) {
                LINFO(fmt::format("Collision with node: {}!", node->identifier()));

                // to avoid collision, take a step in an orhtogonal direction of the 
                // collision point and add a new point
                glm::dvec3 lineDir = glm::normalize(lineEnd - lineStart);
                glm::dvec3 nodePos = node->worldPosition();
                glm::dvec3 collisionPointToCenter = nodePos - pointWorld;
                glm::dvec3 parallell = glm::proj(collisionPointToCenter, lineDir);
                glm::dvec3 orthogonal = collisionPointToCenter - parallell;

                double distance = 3.0 * radius;
                glm::dvec3 extraKnot = pointWorld - distance * glm::normalize(orthogonal);
                _points.insert(_points.begin() + i + 1, extraKnot);

                // TODO: maybe make this more efficient, and make sure that we have a base case. 
                removeCollisions(relevantNodes, ++step);
                break;
            }
        }
    }
}

// compute curve parameter intervals based on relative arc length
void AvoidCollisionCurve::initParameterIntervals() {
    std::vector<double> newIntervals;
    int nrSegments = _points.size() - 1;
    for (double t = 0.0; t <= 1.0; t += 1.0 / (double)nrSegments) {
        newIntervals.push_back(arcLength(t) / _length);
    }
    _parameterIntervals.swap(newIntervals);
}

// Catmull-Rom inspired interpolation of the curve points
glm::dvec3 AvoidCollisionCurve::interpolatePoints(double u)
{
    ghoul_assert(_points.size() == _parameterIntervals.size(), 
        "Must have equal number of points and times!");
    ghoul_assert(_points.size() > 2, "Minimum of two control points needed for interpolation!");

    size_t nrSegments = _points.size() - 1;
    const glm::dvec3 start = _points.front();
    const glm::dvec3 end = _points.back();

    if (u <= 0.0) return _points.front();
    if (u >= 1.0) return _points.back();

    if (nrSegments == 1) {
        return roundedSpline(u, start, start, end, end);
    }

    // compute current segment index
    std::vector<double>::const_iterator segmentEndIt =
        std::lower_bound(_parameterIntervals.begin(), _parameterIntervals.end(), u);
    unsigned int idx = (segmentEndIt - 1) - _parameterIntervals.begin();

    double segmentStart = _parameterIntervals[idx];
    double segmentDuration = (_parameterIntervals[idx + 1] - _parameterIntervals[idx]);
    double tScaled = (u - segmentStart) / segmentDuration;

    glm::dvec3 first = (idx == 0) ? start : _points[idx - 1];
    glm::dvec3 last = (idx == (nrSegments - 1)) ? end : _points[idx + 2];

    return roundedSpline(tScaled, first, _points[idx], _points[idx + 1], last);
}

glm::dvec3 AvoidCollisionCurve::roundedSpline(double t, const glm::dvec3 &a, 
              const glm::dvec3 &b, const glm::dvec3 &c, const glm::dvec3 &d)
{
    ghoul_assert(t >= 0 && t <= 1.0, "Interpolation variable out of range [0, 1]");

    if (t <= 0.0) return b;
    if (t >= 1.0) return c;

    auto isNormalizable = [](const glm::dvec3 v) {
        return !(abs(glm::length(v)) < Epsilon);
    };

    // find velocities at b and c
    glm::dvec3 cb = c - b;
    glm::dvec3 bc = -cb;

    if (!isNormalizable(cb)) {
        return b;
    }

    glm::dvec3 ab = a - b;

    // a and b are the same point
    if (!isNormalizable(ab)) {
        ab = bc;
    }

    glm::dvec3 bVelocity = glm::normalize(cb) - glm::normalize(ab);
    bVelocity = isNormalizable(bVelocity) ?
        glm::normalize(bVelocity) : glm::dvec3(0.0, 1.0, 0.0);

    glm::dvec3 dc = d - c;

    // d and c are the same point
    if (!isNormalizable(dc)) {
        dc = cb;
    }

    glm::dvec3 cVelocity = glm::normalize(dc) - glm::normalize(bc);
    cVelocity = isNormalizable(cVelocity) ?
        glm::normalize(cVelocity) : glm::dvec3(0.0, 1.0, 0.0);

    double cbDistance = glm::length(cb);
    double tangetLength = cbDistance;

    // Distances in space can be extremely large, so we dont want the tangents to always have the full length. 
    const double tangentlengthThreshold = 1E10; // TODO: What's a good threshold?? ALSO make global
    if (tangetLength > tangentlengthThreshold)
        tangetLength = tangentlengthThreshold;

    // the resulting curve gets much better and more predictable when using a genric 
    // hermite curve compared to the Catmull Rom implementation
    return interpolation::hermite(t, b, c, bVelocity * tangetLength, cVelocity * tangetLength);

    //return interpolation::catmullRom(t, b - bVelocity * cbDistance, b, c, c + cVelocity * cbDistance);
}
} // namespace openspace::autonavigation
