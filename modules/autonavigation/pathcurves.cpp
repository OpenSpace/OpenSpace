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

#include <modules/autonavigation/pathcurves.h>

#include <modules/autonavigation/helperfunctions.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>

namespace {
constexpr const char* _loggerCat = "PathCurve";
} // namespace

namespace openspace::autonavigation {

PathCurve::~PathCurve() {}

// Approximate the curve length by dividing the curve into smaller linear 
// segments and accumulate their length
double PathCurve::arcLength(double tLimit) {
    double dt = 0.01; // TODO: choose a good dt
    double sum = 0.0;
    for (double t = 0.0; t <= tLimit - dt; t += dt) {
        double ds = glm::length(valueAt(t + dt) - valueAt(t));
        sum += ds;
    }
    return sum;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> PathCurve::getPoints() {
    return _points;
}

Bezier3Curve::Bezier3Curve(CameraState& start, CameraState& end) {
    glm::dvec3 startNodePos = sceneGraphNode(start.referenceNode)->worldPosition();
    glm::dvec3 endNodePos = sceneGraphNode(end.referenceNode)->worldPosition();
    double startNodeRadius = sceneGraphNode(start.referenceNode)->boundingSphere();
    double endNodeRadius = sceneGraphNode(end.referenceNode)->boundingSphere();

    glm::dvec3 startNodeToStartPos = start.position - startNodePos;
    glm::dvec3 endNodeToEndPos = end.position - endNodePos;

    double startTangentLength = 2.0 * startNodeRadius;
    double endTangentLength = 2.0 * endNodeRadius;
    glm::dvec3 startTangentDirection = normalize(startNodeToStartPos);
    glm::dvec3 endTangentDirection = normalize(endNodeToEndPos);

    // Start by going outwards
    _points.push_back(start.position);
    _points.push_back(start.position + startTangentLength * startTangentDirection);

    if (start.referenceNode != end.referenceNode) {

        glm::dvec3 startNodeToEndNode = endNodePos - startNodePos;
        glm::dvec3 startToEndDirection = normalize(end.position - start.position);

        // Assuming we move straigh out to point to a distance proportional to radius, angle is enough to check collision risk
        double cosStartAngle = glm::dot(startTangentDirection, startToEndDirection);
        double cosEndAngle = glm::dot(endTangentDirection, startToEndDirection);
        double cosStartDir = glm::dot(startTangentDirection, endTangentDirection);

        //TODO: investigate suitable values, could be risky close to surface..
        bool TARGET_BEHIND_STARTNODE = cosStartAngle < -0.8;
        bool TARGET_BEHIND_ENDNODE = cosEndAngle > 0.8;
        bool TARGET_IN_OPPOSITE_DIRECTION = cosStartAngle > 0.7;

        // Avoid collision with startnode by adding control points on the side of it
        if (TARGET_BEHIND_STARTNODE) {
            glm::dvec3 parallell = glm::proj(startNodeToStartPos, startNodeToEndNode);
            glm::dvec3 orthogonal = normalize(startNodeToStartPos - parallell);
            double dist = 5.0 * startNodeRadius;
            glm::dvec3 extraKnot = startNodePos + dist * orthogonal;

            _points.push_back(extraKnot + parallell);
            _points.push_back(extraKnot);
            _points.push_back(extraKnot - parallell);
        }

        // Zoom out, to get a better understanding in a 180 degree turn situation
        if (TARGET_IN_OPPOSITE_DIRECTION) {
            glm::dvec3 parallell = glm::proj(startNodeToStartPos, startNodeToEndNode);
            glm::dvec3 orthogonal = normalize(startNodeToStartPos - parallell);
            double dist = 0.5 * length(startNodeToEndNode);
            // Distant middle point
            glm::dvec3 extraKnot = startNodePos + dist * normalize(parallell) + 3.0 * dist * orthogonal;

            _points.push_back(extraKnot - 0.3 * dist *  normalize(parallell));
            _points.push_back(extraKnot);
            _points.push_back(extraKnot + 0.3 * dist *  normalize(parallell));
        }

        // Avoid collision with endnode by adding control points on the side of it
        if (TARGET_BEHIND_ENDNODE) {
            glm::dvec3 parallell = glm::proj(endNodeToEndPos, startNodeToEndNode);
            glm::dvec3 orthogonal = normalize(endNodeToEndPos - parallell);
            double dist = 5.0 * endNodeRadius;
            glm::dvec3 extraKnot = endNodePos + dist * orthogonal;

            _points.push_back(extraKnot - parallell);
            _points.push_back(extraKnot);
            _points.push_back(extraKnot + parallell);
        }
    }

    _points.push_back(end.position + endTangentLength * endTangentDirection);
    _points.push_back(end.position);

  
    // initial interval times
    int nrIntervals = (int)std::floor((_points.size() - 1) / 3.0); //TODO valivate!

    for (double time = 0.0; time <= 1.0; time += 1.0 / nrIntervals) {
        _intervalTimes.push_back(time);
    }
    // TODO: test that time == 1 is included
       
    reparameterizeByArcLength();
}

// Interpolate a list of control points and knot times
glm::dvec3 Bezier3Curve::valueAt(double t) {
    size_t nrPoints = _points.size();
    size_t nrTimes = _intervalTimes.size();
    unsigned int nrSegments = (unsigned int)std::floor((nrPoints - 1) / 3.0);

    ghoul_assert(nrPoints > 4, "Minimum of four control points needed for interpolation!");
    ghoul_assert((nrPoints - 1) % 3 == 0, "A vector containing 3n + 1 control points must be provided!");
    ghoul_assert(nrSegments == (nrTimes - 1), "Number of interval times must match number of intervals");

    if (abs(t) < 0.000001)
        return _points.front();

    // compute current segment, by first finding iterator to the first time that is larger than t 
    std::vector<double>::iterator segmentEndIt = 
        std::lower_bound(_intervalTimes.begin(), _intervalTimes.end(), t);
    unsigned int segmentIdx = (segmentEndIt - 1) - _intervalTimes.begin();

    double segmentStart = _intervalTimes[segmentIdx];
    double segmentDuration = (_intervalTimes[segmentIdx + 1] - _intervalTimes[segmentIdx]);
    double tSegment = (t - segmentStart) / segmentDuration;

    unsigned int idx = segmentIdx * 3;

    return interpolation::cubicBezier(tSegment, _points[idx], _points[idx + 1],
        _points[idx + 2], _points[idx + 3]);
}

void Bezier3Curve::reparameterizeByArcLength() {
    // For bezier every third is a knot shared beetween segments
    int nrIntervals = (int)std::floor((float)(_points.size() - 1) / 3.0); //TODO valivate!

    if (nrIntervals == 1) {
        return;
    }

    double totalLength = arcLength(1.0); //THIS IS WHERE IT GOES INTO INTERPOLATOR WITHOUT ANY VALUES IN TIMES VECTOR IF >! SEGMENTS OTHERWISE IN PATHSEGMENT::INITCURVE
    std::vector<double> newTimes;

    // Save time as relative curve length from start to each knot
    for ( double time : _intervalTimes) {
        newTimes.push_back(arcLength(time) / totalLength);
    }

    _intervalTimes.swap(newTimes);
}

LinearCurve::LinearCurve(CameraState& start, CameraState& end) {
    _points.push_back(start.position);
    _points.push_back(end.position);
}

glm::dvec3 LinearCurve::valueAt(double t) {
    return interpolation::linear(t, _points[0], _points[1]);
}

// TODO: Iprove handling of pauses
PauseCurve::PauseCurve(CameraState& state) {
    _points.push_back(state.position);
}

glm::dvec3 PauseCurve::valueAt(double t) {
    return _points[0];
}

} // namespace openspace::autonavigation
