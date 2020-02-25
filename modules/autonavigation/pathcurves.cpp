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
    const double Epsilon = 1E-7;
} // namespace

namespace openspace::autonavigation {

PathCurve::~PathCurve() {}

const double PathCurve::length() const {
    return _length;
}

// Approximate the curve length using Simpson's rule
double PathCurve::arcLength(double limit) {
    const int n = 30; // resolution, must be even for Simpson's rule
    const double h = limit / (double)n;

    // Points to be multiplied by 1
    double endPoints = glm::length(valueAt(0.0 + h) - valueAt(0.0)) + glm::length(valueAt(1.0) - valueAt(1.0 - h));

    // Points to be multiplied by 4
    double times4 = 0.0;
    for (int i = 1; i < n; i += 2) {
        float t = h * i;
        times4 += glm::length(valueAt(t + h) - valueAt(t));
    }

    // Points to be multiplied by 2
    double times2 = 0.0;
    for (int i = 2; i < n; i += 2) {
        float t = h * i;
        times2 += glm::length(valueAt(t + h) - valueAt(t));
    }

    return (h / 3.0) * (endPoints + 4.0 * times4 + 2.0 *times2);
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
            double dist = 0.5 * glm::length(startNodeToEndNode);
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

    _nrSegments = (unsigned int)std::floor((_points.size() - 1) / 3.0);

    // default values for the curve parameter - equally spaced
    for (double t = 0.0; t <= 1.0; t += 1.0 / _nrSegments) {
        _parameterIntervals.push_back(t);
    }

    _length = arcLength(1.0);

    initParameterIntervals();
}

// Interpolate a list of control points and knot times
glm::dvec3 Bezier3Curve::valueAt(double u) {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable out of range [0, 1]");

    size_t nrPoints = _points.size();
    size_t nrTimes = _parameterIntervals.size();

    ghoul_assert(nrPoints > 4, "Minimum of four control points needed for interpolation!");
    ghoul_assert((nrPoints - 1) % 3 == 0, "A vector containing 3n + 1 control points must be provided!");
    ghoul_assert(_nrSegments == (nrTimes - 1), "Number of interval times must match number of intervals");

    if (abs(u) < Epsilon)
        return _points.front();

    if (abs(1.0 - u) < Epsilon)
        return _points.back();

    // compute current segment, by first finding iterator to the first value that is larger than s 
    std::vector<double>::iterator segmentEndIt = 
        std::lower_bound(_parameterIntervals.begin(), _parameterIntervals.end(), u);
    unsigned int segmentIdx = (segmentEndIt - 1) - _parameterIntervals.begin();

    double segmentStart = _parameterIntervals[segmentIdx];
    double segmentDuration = (_parameterIntervals[segmentIdx + 1] - _parameterIntervals[segmentIdx]);
    double sScaled = (u - segmentStart) / segmentDuration;

    unsigned int idx = segmentIdx * 3;

    // Interpolate using De Casteljau's algorithm
    return interpolation::cubicBezier(sScaled, _points[idx], _points[idx + 1],
        _points[idx + 2], _points[idx + 3]);
}

// compute curve parameter intervals based on relative arc length
void Bezier3Curve::initParameterIntervals() {
    std::vector<double> newIntervals;
    for (double t = 0.0; t <= 1.0; t += 1.0 / _nrSegments) {
        newIntervals.push_back(arcLength(t) / _length);
    }
    _parameterIntervals.swap(newIntervals);
}

LinearCurve::LinearCurve(CameraState& start, CameraState& end) {
    _points.push_back(start.position);
    _points.push_back(end.position);
    _length = glm::distance(end.position, start.position);
}

glm::dvec3 LinearCurve::valueAt(double u) {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable out of range [0, 1]");
    return interpolation::linear(u, _points[0], _points[1]);
}

// TODO: Iprove handling of pauses
PauseCurve::PauseCurve(CameraState& state) {
    _points.push_back(state.position);
    _length = 1.0; // OBS! Length of a pause curve makes no sense, but it also doesn't matter
}

glm::dvec3 PauseCurve::valueAt(double u) {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable out of range [0, 1]");
    return _points[0];
}

} // namespace openspace::autonavigation
