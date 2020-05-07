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

glm::dvec3 PathCurve::positionAt(double relativeLength) {
    double u = approximatedCurveParameter(relativeLength * _length); // TODO: only use relative length
    //LINFO(fmt::format("Curve parameter: u = {}", u));
    return interpolate(u);
}

double PathCurve::approximatedCurveParameter(double s) {
    if (s <= Epsilon) return 0.0;
    if (s >= _length) return 1.0;

    std::map<double, double>::iterator itSample = _parameterPairs.upper_bound(s);
    std::map<double, double>::iterator itPrevSample = std::prev(itSample, 1);

    double sSample = itSample->first;
    double sPrevSample = itPrevSample->first;
    double uSample = itSample->second;
    double uPrevSample = itPrevSample->second;

    double slope = (uSample - uPrevSample) / (sSample - sPrevSample);

    // linear fit (TODO: investigate higher order fits?)
    return uPrevSample + slope * (s - sPrevSample);
}

// Input s is a length value, in the range [0, _length]
// Returns curve parameter in range [0, 1]
double PathCurve::curveParameter(double s) {
    if (s <= Epsilon) return 0.0;
    if (s >= _length) return 1.0; 

    unsigned int segmentIndex;
    for (segmentIndex = 1; segmentIndex < _nrSegments; ++segmentIndex) {
        if (s <= _lengthSums[segmentIndex]) 
            break;
    }

    // initial guess for Newton's method
    double segmentS = s - _lengthSums[segmentIndex - 1];
    double segmentLength = _lengths[segmentIndex];

    const double uMin = _parameterIntervals[segmentIndex - 1];
    const double uMax = _parameterIntervals[segmentIndex];
    double u = uMin + (uMax - uMin) * (segmentS / segmentLength);

    const int nrIterations = 10; 

    // initialize root boudning limits for bisection
    double lower = uMin;
    double upper = uMax;

    for (int i = 0; i < nrIterations; ++i) {
        double F = arcLength(uMin, u) - segmentS; 

        const double tolerance = 0.01 * Epsilon * _length;
        if (std::abs(F) <= tolerance) {
            return u;
        }

        // generate a candidate for Newton's method        
        double dfdu = approximatedDerivative(u, Epsilon); // > 0                            
        double uCandidate = u - F / dfdu;

        // Update root-bounding interval and test candidate
        if (F > 0) {  // => candidate < u <= upper
            upper = u;
            if (uCandidate <= lower) {
                // the candidate is outside [lower, upper] => use bisection 
                u = (upper + lower) / 2.0;
            }
            else {
                u = uCandidate;
            }
        }
        else { // F < 0 => lower <= u < candidate 
            lower = u;
            if (uCandidate >= upper) {
                // the candidate is outside [lower, upper] => use bisection 
                u = (upper + lower) / 2.0;
            }
            else {
                u = uCandidate;
            }
        }
    }

    // TODO: write meaningful comment about how no root was found in the number of iterations
    return u;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> PathCurve::getPoints() {
    return _points;
}

// TODO: move to base constructor
void PathCurve::initParameterIntervals() {
    _parameterIntervals.clear();
    _parameterIntervals.reserve(_nrSegments + 1);

    double delta = 1.0 / _nrSegments;

    _parameterIntervals.push_back(0.0);
    for (unsigned int i = 1; i < _nrSegments; i++) {
        _parameterIntervals.push_back(delta * i);
    }
    _parameterIntervals.push_back(1.0);
}

void PathCurve::initLengths() {
    _lengths.clear();
    _lengths.reserve(_nrSegments + 1);
    _lengthSums.clear();
    _lengthSums.reserve(_nrSegments + 1);

    _lengths.push_back(0.0);
    _lengthSums.push_back(0.0);
    for (unsigned int i = 1; i <= _nrSegments; i++) {
        double u = _parameterIntervals[i];
        double uPrev = _parameterIntervals[i - 1];
        _lengths.push_back(arcLength(uPrev, u)); // OBS! Is this length computed well enough?
        _lengthSums.push_back(_lengthSums[i - 1] + _lengths[i]);
    }
    _length = _lengthSums.back();

    // Scale parameterIntervals to better match arc lengths
    for (unsigned int i = 1; i <= _nrSegments; i++) {
        _parameterIntervals[i] = _lengthSums[i] / _length;
    }

    // Prepare samples for arc length reparameterization
    _parameterPairs.clear();
    _parameterPairs[0.0] = 0.0;
    double s = 0.0;

    for (int i = 1; i <= _nrSegments; ++i) {
        double segmentLength = _lengths[i];
        // TODO: make nr of samples dependent on segment length somehow
        double deltaLength = _lengths[i] / _nrParameterSamplesPerSegment;
        double endLength = _lengthSums[i];
        double startLength = _lengthSums[i-1];
        s += deltaLength;
        while (s < endLength) {
            double sSegment = s - startLength;
            double sSegmentEased = segmentLength * ghoul::cubicEaseInOut(sSegment / segmentLength);
            double sEased = sSegmentEased + startLength;
            double u = curveParameter(sEased);
            _parameterPairs[sEased] = u;
            s += deltaLength;
        }
        _parameterPairs[_lengthSums[i]] = _parameterIntervals[i];
    }
    _parameterPairs[_length] = 1.0;

    //LINFO(fmt::format("number samples: {}", _parameterPairs.size()));
    //LINFO(fmt::format("next to last value: {}", std::prev(_parameterPairs.end(), 2)->second));
}

double PathCurve::approximatedDerivative(double u, double h) {
    if (u <= h) {
        return (1.0 / h) * glm::length(interpolate(0.0 + h) - interpolate(0.0));
    }
    if (u >= 1.0 - h) {
        return (1.0 / h) * glm::length(interpolate(1.0) - interpolate(1.0 - h));
    }
    return (0.5 / h) * glm::length(interpolate(u + h) - interpolate(u - h));
}

double PathCurve::arcLength(double limit) {
    return arcLength(0.0, limit);
}

// Approximate the curve length using Simpson's rule
double PathCurve::arcLength(double lowerLimit, double upperLimit) {
    const int n = 50; // resolution, must be even for Simpson's rule
    const double h = (upperLimit - lowerLimit) / (double)n;

    auto arcLengthFunction = [&](double u, double h) {
        const double stepScale = 0.001; // for higher precision in derivative
        double d = approximatedDerivative(u, stepScale * h);
        long double derivativeSquared = d * d;

        // OBS! Sqrt is expensive! And the derivatives are really large, so might be 
        // sufficient with just the derivative 
        return std::sqrt(1 + derivativeSquared);
    };

    // Points to be multiplied by 1
    double endPoints = arcLengthFunction(lowerLimit, h) + arcLengthFunction(upperLimit, h);

    // Points to be multiplied by 4
    double times4 = 0.0;
    for (int i = 1; i < n; i += 2) {
        double u = lowerLimit + h * i;
        times4 += arcLengthFunction(u, h);
    }

    // Points to be multiplied by 2
    double times2 = 0.0;
    for (int i = 2; i < n; i += 2) {
        double u = lowerLimit + h * i;
        times2 += arcLengthFunction(u, h);
    }

    return (h / 3.0) * (endPoints + 4.0 * times4 + 2.0 * times2);
}

Bezier3Curve::Bezier3Curve(const Waypoint& start, const Waypoint& end) {
    glm::dvec3 startNodePos = start.node()->worldPosition();
    glm::dvec3 endNodePos = end.node()->worldPosition();

    double startNodeRadius = start.nodeDetails.validBoundingSphere;
    double endNodeRadius = end.nodeDetails.validBoundingSphere;

    glm::dvec3 startNodeToStartPos = start.position() - startNodePos;
    glm::dvec3 endNodeToEndPos = end.position() - endNodePos;

    double startTangentLength = 2.0 * startNodeRadius;
    double endTangentLength = 2.0 * endNodeRadius;
    glm::dvec3 startTangentDirection = normalize(startNodeToStartPos);
    glm::dvec3 endTangentDirection = normalize(endNodeToEndPos);

    // Start by going outwards
    _points.push_back(start.position());
    _points.push_back(start.position() + startTangentLength * startTangentDirection);

    const std::string& startNode = start.nodeDetails.identifier;
    const std::string& endNode = end.nodeDetails.identifier;

    if (startNode != endNode) {

        glm::dvec3 startNodeToEndNode = endNodePos - startNodePos;
        glm::dvec3 startToEndDirection = normalize(end.position() - start.position());

        // Assuming we move straigh out to point to a distance proportional to radius, angle is enough to check collision risk
        double cosStartAngle = glm::dot(startTangentDirection, startToEndDirection);
        double cosEndAngle = glm::dot(endTangentDirection, startToEndDirection);

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

    _points.push_back(end.position() + endTangentLength * endTangentDirection);
    _points.push_back(end.position());

    _nrSegments = (unsigned int)std::floor((_points.size() - 1) / 3.0);

   initParameterIntervals();
   initLengths();
}

// Interpolate a list of control points and knot times
glm::dvec3 Bezier3Curve::interpolate(double u) {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable out of range [0, 1]");
    ghoul_assert(_points.size() > 4, "Minimum of four control points needed for interpolation!");
    ghoul_assert((_points.size() - 1) % 3 == 0, "A vector containing 3n + 1 control points must be provided!");
    ghoul_assert(_nrSegments == (_parameterIntervals.size() - 1), "Number of interval times must match number of intervals");

    if (u <= Epsilon)
        return _points.front();

    if (u >= 1.0 - Epsilon)
        return _points.back();

    // compute current segment, by first finding iterator to the first value that is larger than s 
    std::vector<double>::iterator segmentEndIt = 
        std::lower_bound(_parameterIntervals.begin(), _parameterIntervals.end(), u);
    unsigned int segmentIdx = (unsigned int)((segmentEndIt - 1) - _parameterIntervals.begin());

    double segmentStart = _parameterIntervals[segmentIdx];
    double segmentDuration = (_parameterIntervals[segmentIdx + 1] - segmentStart);
    double uScaled = (u - segmentStart) / segmentDuration;

    unsigned int idx = segmentIdx * 3;

    // Interpolate using De Casteljau's algorithm
    return interpolation::cubicBezier(uScaled, _points[idx], _points[idx + 1],
        _points[idx + 2], _points[idx + 3]);
}

LinearCurve::LinearCurve(const Waypoint& start, const Waypoint& end) {
    _points.push_back(start.position());
    _points.push_back(end.position());
    _length = glm::distance(end.position(), start.position());

    // TODO: new method
}

glm::dvec3 LinearCurve::interpolate(double u) {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable out of range [0, 1]");
    return interpolation::linear(u, _points[0], _points[1]);
}

} // namespace openspace::autonavigation
