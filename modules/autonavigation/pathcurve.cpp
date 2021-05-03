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

#include <modules/autonavigation/pathcurve.h>

#include <modules/autonavigation/helperfunctions.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "PathCurve";
    const double Epsilon = 1E-7;
} // namespace

namespace openspace::autonavigation {

PathCurve::~PathCurve() {}

const double PathCurve::length() const {
    return _totalLength;
}

glm::dvec3 PathCurve::positionAt(double relativeLength) {
    double u = curveParameter(relativeLength * _totalLength);
    return interpolate(u);
}

// Compute the curve parameter from an arc length value, using a combination of
// Newton's method and bisection. Source:
// https://www.geometrictools.com/Documentation/MovingAlongCurveSpecifiedSpeed.pdf
// Input s is a length value, in the range [0, _length]
// Returns curve parameter in range [0, 1]
double PathCurve::curveParameter(double s) {
    if (s <= Epsilon) return 0.0;
    if (s >= _totalLength) return 1.0;

    unsigned int segmentIndex;
    for (segmentIndex = 1; segmentIndex < _nSegments; ++segmentIndex) {
        if (s <= _lengthSums[segmentIndex]) {
            break;
        }
    }

    double segmentS = s - _lengthSums[segmentIndex - 1];
    double segmentLength = _lengths[segmentIndex];

    const double uMin = _parameterIntervals[segmentIndex - 1];
    const double uMax = _parameterIntervals[segmentIndex];

    // Compute curve parameter through linerar interpolation. This adds some variations 
    // in speed, especially in breakpoints between curve segments, but compared to the 
    // root bounding approach below the motion becomes much smoother. 
    // The root finding simply does not work well enough as of now.
    return uMin + (uMax - uMin) * (segmentS / segmentLength);


    // ROOT FINDING USING NEWTON'S METHOD BELOW

    //// Initialize root bounding limits for bisection
    //double lower = uMin;
    //double upper = uMax;
    //double u = uMin;

    //LINFO(fmt::format("Segment: {}", segmentIndex));
    //LINFO(fmt::format("Intitial guess u: {}", u));

    //// The function we want to find the root for
    //auto F = [this, segmentS, uMin](double u) -> double {
    //    return (arcLength(uMin, u) - segmentS);
    //};

    //// Start by doing a few bisections to find a good estimate 
    // (could potentially use linear guess as well)
    //int counter = 0;
    //while (upper - lower > 0.0001) {
    //    u = (upper + lower) / 2.0;

    //    if (F(u) * F(lower) < 0.0) {
    //        upper = u;
    //    }
    //    else {
    //        lower = u;
    //    }
    //    counter++;
    //}

    // OBS!! It actually seems like just using bisection returns a better, or at least as
    // good, result compared to Newton's method. Is this because of a problem with the 
    // derivative or arc length computation?

    //LINFO(fmt::format("Bisected u: {}", u));
    //LINFO(fmt::format("nBisections: {}", counter));

    //const int nIterations = 100;

    //for (int i = 0; i < nIterations; ++i) {
    //    double function = F(u);

    //    const double tolerance = 0.001; 
    //    if (std::abs(function) <= tolerance) {
    //        LINFO(fmt::format("nIterations: {}", i));
    //        return u;
    //    }

    //    // Generate a candidate for Newton's method
    //    double dfdu = approximatedDerivative(u, Epsilon); // > 0
    //    double uCandidate = u - function / dfdu;

    //    // Update root-bounding interval and test candidate
    //    if (function > 0) {  // => candidate < u <= upper
    //        upper = u;
    //        u = (uCandidate <= lower) ? (upper + lower) / 2.0 : uCandidate;
    //    }
    //    else { // F < 0 => lower <= u < candidate
    //        lower = u;
    //        u = (uCandidate >= upper) ? (upper + lower) / 2.0 : uCandidate;
    //    }
    //}

    ////LINFO(fmt::format("Max iterations! ({})", nIterations));

    //// No root was found based on the number of iterations and tolerance. However, it is
    //// safe to report the last computed u value, since it is within the segment interval
    //return u;
}

std::vector<glm::dvec3> PathCurve::points() {
    return _points;
}

void PathCurve::initParameterIntervals() {
    ghoul_assert(_nSegments > 0, "Cannot have a curve with zero segments!");
    _parameterIntervals.clear();
    _parameterIntervals.reserve(_nSegments + 1);

    // Space out parameter intervals
    double dt = 1.0 / _nSegments;
    _parameterIntervals.push_back(0.0);
    for (unsigned int i = 1; i < _nSegments; i++) {
        _parameterIntervals.push_back(dt * i);
    }
    _parameterIntervals.push_back(1.0);

    // Lengths
    _lengths.clear();
    _lengths.reserve(_nSegments + 1);
    _lengthSums.clear();
    _lengthSums.reserve(_nSegments + 1);

    _lengths.push_back(0.0);
    _lengthSums.push_back(0.0);
    for (unsigned int i = 1; i <= _nSegments; i++) {
        double u = _parameterIntervals[i];
        double uPrev = _parameterIntervals[i - 1];
        _lengths.push_back(arcLength(uPrev, u));
        _lengthSums.push_back(_lengthSums[i - 1] + _lengths[i]);
    }
    _totalLength = _lengthSums.back();
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

double PathCurve::arcLength(double lowerLimit, double upperLimit) {
    return helpers::fivePointGaussianQuadrature(
        lowerLimit,
        upperLimit,
        [this](double u) { return approximatedDerivative(u); }
    );
}

LinearCurve::LinearCurve(const Waypoint& start, const Waypoint& end) {
    _points.push_back(start.position());
    _points.push_back(end.position());
    _nSegments = 1;
    initParameterIntervals();
}

glm::dvec3 LinearCurve::interpolate(double u) {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable out of range [0, 1]");
    return interpolation::linear(u, _points[0], _points[1]);
}

} // namespace openspace::autonavigation
