/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/navigation/pathcurve.h>

#include <openspace/navigation/waypoint.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/integration.h>
#include <ghoul/misc/interpolator.h>
#include <glm/gtx/projection.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "PathCurve";
    constexpr const int NrSamplesPerSegment = 100;
} // namespace

namespace openspace::interaction {

PathCurve::~PathCurve() {}

double PathCurve::length() const {
    return _totalLength;
}

glm::dvec3 PathCurve::positionAt(double relativeDistance) const {
    const double u = curveParameter(relativeDistance * _totalLength);
    return interpolate(u);
}

std::vector<glm::dvec3> PathCurve::points() const {
    return _points;
}

void PathCurve::initializeParameterData() {
    _nSegments = static_cast<int>(_points.size() - 3);

    ghoul_assert(_nSegments > 0, "Cannot have a curve with zero segments!");

    _curveParameterSteps.clear();
    _lengthSums.clear();
    _parameterSamples.clear();

    // Evenly space out parameter intervals
    _curveParameterSteps.reserve(_nSegments + 1);
    const double dt = 1.0 / _nSegments;
    _curveParameterSteps.push_back(0.0);
    for (unsigned int i = 1; i < _nSegments; i++) {
        _curveParameterSteps.push_back(dt * i);
    }
    _curveParameterSteps.push_back(1.0);

    // Arc lengths
    _lengthSums.reserve(_nSegments + 1);
    _lengthSums.push_back(0.0);
    for (unsigned int i = 1; i <= _nSegments; i++) {
        double u = _curveParameterSteps[i];
        double uPrev = _curveParameterSteps[i - 1];
        double length = arcLength(uPrev, u);
        _lengthSums.push_back(_lengthSums[i - 1] + length);
    }
    _totalLength = _lengthSums.back();

    // Compute a map of arc lengths s and curve parameters u, for reparameterization
    _parameterSamples.reserve(NrSamplesPerSegment * _nSegments + 1);
    const double uStep = 1.0 / (_nSegments * NrSamplesPerSegment);
    for (unsigned int i = 0; i < _nSegments; i++) {
        double uStart = _curveParameterSteps[i];
        double sStart = _lengthSums[i];
        for (int j = 0; j < NrSamplesPerSegment; ++j) {
            double u = uStart + j * uStep;
            double s = sStart + arcLength(uStart, u);
            _parameterSamples.push_back({ u, s });
        }
    }
    _parameterSamples.push_back({ 1.0, _totalLength });
}

// Compute the curve parameter from an arc length value, using a combination of
// Newton's method and bisection. Source:
// https://www.geometrictools.com/Documentation/MovingAlongCurveSpecifiedSpeed.pdf
// Input s is a length value, in the range [0, _totalLength]
// Returns curve parameter in range [0, 1]
double PathCurve::curveParameter(double s) const {
    if (s <= 0.0) return 0.0;
    if (s >= _totalLength) return 1.0;

    unsigned int segmentIndex = 1;
    while (s > _lengthSums[segmentIndex]) {
        segmentIndex++;
    }

    const int startIndex = (segmentIndex - 1) * NrSamplesPerSegment;
    const int endIndex = segmentIndex * NrSamplesPerSegment + 1;

    const double segmentS = s - _lengthSums[segmentIndex - 1];
    const double uMin = _curveParameterSteps[segmentIndex - 1];
    const double uMax = _curveParameterSteps[segmentIndex];

    // Use samples to find an initial guess for Newton's method
    // Find first sample with s larger than input s
    auto sampleIterator = std::upper_bound(
        _parameterSamples.begin() + startIndex,
        _parameterSamples.begin() + endIndex,
        ParameterPair{ 0.0 , s }, // 0.0 is a dummy value for u
        [](const ParameterPair& lhs,const ParameterPair& rhs) {
            return lhs.s < rhs.s;
        }
    );

    const ParameterPair& sample = *sampleIterator;
    const ParameterPair& prevSample = *(sampleIterator - 1);
    const double uPrev = prevSample.u;
    const double sPrev = prevSample.s;
    const double slope = (sample.u - uPrev) / (sample.s - sPrev);
    double u = uPrev + slope * (s - sPrev);

    constexpr const int maxIterations = 50;

    // Initialize root bounding limits for bisection
    double lower = uMin;
    double upper = uMax;

    for (int i = 0; i < maxIterations; ++i) {
        double F = arcLength(uMin, u) - segmentS;

        // The error we tolerate, in meters. Note that distances are very large
        constexpr const double tolerance = 0.005;
        if (std::abs(F) <= tolerance) {
            return u;
        }

        // Generate a candidate for Newton's method
        double dfdu = approximatedDerivative(u); // > 0
        double uCandidate = u - F / dfdu;

        // Update root-bounding interval and test candidate
        if (F > 0) {  // => candidate < u <= upper
            upper = u;
            u = (uCandidate <= lower) ? (upper + lower) / 2.0 : uCandidate;
        }
        else { // F < 0 => lower <= u < candidate
            lower = u;
            u = (uCandidate >= upper) ? (upper + lower) / 2.0 : uCandidate;
        }
    }

    // No root was found based on the number of iterations and tolerance. However, it is
    // safe to report the last computed u value, since it is within the segment interval
    return u;
}

double PathCurve::approximatedDerivative(double u, double h) const {
    if (u <= h) {
        return (1.0 / h) * glm::length(interpolate(0.0 + h) - interpolate(0.0));
    }
    if (u >= 1.0 - h) {
        return (1.0 / h) * glm::length(interpolate(1.0) - interpolate(1.0 - h));
    }
    return (0.5 / h) * glm::length(interpolate(u + h) - interpolate(u - h));
}

double PathCurve::arcLength(double limit) const {
    return arcLength(0.0, limit);
}

double PathCurve::arcLength(double lowerLimit, double upperLimit) const {
    return ghoul::integrateGaussianQuadrature<double>(
        lowerLimit,
        upperLimit,
        [this](double u) { return approximatedDerivative(u); }
    );
}

glm::dvec3 PathCurve::interpolate(double u) const {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable must be in range [0,1]");

    if (u < 0.0) {
        return _points[1];
    }
    if (u > 1.0) {
        return *(_points.end() - 2);
    }

    std::vector<double>::const_iterator segmentEndIt =
        std::lower_bound(_curveParameterSteps.begin(), _curveParameterSteps.end(), u);

    const int index =
        static_cast<int>((segmentEndIt - 1) - _curveParameterSteps.begin());

    double segmentStart = _curveParameterSteps[index];
    double segmentDuration = (_curveParameterSteps[index + 1] - segmentStart);
    double uSegment = (u - segmentStart) / segmentDuration;

    return ghoul::interpolateCatmullRom(
        uSegment,
        _points[index],
        _points[index + 1],
        _points[index + 2],
        _points[index + 3],
        1.0 // chordal version
    );
}

LinearCurve::LinearCurve(const Waypoint& start, const Waypoint& end) {
    _points.push_back(start.position());
    _points.push_back(start.position());
    _points.push_back(end.position());
    _points.push_back(end.position());
    initializeParameterData();
}

} // namespace openspace::interaction
