/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
    constexpr double LengthEpsilon = 100.0 * std::numeric_limits<double>::epsilon();
} // namespace

namespace openspace::interaction {

PathCurve::InsufficientPrecisionError::InsufficientPrecisionError(std::string error)
    : ghoul::RuntimeError(std::move(error), "PathCurve")
{}

PathCurve::TooShortPathError::TooShortPathError(std::string error)
    : ghoul::RuntimeError(std::move(error), "PathCurve")
{}

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
    ghoul_assert(_nSegments > 0, "Cannot have a curve with zero segments");

    _curveParameterSteps.clear();
    _lengthSums.clear();
    _parameterSamples.clear();

    const double max = static_cast<double>(_nSegments);

    // Evenly space out parameter intervals
    _curveParameterSteps.reserve(_nSegments + 1);
    for (unsigned int i = 0; i <= _nSegments; i++) {
        _curveParameterSteps.push_back(static_cast<double>(i));
    }

    // Arc lengths
    _lengthSums.reserve(_nSegments + 1);
    _lengthSums.push_back(0.0);
    for (unsigned int i = 1; i <= _nSegments; i++) {
        const double u = _curveParameterSteps[i];
        const double uPrev = _curveParameterSteps[i - 1];
        const double length = arcLength(uPrev, u);
        _lengthSums.push_back(_lengthSums[i - 1] + length);
    }
    _totalLength = _lengthSums.back();

    if (_totalLength < LengthEpsilon) {
        throw TooShortPathError("Path too short");
    }

    // Compute a map of arc lengths s and curve parameters u, for reparameterization
    constexpr int Steps = 100;
    const double uStep = 1.0 / static_cast<double>(Steps);
    _parameterSamples.reserve(Steps * _nSegments + 1);

    for (unsigned int i = 0; i < _nSegments; i++) {
        const double uStart = _curveParameterSteps[i];
        const double sStart = _lengthSums[i];
        _parameterSamples.push_back({ uStart, sStart });
        // Intermediate sampels
        for (int j = 1; j < Steps; j++) {
            const double u = uStart + j * uStep;
            const double s = sStart + arcLength(uStart, u);
            // Identify samples that are indistinguishable due to precision limitations
            if (std::abs(s - _parameterSamples.back().s) < LengthEpsilon) {
                throw InsufficientPrecisionError(
                    "Insufficient precision due to path length"
                );
            }
            _parameterSamples.push_back({ u, s });
        }
    }

    // Remove the very last sample if indistinguishable from the final one
    const double diff = std::abs(_totalLength - _parameterSamples.back().s);
    if (diff < LengthEpsilon) {
        _parameterSamples.pop_back();
    }

    _parameterSamples.push_back({ max, _totalLength });
    _parameterSamples.shrink_to_fit();
}

// Compute the curve parameter from an arc length value, using a combination of
// Newton's method and bisection. Source:
// https://www.geometrictools.com/Documentation/MovingAlongCurveSpecifiedSpeed.pdf
// Input s is a length value, in the range [0, _totalLength]
// Returns curve parameter in range [0, _nSegments]
double PathCurve::curveParameter(double s) const {
    if (s <= 0.0) {
        return 0.0;
    }
    if (s >= (_totalLength - LengthEpsilon)) {
        return _curveParameterSteps.back();
    }

    unsigned int segmentIndex = 1;
    while (s > _lengthSums[segmentIndex]) {
        segmentIndex++;
    }

    const double segmentS = s - _lengthSums[segmentIndex - 1];
    const double uMin = _curveParameterSteps[segmentIndex - 1];
    const double uMax = _curveParameterSteps[segmentIndex];

    // Find sample indices corresponding to the selected sample
    auto findIndexOfFirstEqualOrLarger_u = [&samples = _parameterSamples](double value) {
        auto it = std::lower_bound(
            samples.begin(),
            samples.end(),
            ParameterPair{ value, 0.0 }, // 0.0 is a dummy value for s
            [](const ParameterPair& lhs, const ParameterPair& rhs) {
                return lhs.u < rhs.u;
            }
        );
        return std::distance(samples.begin(), it);
    };

    const size_t startIndex = findIndexOfFirstEqualOrLarger_u(uMin);
    const size_t endIndex = findIndexOfFirstEqualOrLarger_u(uMax);

    // Use samples to find an initial guess for Newton's method
    // Find first sample with s larger than input s
    auto sampleIterator = std::upper_bound(
        _parameterSamples.begin() + startIndex,
        _parameterSamples.begin() + endIndex,
        ParameterPair{ 0.0 , s }, // 0.0 is a dummy value for u
        [](const ParameterPair& lhs, const ParameterPair& rhs) {
            return lhs.s < rhs.s;
        }
    );

    const ParameterPair& sample = *sampleIterator;
    const ParameterPair& prevSample = *(sampleIterator - 1);
    const double uPrev = prevSample.u;
    const double sPrev = prevSample.s;
    const double slope = (sample.u - uPrev) / (sample.s - sPrev);
    double u = uPrev + slope * (s - sPrev);

    constexpr int maxIterations = 50;

    // Initialize root bounding limits for bisection
    double lower = uMin;
    double upper = uMax;

    for (int i = 0; i < maxIterations; i++) {
        const double F = arcLength(uMin, u) - segmentS;

        // The error we tolerate, in meters. Note that distances are very large
        constexpr double tolerance = 0.5;
        if (std::abs(F) <= tolerance) {
            return u;
        }

        // Generate a candidate for Newton's method
        const double dfdu = approximatedDerivative(u); // > 0
        const double uCandidate = u - F / dfdu;

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
    const double max = _curveParameterSteps.back();
    if (u <= h) {
        return (1.0 / h) * glm::length(interpolate(0.0 + h) - interpolate(0.0));
    }
    if (u >= max - h) {
        return (1.0 / h) * glm::length(interpolate(max) - interpolate(max - h));
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
    const double max = _curveParameterSteps.back();
    ghoul_assert(u >= 0 && u <= max, "Interpolation variable must be in [0, _nSegments]");

    if (u <= 0.0) {
        return _points[1];
    }
    if (u >= max) {
        return *(_points.end() - 2);
    }

    const std::vector<double>::const_iterator segmentEndIt =
        std::lower_bound(_curveParameterSteps.begin(), _curveParameterSteps.end(), u);

    const int index =
        static_cast<int>((segmentEndIt - 1) - _curveParameterSteps.begin());

    const double segmentStart = _curveParameterSteps[index];
    const double segmentDuration = (_curveParameterSteps[index + 1] - segmentStart);
    const double uSegment = (u - segmentStart) / segmentDuration;

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
    _points.push_back(end.position());

    _totalLength = glm::distance(start.position(), end.position());
    _nSegments = 1;
}

glm::dvec3 LinearCurve::positionAt(double relativeDistance) const {
    return interpolate(relativeDistance);
}

glm::dvec3 LinearCurve::interpolate(double u) const {
    if (u <= 0.0) {
        return _points.front();
    }
    if (u >= 1.0) {
        return _points.back();
    }

    return ghoul::interpolateLinear(u, _points.front(), _points.back());
}

} // namespace openspace::interaction
