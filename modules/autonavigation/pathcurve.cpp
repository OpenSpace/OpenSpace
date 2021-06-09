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
    constexpr const int NrSamplesPerSegment = 100;
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

// Compute the curve parameter from an arc length value
// Input s is a length value, in the range [0, _totalLength]
// Returns curve parameter in range [0, 1]
double PathCurve::curveParameter(double s) {
    if (s <= 0.0) return 0.0;
    if (s >= _totalLength) return 1.0;

    unsigned int segmentIndex = 1;
    while (s > _arcLengthSums[segmentIndex]) {
        segmentIndex++;
    }

    const int startIndex = (segmentIndex - 1) * NrSamplesPerSegment;
    const int endIndex = segmentIndex * NrSamplesPerSegment + 1;

    // Find first sample with s larger than input s
    auto sampleIterator = std::upper_bound(
        _parameterSamples.begin() + startIndex,
        _parameterSamples.begin() + endIndex,
        ParameterPair{ 0.0 , s }, // 0.0 is a dummy value for u
        [](ParameterPair lhs, ParameterPair rhs) {
            return lhs.s < rhs.s;
        }
    );

    const ParameterPair& sample = *sampleIterator;
    const ParameterPair& prevSample = *(sampleIterator - 1);
    const double uPrev = prevSample.u;
    const double sPrev = prevSample.s;

    // Linearly interpolate between samples
    const double slope = (sample.u - uPrev) / (sample.s - sPrev);
    return uPrev + slope * (s - sPrev);
}

std::vector<glm::dvec3> PathCurve::points() {
    return _points;
}

void PathCurve::initializeParameterData() {
    ghoul_assert(_nSegments > 0, "Cannot have a curve with zero segments!");

    _curveParameterSteps.clear();
    _arcLengthSums.clear();
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
    _arcLengthSums.reserve(_nSegments + 1);
    _arcLengthSums.push_back(0.0);
    for (unsigned int i = 1; i <= _nSegments; i++) {
        double u = _curveParameterSteps[i];
        double uPrev = _curveParameterSteps[i - 1];
        double length = arcLength(uPrev, u);
        _arcLengthSums.push_back(_arcLengthSums[i - 1] + length);
    }
    _totalLength = _arcLengthSums.back();

    // Compute a map of arc lengths s and curve parameters u, for reparameterization
    _parameterSamples.reserve(NrSamplesPerSegment * _nSegments + 1);
    const double uStep = 1.0 / (_nSegments * NrSamplesPerSegment);
    for (unsigned int i = 0; i < _nSegments; i++) {
        double uStart = _curveParameterSteps[i];
        double sStart = _arcLengthSums[i];
        for (int j = 0; j < NrSamplesPerSegment; ++j) {
            double u = uStart + j * uStep;
            double s = sStart + arcLength(uStart, u);
            _parameterSamples.push_back({ u, s });
        }
    }
    _parameterSamples.push_back({ 1.0, _totalLength });
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
    initializeParameterData();
}

glm::dvec3 LinearCurve::interpolate(double u) {
    ghoul_assert(u >= 0 && u <= 1.0, "Interpolation variable out of range [0, 1]");
    return interpolation::linear(u, _points[0], _points[1]);
}

} // namespace openspace::autonavigation
