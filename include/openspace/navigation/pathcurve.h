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

#ifndef __OPENSPACE_CORE___PATHCURVE___H__
#define __OPENSPACE_CORE___PATHCURVE___H__

#include <ghoul/glm.h>
#include <ghoul/misc/exception.h>
#include <vector>

namespace openspace::interaction {

class Waypoint;

class PathCurve {
public:
    struct InsufficientPrecisionError : public ghoul::RuntimeError {
        explicit InsufficientPrecisionError(std::string error);
    };

    struct TooShortPathError : public ghoul::RuntimeError {
        explicit TooShortPathError(std::string error);
    };

    virtual ~PathCurve() = 0;

    /**
     * Return the length of the curve, in meters.
     */
    double length() const;

    /**
     * Compute and return the position along the path at the specified relative
     * distance. The input parameter should be in range [0, 1], where 1 correspond to
     * the full length of the path.
     *
     * Can be overridden by subclasses that want more control over the position
     * interpolation.
     */
    virtual glm::dvec3 positionAt(double relativeDistance) const;

    /**
     * Get the intorlatied position along the spline, based on the given curve parameter
     * u in range [0, 1]. A curve parameter of 0 returns the start position and 1 the end
     * position. Note that u does not correspond to the relatively traveled distance.
     *
     * Can be overridden by subclasses that want more control over the position
     * interpolation.
     */
    virtual glm::dvec3 interpolate(double u) const;

    /**
     * Return the positions defining the control points for the spline interpolation.
     */
    std::vector<glm::dvec3> points() const;

protected:
    /**
     * Precompute information related to the spline parameters that are needed for arc
     * length reparameterization. Must be called after control point creation.
     */
    void initializeParameterData();

    /**
     * Compute curve parameter u that matches the input arc length s. Input s is a length
     * value in meters, in the range [0, _totalLength]. The returned curve parameter u is
     * in range [0, 1].
     */
    double curveParameter(double s) const;

    double approximatedDerivative(double u, double h = 0.0001) const;
    double arcLength(double limit = 1.0) const;
    double arcLength(double lowerLimit, double upperLimit) const;

    std::vector<glm::dvec3> _points;
    unsigned int _nSegments = 0;

    std::vector<double> _curveParameterSteps; // per segment
    std::vector<double> _lengthSums; // per segment
    double _totalLength = 0.0; // meters

    struct ParameterPair {
        double u; // curve parameter
        double s; // arc length parameter
    };

    std::vector<ParameterPair> _parameterSamples;
};

class LinearCurve : public PathCurve {
public:
    LinearCurve(const Waypoint& start, const Waypoint& end);

    glm::dvec3 positionAt(double relativeDistance) const override;
    glm::dvec3 interpolate(double u) const override;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___PATHCURVE___H__
