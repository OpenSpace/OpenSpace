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

#ifndef __OPENSPACE_MODULE_AUTONAVIGATION___PATHCURVE___H__
#define __OPENSPACE_MODULE_AUTONAVIGATION___PATHCURVE___H__

#include <modules/autonavigation/waypoint.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace::autonavigation {

// TODO: move to pathsegment.h instead
enum CurveType {
    AvoidCollision,
    Bezier3,
    Linear,
};

class PathCurve {
public:
    virtual ~PathCurve() = 0;

    const double length() const;
    glm::dvec3 positionAt(double relativeLength);

    // compute curve parameter that matches the input arc length s
    double curveParameter(double s);

    virtual glm::dvec3 interpolate(double u) = 0;

    std::vector<glm::dvec3> getPoints(); // for debugging

protected:
    // TODO: give a better name after experimental curve types have been added
    void initParameterIntervals();

    double approximatedDerivative(double u, double h = 1E-7);
    double arcLength(double limit = 1.0);
    double arcLength(double lowerLimit, double upperLimit);

    std::vector<glm::dvec3> _points; 
    unsigned int _nrSegments;

    std::vector<double> _parameterIntervals;
    std::vector<double> _lengths;
    std::vector<double> _lengthSums;
    double _totalLength;
};

// TODO: Put path curve classes in separate files

class Bezier3Curve : public PathCurve {
public:
    Bezier3Curve(const Waypoint& start, const Waypoint& end);
    glm::dvec3 interpolate(double u);
};

class LinearCurve : public PathCurve {
public:
    LinearCurve(const Waypoint& start, const Waypoint& end);
    glm::dvec3 interpolate(double u);
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___PATHCURVE___H__
