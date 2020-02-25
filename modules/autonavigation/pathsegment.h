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

#ifndef __OPENSPACE_MODULE___PATHSEGMENT___H__
#define __OPENSPACE_MODULE___PATHSEGMENT___H__

#include <modules/autonavigation/camerastate.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace::autonavigation {

enum class CurveType {
    Bezier3,
    Linear,
    Pause, // OBS! Temporary special case for handling pauses
    None
};

class PathCurve;

class PathSegment {
public:
    PathSegment(CameraState start, CameraState end, double startTime, CurveType type = CurveType::Bezier3);
    ~PathSegment() = default;

    // Mutators
    void setStart(CameraState cs);
    void setDuration(double d);

    // Accessors
    const CameraState start() const;
    const CameraState end() const;
    const double duration() const;
    const double startTime() const;
    const double endTime() const;
    const double pathLength() const;

    const std::vector<glm::dvec3> getControlPoints() const; // TODO: remove this debugging function

    double speedAtTime(double time);

    glm::dvec3 getPositionAt(double u) const;
    glm::dquat getRotationAt(double u) const;

private: 
    const glm::dquat piecewiseSlerpRotation(double u) const;
    void initCurve();

    // The speed function describing the shape of the speed curve. Values in [0,1].
    struct SpeedFunction {
        SpeedFunction() = default;
        SpeedFunction(double duration);
        double value(double t);

        // store the sum of the function over the duration of the segment, 
        // so we don't need to recompue it every time we access the speed 
        double integratedSum;
    };

    CameraState _start;
    CameraState _end;
    double _startTime; 
    double _duration;
    CurveType _curveType; 

    SpeedFunction _speedFunction;

    std::shared_ptr<PathCurve> _curve; 
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE___PATHSEGMENT___H__
