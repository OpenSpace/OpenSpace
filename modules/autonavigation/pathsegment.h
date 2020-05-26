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

#include <modules/autonavigation/pathcurves.h>
#include <modules/autonavigation/rotationinterpolator.h>
#include <modules/autonavigation/speedfunction.h>
#include <modules/autonavigation/waypoint.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace::autonavigation {

class PathSegment {
public:
    PathSegment(Waypoint start, Waypoint end, CurveType type, 
        std::optional<double> duration = std::nullopt);

    ~PathSegment() = default;

    // Mutators
    void setStart(Waypoint wp);

    // Accessors
    const Waypoint start() const;
    const Waypoint end() const;
    const double duration() const;
    const double pathLength() const;

    const std::vector<glm::dvec3> getControlPoints() const; // TODO: remove this debugging function

    CameraPose traversePath(double dt);
    std::string getCurrentAnchor() const;
    bool hasReachedEnd() const;

    double speedAtTime(double time) const;
    CameraPose interpolatedPose(double u) const; 

private: 
    void initCurve();

    Waypoint _start;
    Waypoint _end;
    double _duration;
    CurveType _curveType; 

    std::unique_ptr<SpeedFunction> _speedFunction;
    std::unique_ptr<RotationInterpolator> _rotationInterpolator;
    std::unique_ptr<PathCurve> _curve;

    // Playback variables
    double _traveledDistance = 0.0; 
    double _progressedTime = 0.0; // Time since playback started
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE___PATHSEGMENT___H__
