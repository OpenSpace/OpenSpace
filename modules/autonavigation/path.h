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

#ifndef __OPENSPACE_MODULE_AUTONAVIGATION___PATH___H__
#define __OPENSPACE_MODULE_AUTONAVIGATION___PATH___H__

#include <modules/autonavigation/pathcurve.h>
#include <modules/autonavigation/speedfunction.h>
#include <modules/autonavigation/waypoint.h>

namespace openspace::autonavigation {

class Path {
public:
    enum CurveType {
        AvoidCollision,
        Linear,
        ZoomOutOverview
    };

    Path(Waypoint start, Waypoint end, CurveType type,
        std::optional<double> duration = std::nullopt);

    // TODO: add a constructor that takes an instruction and curve type?

    Waypoint startPoint() const;
    Waypoint endPoint() const;
    double duration() const;
    double pathLength() const;

    std::vector<glm::dvec3> controlPoints() const;

    CameraPose traversePath(double dt);
    std::string currentAnchor() const;
    bool hasReachedEnd() const;

    CameraPose interpolatedPose(double distance) const;

private:
    double speedAtTime(double time) const;
    glm::dquat interpolateRotation(double u) const;

    Waypoint _start;
    Waypoint _end;
    double _duration;
    CurveType _curveType;

    SpeedFunction _speedFunction;
    std::unique_ptr<PathCurve> _curve;

    // Playback variables
    double _traveledDistance = 0.0;
    double _progressedTime = 0.0; // Time since playback started
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___PATH___H__
