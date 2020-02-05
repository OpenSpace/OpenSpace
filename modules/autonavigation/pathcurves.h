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

#include <modules/autonavigation/camerastate.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace::autonavigation {

// OBS! Path curves has curve parameter in range [0,1]
class PathCurve {
public:
    virtual ~PathCurve() = 0;
    virtual glm::dvec3 valueAt(double t) = 0;
    std::vector<glm::dvec3> getPoints(); // for debugging

    double arcLength(double tLimit = 1.0);
protected:
    // the points used for creating the curve (e.g. control points of a Bezier curve)
    std::vector<glm::dvec3> _points; 
};

class Bezier3Curve : public PathCurve {
public:
    Bezier3Curve(CameraState& start, CameraState& end);
    glm::dvec3 valueAt(double t);
};

class LinearCurve : public PathCurve {
public:
    LinearCurve(CameraState& start, CameraState& end);
    glm::dvec3 valueAt(double t);
};

// OBS! This is a temporary class specialised for handling pauses. 
// TODO: handle better in the future. 
class PauseCurve : public PathCurve {
public:
    PauseCurve(CameraState& state);
    glm::dvec3 valueAt(double t);
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___PATHCURVE___H__
