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

#include <modules/autonavigation/pathsegment.h>

#include <modules/autonavigation/helperfunctions.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <ghoul/Misc/interpolator.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PathSegment";
} // namespace

namespace openspace::autonavigation {

PathSegment::PathSegment(
    CameraState start, CameraState end, double duration, double startTime, CurveType type)
    : _start(start), _end(end), _duration(duration), _startTime(startTime), _curveType(type)
{ 
    switch(_curveType) {
    case Bezier:
        generateBezier();
        break;
    case Bezier2:
        generateBezier2();
        break;
    case Linear:
        break;
    case Linear2:
        generateLinear2(); 
        break;
    default:
        LERROR(fmt::format("Cannot create curve of type {}. Type does not exist!", _curveType));
    }  
}

CameraState PathSegment::start() const { return _start; }

CameraState PathSegment::end() const { return _end; }

double PathSegment::duration() const { return _duration; }

double PathSegment::startTime() const { return _startTime; }

glm::vec3 PathSegment::getPositionAt(double t) {
    t = easingfunctions::cubicEaseInOut(t);

    switch(_curveType) {
    case Bezier: 
        return interpolator::cubicBezier(t,
            _controlPoints[0], _controlPoints[1], _controlPoints[2], _controlPoints[3]);
        break;
    case Bezier2:
        return interpolator::piecewiseCubicBezier(t, _controlPoints);
        break;
    case Linear:
        return ghoul::interpolateLinear(t, _start.position, _end.position);
        break;
    case Linear2:
        return interpolator::piecewiseLinear(t, _controlPoints);
        break;
    default:
        LERROR(fmt::format("Cannot get position for curve type {}. Type does not exist!", _curveType));
    }        
}

glm::dquat PathSegment::getRotationAt(double t) {
    double tRot = helpers::shiftAndScale(t, 0.1, 0.9);
    tRot = easingfunctions::cubicEaseInOut(tRot);

    switch (_curveType) {
    case Linear2:
        return getLookAtRotation(
            tRot, 
            getPositionAt(t), 
            global::navigationHandler.camera()->lookUpVectorWorldSpace()
        );
        break;
    default:
        return glm::slerp(_start.rotation, _end.rotation, tRot);
    }
}

glm::dquat PathSegment::getLookAtRotation(double t, glm::dvec3 currentPos, glm::dvec3 up) {
    glm::dvec3 startLookAtPos = sceneGraphNode(_start.referenceNode)->worldPosition();
    glm::dvec3 endLookAtPos = sceneGraphNode(_end.referenceNode)->worldPosition();
    glm::dvec3 lookAtPos = ghoul::interpolateLinear(t, startLookAtPos, endLookAtPos);

    glm::dmat4 lookAtMat = glm::lookAt(
        currentPos,
        lookAtPos, 
        up
    );

    return glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));
}

// A single bezier segment with four control points
void PathSegment::generateBezier() {
    glm::dvec3 startNodePos = sceneGraphNode(_start.referenceNode)->worldPosition();
    glm::dvec3 endNodePos = sceneGraphNode(_end.referenceNode)->worldPosition();
    // vectors pointing away from target nodes
    glm::dvec3 startDirection =_start.position - startNodePos; 
    glm::dvec3 endDirection = _end.position - endNodePos; 

    _controlPoints.push_back(_start.position);
    _controlPoints.push_back(_start.position + 10.0 * startDirection);
    _controlPoints.push_back(_end.position + 10.0 * endDirection);
    _controlPoints.push_back(_end.position);
}

void PathSegment::generateBezier2() {
    // START: 
    glm::dvec3 startNodePos = sceneGraphNode(_start.referenceNode)->worldPosition();
    glm::dvec3 startDirection = _start.position - startNodePos;

    // END:   
    glm::dvec3 endNodePos = sceneGraphNode(_end.referenceNode)->worldPosition();
    glm::dvec3 endDirection = _end.position - endNodePos;

    // MIDDLE: one knot and two control points parallell to target nodes
    glm::dvec3 AB = endNodePos - startNodePos;
    glm::dvec3 C = normalize(startDirection + endDirection);
    glm::dvec3 CparAB = glm::dot(C, normalize(AB))* normalize(AB);
    glm::dvec3 CortAB = normalize(C - CparAB);
    double d = length(AB);

    // TODO: set points that actually look good
    _controlPoints.push_back(_start.position);
    _controlPoints.push_back(_start.position + 2.0 * startDirection);

    _controlPoints.push_back(_start.position + 1.5 * d * CortAB);
    _controlPoints.push_back(_start.position + 1.5 * d * CortAB + 0.5 * AB);
    _controlPoints.push_back(_end.position + 1.5 * d * CortAB);

    _controlPoints.push_back(_end.position + 2.0 * endDirection);
    _controlPoints.push_back(_end.position);
}

void PathSegment::generateLinear2() {
    // START: 
    glm::dvec3 startNodePos = sceneGraphNode(_start.referenceNode)->worldPosition();
    glm::dvec3 startDirection = _start.position - startNodePos;

    // END:   
    glm::dvec3 endNodePos = sceneGraphNode(_end.referenceNode)->worldPosition();
    glm::dvec3 endDirection = _end.position - endNodePos;

    // MIDDLE: 
    glm::dvec3 AB = endNodePos - startNodePos;
    glm::dvec3 C = normalize(startDirection + endDirection);
    glm::dvec3 CparAB = glm::dot(C, normalize(AB))* normalize(AB);
    glm::dvec3 CortAB = normalize(C - CparAB);
    double d = length(AB);

    _controlPoints.push_back(_start.position);
    _controlPoints.push_back(_start.position + 2.0 * d * CortAB + 0.5 * AB); //TODO: use scale instead of 2.0
    _controlPoints.push_back(_end.position);
}
} // namespace openspace::autonavigation
