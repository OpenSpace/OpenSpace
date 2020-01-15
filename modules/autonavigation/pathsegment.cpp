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

// TODO: move all these later --------------------
PathCurve::~PathCurve() {}

BezierCurve::BezierCurve(CameraState& start, CameraState& end) {
    glm::dvec3 startNodePos = sceneGraphNode(start.referenceNode)->worldPosition();
    glm::dvec3 endNodePos = sceneGraphNode(start.referenceNode)->worldPosition();
    // vectors pointing away from target nodes
    glm::dvec3 startDirection = start.position - startNodePos;
    glm::dvec3 endDirection = end.position - endNodePos;

    _points.push_back(start.position);
    _points.push_back(start.position + 10.0 * startDirection);
    _points.push_back(end.position + 10.0 * endDirection);
    _points.push_back(end.position);
}

glm::dvec3 BezierCurve::interpolate(double t) {
    return interpolator::cubicBezier(t,
        _points[0], _points[1], _points[2], _points[3]);
}

Bezier2Curve::Bezier2Curve(CameraState& start, CameraState& end) {
    // START: 
    glm::dvec3 startNodePos = sceneGraphNode(start.referenceNode)->worldPosition();
    glm::dvec3 startDirection = start.position - startNodePos;

    // END:   
    glm::dvec3 endNodePos = sceneGraphNode(end.referenceNode)->worldPosition();
    glm::dvec3 endDirection = end.position - endNodePos;

    // MIDDLE: one knot and two control points parallell to target nodes
    glm::dvec3 AB = endNodePos - startNodePos;
    glm::dvec3 C = normalize(startDirection + endDirection);
    glm::dvec3 CparAB = glm::dot(C, normalize(AB))* normalize(AB);
    glm::dvec3 CortAB = normalize(C - CparAB);
    double d = length(AB);

    // TODO: set points that actually look good
    _points.push_back(start.position);
    _points.push_back(start.position + 2.0 * startDirection);

    _points.push_back(start.position + 1.5 * d * CortAB);
    _points.push_back(start.position + 1.5 * d * CortAB + 0.5 * AB);
    _points.push_back(end.position + 1.5 * d * CortAB);

    _points.push_back(end.position + 2.0 * endDirection);
    _points.push_back(end.position);
}

glm::dvec3 Bezier2Curve::interpolate(double t) {
    return interpolator::piecewiseCubicBezier(t, _points);
}

LinearCurve::LinearCurve(CameraState& start, CameraState& end) {
    _points.push_back(start.position);
    _points.push_back(end.position);
}

glm::dvec3 LinearCurve::interpolate(double t) {
    return ghoul::interpolateLinear(t, _points[0], _points[1]);
}

Linear2Curve::Linear2Curve(CameraState& start, CameraState& end) {
    // START: 
    glm::dvec3 startNodePos = sceneGraphNode(start.referenceNode)->worldPosition();
    glm::dvec3 startDirection = start.position - startNodePos;

    // END:   
    glm::dvec3 endNodePos = sceneGraphNode(end.referenceNode)->worldPosition();
    glm::dvec3 endDirection = end.position - endNodePos;

    // MIDDLE: 
    glm::dvec3 AB = endNodePos - startNodePos;
    glm::dvec3 C = normalize(startDirection + endDirection);
    glm::dvec3 CparAB = glm::dot(C, normalize(AB))* normalize(AB);
    glm::dvec3 CortAB = normalize(C - CparAB);
    double d = length(AB);

    _points.push_back(start.position);
    _points.push_back(start.position + 2.0 * d * CortAB + 0.5 * AB); //TODO: use scale instead of 2.0
    _points.push_back(end.position);
}

glm::dvec3 Linear2Curve::interpolate(double t) {
    return interpolator::piecewiseLinear(t, _points);
}

PathSegment::PathSegment(
    CameraState start, CameraState end, double startTime, CurveType type)
    : _start(start), _end(end), _startTime(startTime), _curveType(type)
{ 
    // TODO: compute duration based on method later
    _duration = 5;

    switch(type) {
    case Bezier:
        _curve = std::make_shared<BezierCurve>(start, end);
        break;
    case Bezier2:
        _curve = std::make_shared<Bezier2Curve>(start, end);
        break;
    case Linear:
        _curve = std::make_shared<LinearCurve>(start, end);
        break;
    case Linear2:
        _curve = std::make_shared<Linear2Curve>(start, end);
        break;
    default:
        LERROR(fmt::format("Cannot create curve of type {}. Type does not exist!", _curveType));
    }  
}

void PathSegment::setStart(CameraState cs) {
    _start = std::move(cs);
}

void PathSegment::setDuration(double d) {
    _duration = d;
}

const CameraState PathSegment::start() const { return _start; }

const CameraState PathSegment::end() const { return _end; }

const double PathSegment::duration() const { return _duration; }

const double PathSegment::startTime() const { return _startTime; }

const glm::vec3 PathSegment::getPositionAt(double t) const {
    t = easingfunctions::cubicEaseInOut(t);
    return _curve.get()->interpolate(t);    
}

const glm::dquat PathSegment::getRotationAt(double t) const {
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

const glm::dquat PathSegment::getLookAtRotation(
    double t, glm::dvec3 currentPos, glm::dvec3 up) const 
{
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

} // namespace openspace::autonavigation
