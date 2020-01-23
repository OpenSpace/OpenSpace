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

#include <modules/autonavigation/pathcurves.h>

#include <modules/autonavigation/helperfunctions.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PathCurve";
} // namespace

namespace openspace::autonavigation {

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
    return interpolation::cubicBezier(t,
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
    return interpolation::piecewiseCubicBezier(t, _points);
}

Bezier3Curve::Bezier3Curve(CameraState& start, CameraState& end) {
    // TODO: CALCULATE AND SET CONDITION BOOLS IN CURVE CONSTRUCTOR
    glm::dvec3 startNodePos = sceneGraphNode(start.referenceNode)->worldPosition();
    glm::dvec3 startDirection = start.position - startNodePos;
    double startRadius = sceneGraphNode(start.referenceNode)->boundingSphere();

    glm::dvec3 endNodePos = sceneGraphNode(end.referenceNode)->worldPosition();
    glm::dvec3 endDirection = end.position - endNodePos;

    glm::dvec3 nodePosDiff = endNodePos - startNodePos;
    double cosStartAngle = glm::dot(normalize(startDirection), normalize(nodePosDiff));

    // TODO: Test with raycaster, value can vary depending on how close we are to surface!!
    bool TARGET_BEHIND_STARTNODE = cosStartAngle < -0.8; 
    bool TARGET_IN_OPPOSITE_DIRECTION = cosStartAngle > 0.8;


    // SET CONTROL POINTS
    _points.push_back(start.position);
    _points.push_back(start.position + 2.0 * startRadius * normalize(startDirection));

    if (TARGET_BEHIND_STARTNODE)
    {
        glm::dvec3 parallell = normalize(nodePosDiff) * glm::dot(startDirection, normalize(nodePosDiff));
        glm::dvec3 orthogonal = normalize(startDirection - parallell);
        //Point on the side of start node
        double dist = 5.0 * startRadius;
        glm::dvec3 extraKnot = startNodePos + dist * orthogonal;
        
        _points.push_back(extraKnot - parallell);
        _points.push_back(extraKnot); 
        _points.push_back(extraKnot + parallell);
    }

    _points.push_back(end.position + 2.0 * endDirection);
    _points.push_back(end.position);
}


glm::dvec3 Bezier3Curve::interpolate(double t) {
    return interpolation::piecewiseCubicBezier(t, _points);
}

LinearCurve::LinearCurve(CameraState& start, CameraState& end) {
    _points.push_back(start.position);
    _points.push_back(end.position);
}

glm::dvec3 LinearCurve::interpolate(double t) {
    return interpolation::linear(t, _points[0], _points[1]);
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
    return interpolation::piecewiseLinear(t, _points);
}

} // namespace openspace::autonavigation
