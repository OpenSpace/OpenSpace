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

#include <modules/autonavigation/zoomoutoverviewcurve.h>

#include <modules/autonavigation/autonavigationmodule.h>
#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/waypoint.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "ZoomOutOverviewCurve";
    const double Epsilon = 1E-12;

} // namespace

namespace openspace::autonavigation {

// Go far out to get a view of both tagets, aimed to match lookAt orientation 
ZoomOutOverviewCurve::ZoomOutOverviewCurve(const Waypoint& start, const Waypoint& end) {
    glm::dvec3 startNodePos = start.node()->worldPosition();
    glm::dvec3 endNodePos = end.node()->worldPosition();

    double startNodeRadius = start.nodeDetails.validBoundingSphere;
    double endNodeRadius = end.nodeDetails.validBoundingSphere;

    glm::dvec3 startNodeToStartPos = start.position() - startNodePos;
    glm::dvec3 endNodeToEndPos = end.position() - endNodePos;
    glm::dvec3 startNodeToEndNode = endNodePos - startNodePos;

    double startTangentLength = 2.0 * startNodeRadius;
    double endTangentLength = 2.0 * endNodeRadius;
    glm::dvec3 startTangentDirection = normalize(startNodeToStartPos);
    glm::dvec3 endTangentDirection = normalize(endNodeToEndPos);

    // Start by going outwards
    _points.push_back(start.position());
    _points.push_back(start.position() + startTangentLength * startTangentDirection);

    const std::string& startNode = start.nodeDetails.identifier;
    const std::string& endNode = end.nodeDetails.identifier;

    // Zoom out
    if (startNode != endNode) {
        // TODO: set a smarter orthogonal direction 
        glm::dvec3 parallell = glm::proj(startNodeToStartPos, startNodeToEndNode);
        glm::dvec3 orthogonalDirction = normalize(startNodeToStartPos - parallell);
        
        glm::dvec3 extraKnot = startNodePos + 0.5 * startNodeToEndNode 
            + 1.5 * glm::length(startNodeToEndNode) * orthogonalDirction;

        _points.push_back(extraKnot - 0.2 * startNodeToEndNode);
        _points.push_back(extraKnot);
        _points.push_back(extraKnot + 0.2 * startNodeToEndNode);
    }

    // closing in on end node
    _points.push_back(end.position() + endTangentLength * endTangentDirection);
    _points.push_back(end.position());

    _nrSegments = (unsigned int)std::floor((_points.size() - 1) / 3.0);

    initParameterIntervals();
}

glm::dvec3 ZoomOutOverviewCurve::interpolate(double u) {
    if (u < Epsilon)
        return _points[0];
    if (u > (1.0 - Epsilon))
        return _points.back();

    return interpolation::piecewiseCubicBezier(u, _points, _parameterIntervals);
}

} // namespace openspace::autonavigation
