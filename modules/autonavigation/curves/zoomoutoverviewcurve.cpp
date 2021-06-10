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

#include <modules/autonavigation/curves/zoomoutoverviewcurve.h>

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
} // namespace

namespace openspace::autonavigation {

// Go far out to get a view of both tagets, aimed to match lookAt orientation
ZoomOutOverviewCurve::ZoomOutOverviewCurve(const Waypoint& start, const Waypoint& end) {
    const double startNodeRadius = start.validBoundingSphere;
    const double endNodeRadius = end.validBoundingSphere;

    const double endTangentsLengthFactor = 2.0;
    const double startTangentLength = endTangentsLengthFactor * startNodeRadius;
    const double endTangentLength = endTangentsLengthFactor * endNodeRadius;

    const glm::dvec3 startNodePos = start.node()->worldPosition();
    const glm::dvec3 endNodePos = end.node()->worldPosition();
    const glm::dvec3 startNodeToStartPos = start.position() - startNodePos;
    const glm::dvec3 startTangentDir = normalize(startNodeToStartPos);

    // Start by going outwards
    _points.push_back(start.position());
    _points.push_back(start.position());
    _points.push_back(start.position() + startTangentLength * startTangentDir);

    const std::string& startNode = start.nodeIdentifier;
    const std::string& endNode = end.nodeIdentifier;

    // Zoom out
    if (startNode != endNode) {
        // TODO: set a smarter orthogonal direction, to avoid making a big detour
        glm::dvec3 startNodeToEndNode = endNodePos - startNodePos;
        glm::dvec3 parallell = glm::proj(startNodeToStartPos, startNodeToEndNode);
        glm::dvec3 orthogonal = normalize(startNodeToStartPos - parallell);

        glm::dvec3 extraKnot = startNodePos + 0.5 * startNodeToEndNode
            + 1.5 * glm::length(startNodeToEndNode) * orthogonal;

        _points.push_back(extraKnot);
    }

    // Closing in on end node
    const glm::dvec3 endNodeToEndPos = end.position() - endNodePos;
    const glm::dvec3 endTangentDir = normalize(endNodeToEndPos);

    _points.push_back(end.position() + endTangentLength * endTangentDir);
    _points.push_back(end.position());
    _points.push_back(end.position());

    initializeParameterData();
}

} // namespace openspace::autonavigation
