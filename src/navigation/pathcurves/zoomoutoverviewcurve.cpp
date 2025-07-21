/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/navigation/pathcurves/zoomoutoverviewcurve.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/waypoint.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr std::string_view _loggerCat = "ZoomOutOverviewCurve";
} // namespace

namespace openspace::interaction {

// Go far out to get a view of both tagets, aimed to match lookAt orientation
ZoomOutOverviewCurve::ZoomOutOverviewCurve(const Waypoint& start, const Waypoint& end) {
    const double startNodeRadius = start.validBoundingSphere();
    const double endNodeRadius = end.validBoundingSphere();

    if (!start.node() || !end.node()) { // guard, but should never happen
        LERROR("Something went wrong. The start or end node does not exist");
        return;
    }

    const double endTangentsLengthFactor = 2.0;
    const double startTangentLength = endTangentsLengthFactor * startNodeRadius;
    const double endTangentLength = endTangentsLengthFactor * endNodeRadius;

    const glm::dvec3 startNodePos = start.node()->worldPosition();
    const glm::dvec3 endNodePos = end.node()->worldPosition();
    const glm::dvec3 startTangentDir = normalize(start.position() - startNodePos);
    const glm::dvec3 endTangentDir = normalize(end.position() - endNodePos);

    // Start by going outwards
    _points.push_back(start.position());
    _points.push_back(start.position());
    _points.push_back(start.position() + startTangentLength * startTangentDir);

    const glm::dvec3 startPosToEndPos = end.position() - start.position();
    constexpr double Epsilon = 1E-4;

    // Zoom out
    if (start.nodeIdentifier() != end.nodeIdentifier() &&
        glm::length(startPosToEndPos) > Epsilon)
    {
        const glm::dvec3& n1 = startTangentDir;
        const glm::dvec3& n2 = endTangentDir;
        const glm::dvec3 halfWayPos = start.position() + 0.5 * startPosToEndPos;

        // Decide the step direction for the "overview point" based on the directions
        // at the start and end of the path, to try to get a nice curve shape
        glm::dvec3 goodStepDirection;
        if (glm::dot(n1, n2) < 0.0) {
            // Facing in different directions => step in direction of the cross product
            goodStepDirection = glm::normalize(glm::cross(-n1, n2));
        }
        else {
            goodStepDirection = glm::normalize(n1 + n2);
        }

        // Find a direction that is orthogonal to the line between the start and end
        // position
        const glm::dvec3 outwardStepVector =
            0.5 * glm::length(startPosToEndPos) * goodStepDirection;

        const glm::dvec3 projectedVec = glm::proj(outwardStepVector, startPosToEndPos);
        const glm::dvec3 orthogonalComponent = outwardStepVector - projectedVec;
        const glm::dvec3 stepDirection = glm::normalize(orthogonalComponent);

        // Step half-way along the line between the position and then orthogonally
        const double stepDistance = 1.5 * glm::length(startPosToEndPos);
        const glm::dvec3 extraKnot = halfWayPos + stepDistance * stepDirection;

        _points.push_back(extraKnot);
    }

    // Closing in on end node
    _points.push_back(end.position() + endTangentLength * endTangentDir);
    _points.push_back(end.position());
    _points.push_back(end.position());

    initializeParameterData();
}

} // namespace openspace::interaction
