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

#include <openspace/navigation/pathcurves/orbitobjectcurve.h>

#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/pathcurve.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/navigation/waypoint.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/collisionhelper.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/vector_angle.hpp>
#include <algorithm>
#include <format>
#include <string_view>
#include <utility>
#include <vector>

namespace {
    constexpr std::string_view _loggerCat = "OrbitObjectCurve";
} // namespace

namespace openspace::interaction {

OrbitObjectCurve::OrbitObjectCurve(const Waypoint& start, const Waypoint& end) {
    if (!start.node() || !end.node()) { // guard, but should never happen
        LERROR("Something went wrong. The start or end node does not exist");
        return;
    }

    // TODO: (emmbr, 2022-02-27) For now, this path type only works for flying around
    // a single node, e.g. when flying to different positions around a planet.
    if (start.node()->identifier() != end.node()->identifier()) {
        LERROR(
            "This path type currently only works for flying around one given object. "
            "The start and end node should be the same."
        );
        return;
    }

    // Add control points for a catmull-rom spline, first and last will not be intersected
    _points.push_back(start.position());
    _points.push_back(start.position());

    // Compute positions for the intermediate control points, by steping in a circular
    // path from the start to end position around the object

    // Do somputation in local coordiate system of start node
    const glm::dmat4 invModelMatrix = glm::inverse(start.node()->modelTransform());
    const glm::dvec3 startLocal = glm::dvec3(invModelMatrix * glm::dvec4(start.position(), 1.0));
    const glm::dvec3 endLocal = glm::dvec3(invModelMatrix * glm::dvec4(end.position(), 1.0));

    const double startDistance = glm::length(startLocal);
    const double endDistance = glm::length(endLocal);
    const double distanceDiff = endDistance - startDistance;

    double angleBetweenPoints = glm::acos(glm::dot(
        glm::normalize(startLocal),
        glm::normalize(endLocal)
    ));

    glm::dvec3 rotationAxis = glm::normalize(glm::cross(startLocal, endLocal));

    const int nSteps = 10;
    double anglePerStep = angleBetweenPoints / nSteps;
    double distancePerStep = distanceDiff / nSteps;

    // Compute intermediate points, by rotating the start position step by step towards the end
    if (anglePerStep > 1.0) {
        for (int i = 1; i < nSteps - 1; i++) {
            double angle = anglePerStep * i;

            // Compute rotation to be applied around the axis
            const glm::dquat spinRotation = glm::angleAxis(angle, rotationAxis);

            // Rotate the position vector from the center to camera and update position
            const glm::dvec3 rotatedPosLocal = spinRotation * startLocal;

            // Also interpolate the distance
            const double interpolatedDistance = startDistance + distancePerStep * i;

            const glm::dvec3 rotatedPosLocalScaled =
                glm::normalize(rotatedPosLocal) * interpolatedDistance;

            const glm::dvec3 rotatedPos = glm::dvec3(
                start.node()->modelTransform() * glm::dvec4(rotatedPosLocalScaled, 1.0)
            );

            _points.push_back(rotatedPos);
        }
    }

    _points.push_back(end.position());
    _points.push_back(end.position());

    initializeParameterData();
}

} // namespace openspace::interaction
