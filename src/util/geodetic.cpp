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

#include <openspace/util/geodetic.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/ellipsoid.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>

namespace openspace {

void goToGeodetic2(const SceneGraphNode& globe, Geodetic2 geo) {
    const double altitude = altitudeFromCamera(globe);

    goToGeodetic3(globe, { std::move(geo), altitude });
}

void goToGeodetic3(const SceneGraphNode& sgn, Geodetic3 geo) {
    const glm::dvec3 positionModelSpace = sgn.ellipsoid().cartesianPosition(geo);

    interaction::NavigationState state;
    state.anchor = sgn.identifier();
    state.referenceFrame = sgn.identifier();
    state.position = positionModelSpace;
    // For globes, we know that the up-direction will always be positive Z.
    // @TODO (2023-12-06 emmbr) Eventually, we want each scene graph node to be aware of
    // its own preferred up-direction. At that time, this should no longer be hardcoded
    state.up = glm::dvec3(0.0, 0.0, 1.0);

    global::navigationHandler->setNavigationStateNextFrame(state);
}

glm::vec3 cartesianCoordinatesFromGeo(const SceneGraphNode& sgn, double latitude,
                                      double longitude, std::optional<double> altitude)
{
    const Geodetic3 pos = {
        {.lat = glm::radians(latitude), .lon = glm::radians(longitude) },
        altitude.value_or(altitudeFromCamera(sgn))
    };
    return glm::vec3(sgn.ellipsoid().cartesianPosition(pos));
}

glm::dvec3 geoPositionFromCamera() {
    const SceneGraphNode* n = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!n) {
        return glm::dvec3(0.0);
    }
    const Renderable* renderable = n->renderable();
    if (!renderable) {
        return glm::dvec3(0.0);
    }

    const glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();
    const glm::dmat4 inverseModelTransform = glm::inverse(n->modelTransform());
    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1.0));
    const SurfacePositionHandle posHandle = renderable->calculateSurfacePositionHandle(
        cameraPositionModelSpace
    );

    const Geodetic2 geo2 = renderable->ellipsoid().cartesianToGeodetic2(
        posHandle.centerToReferenceSurface
    );

    const double lat = glm::degrees(geo2.lat);
    const double lon = glm::degrees(geo2.lon);

    double altitude = glm::length(
        cameraPositionModelSpace - posHandle.centerToReferenceSurface
    );

    if (glm::length(cameraPositionModelSpace) <
        glm::length(posHandle.centerToReferenceSurface))
    {
        altitude = -altitude;
    }

    return glm::dvec3(lat, lon, altitude);
}

double altitudeFromCamera(const SceneGraphNode& sgn, bool useHeightMap) {
    const glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();

    const glm::dmat4 inverseModelTransform = glm::inverse(sgn.modelTransform());

    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1.0));

    SurfacePositionHandle posHandle = sgn.calculateSurfacePositionHandle(
        cameraPositionModelSpace
    );

    if (useHeightMap) {
        const glm::dvec3 centerToActualSurface = posHandle.centerToReferenceSurface +
            posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;

        return glm::length(cameraPositionModelSpace - centerToActualSurface);
    }
    else {
        // Do not use height map => compute distance to reference surface
        return glm::length(cameraPositionModelSpace - posHandle.centerToReferenceSurface);
    }
}

} // namespace openspace
