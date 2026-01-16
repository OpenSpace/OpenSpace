/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <openspace/navigation/navigationstate.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/ellipsoid.h>
#include <openspace/util/updatestructures.h>
#include <utility>

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

    const Geodetic2 geo = renderable->ellipsoid().cartesianToGeodetic2(
        posHandle.centerToReferenceSurface
    );

    const double lat = glm::degrees(geo.lat);
    const double lon = glm::degrees(geo.lon);

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

glm::dvec3 geoViewFromCamera() {
    const SceneGraphNode* n = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!n) {
        return glm::dvec3(0.0);
    }

    const glm::dmat4 inverseModelTransform = glm::inverse(n->modelTransform());

    // Get the position of the camera in model space
    const glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();
    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1.0));

    // Get the direction of the camera in model space
    const glm::dvec3 cameraViewDirection =
        global::navigationHandler->camera()->viewDirectionWorldSpace();
    // Scaling the cameraViewDirection to move the precision up a few decimals
    const glm::dvec3 cameraViewPoint = cameraPosition + 10000.0 * cameraViewDirection;
    glm::dvec3 cameraViewDirectionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraViewPoint, 1.0));


    // `d` represents how much we are looking towards the center of the scene graph node
    // The closer `d` is to -1 or 1, the more we are looking either towards the center or
    // away from it
    const glm::dmat3 rotationOnly = glm::mat3_cast(glm::quat_cast(n->modelTransform()));
    const double d = glm::dot(
        glm::normalize(cameraPositionModelSpace),
        glm::normalize(glm::transpose(rotationOnly) * cameraViewDirection)
    );

    const SurfacePositionHandle posHandle = n->calculateSurfacePositionHandle(
        cameraViewDirectionModelSpace
    );

    const Geodetic2 geo = n->ellipsoid().cartesianToGeodetic2(
        posHandle.centerToReferenceSurface
    );

    const double lat = glm::degrees(geo.lat);
    const double lon = glm::degrees(geo.lon);
    return glm::dvec3(lat, lon, d);
}

glm::dvec2 subSolarCoordinates() {
    const SceneGraphNode* n = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!n) {
        return glm::dvec3(0.0);
    }

    const glm::dmat4 inverseModelTransform = glm::inverse(n->modelTransform());
    // @TODO (2025-07-16, abock): For now we use the position of the SolarSystemBarycenter
    // to calculate the Sun position. While this is not completely correct and precludes
    // the calculation of subsolar coordinates on exoplanets, that is a problem left for
    // later
    const glm::dvec3 ssbPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(-n->worldPosition(), 1.0));

    const SurfacePositionHandle sunPosition = n->calculateSurfacePositionHandle(
        ssbPositionModelSpace
    );
    const Geodetic2 subsolarCoordinates = n->ellipsoid().cartesianToGeodetic2(
        sunPosition.centerToReferenceSurface
    );

    const double lat = glm::degrees(subsolarCoordinates.lat);
    const double lon = glm::degrees(subsolarCoordinates.lon);
    return glm::dvec2(lat, lon);
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
