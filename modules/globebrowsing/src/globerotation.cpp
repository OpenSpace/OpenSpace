/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/globebrowsing/src/globerotation.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>

namespace {
    constexpr openspace::properties::Property::PropertyInfo GlobeInfo = {
        "Globe",
        "Attached Globe",
        "The globe on which the longitude/latitude is specified.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LatitudeInfo = {
        "Latitude",
        "Latitude",
        "The latitude of the location on the globe's surface. The value can range from "
        "-90 to 90, with negative values representing the southern hemisphere of the "
        "globe. The default value is 0.0.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LongitudeInfo = {
        "Longitude",
        "Longitude",
        "The longitude of the location on the globe's surface. The value can range from "
        "-180 to 180, with negative values representing the western hemisphere of the "
        "globe. The default value is 0.0.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AngleInfo = {
        "Angle",
        "Angle",
        "A rotation angle that can be used to rotate the object around its own y-axis, "
        "which will be pointing out of the globe's surface.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseHeightmapInfo = {
        "UseHeightmap",
        "Use Heightmap",
        "If set to true, the heightmap will be used when computing the surface normal. "
        "This means that the object will be rotated to lay flat on the surface at the "
        "given coordinate and follow the shape of the landscape.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UseCameraInfo = {
        "UseCamera",
        "Use Camera",
        "If this value is 'true', the lat and lon are updated to match the camera.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(GlobeRotation)]] Parameters {
        // [[codegen::verbatim(GlobeInfo.description)]]
        std::string globe
            [[codegen::annotation("A valid scene graph node with a RenderableGlobe")]];

        // [[codegen::verbatim(LatitudeInfo.description)]]
        std::optional<double> latitude;

        // [[codegen::verbatim(LongitudeInfo.description)]]
        std::optional<double> longitude;

        // [[codegen::verbatim(AngleInfo.description)]]
        std::optional<double> angle;

        // [[codegen::verbatim(UseHeightmapInfo.description)]]
        std::optional<bool> useHeightmap;

        // [[codegen::verbatim(UseCameraInfo.description)]]
        std::optional<bool> useCamera;
    };
#include "globerotation_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GlobeRotation::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_rotation_globerotation");
}

GlobeRotation::GlobeRotation(const ghoul::Dictionary& dictionary)
    : _globe(GlobeInfo)
    , _latitude(LatitudeInfo, 0.0, -90.0, 90.0)
    , _longitude(LongitudeInfo, 0.0, -180.0, 180.0)
    , _angle(AngleInfo, 0.0, 0.0, 360.0)
    , _useHeightmap(UseHeightmapInfo, false)
    , _useCamera(UseCameraInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _globe = p.globe;
    _globe.onChange([this]() {
        findGlobe();
        setUpdateVariables();
    });
    addProperty(_globe);

    _latitude = p.latitude.value_or(_latitude);
    _latitude.onChange([this]() { setUpdateVariables(); });
    addProperty(_latitude);

    _longitude = p.longitude.value_or(_longitude);
    _longitude.onChange([this]() { setUpdateVariables(); });
    addProperty(_longitude);

    _useHeightmap = p.useHeightmap.value_or(_useHeightmap);
    _useHeightmap.onChange([this]() { setUpdateVariables(); });
    addProperty(_useHeightmap);

    _angle = p.angle.value_or(_angle);
    _angle.onChange([this]() { setUpdateVariables(); });
    addProperty(_angle);

    _useCamera = p.useCamera.value_or(_useCamera);
    _useCamera.onChange([this]() { setUpdateVariables(); });
    addProperty(_useCamera);

}

void GlobeRotation::findGlobe() {
    SceneGraphNode* n = sceneGraphNode(_globe);
    if (n && n->renderable() && dynamic_cast<RenderableGlobe*>(n->renderable())) {
        _globeNode = dynamic_cast<RenderableGlobe*>(n->renderable());
    }
    else {
        LERRORC(
            "GlobeRotation",
            "Could not set attached node as it does not have a RenderableGlobe"
        );
        if (_globeNode) {
            // Reset the globe name to it's previous name
            _globe = _globeNode->identifier();
        }
    }
}

void GlobeRotation::setUpdateVariables() {
    _matrixIsDirty = true;
    requireUpdate();
}

glm::vec3 GlobeRotation::computeSurfacePosition(double latitude, double longitude) const
{
    ghoul_assert(_globeNode, "Globe cannot be nullptr");

    GlobeBrowsingModule* mod = global::moduleEngine->module<GlobeBrowsingModule>();
    const glm::vec3 groundPos = mod->cartesianCoordinatesFromGeo(
        *_globeNode,
        latitude,
        longitude,
        0.0
    );

    const SurfacePositionHandle h = _globeNode->calculateSurfacePositionHandle(groundPos);

    // Compute position including heightmap
    return mod->cartesianCoordinatesFromGeo(
        *_globeNode,
        latitude,
        longitude,
        h.heightToSurface
    );
}

void GlobeRotation::update(const UpdateData& data) {
    if (_useHeightmap || _useCamera) {
        // If we use the heightmap, we have to compute the height every frame
        setUpdateVariables();
    }

    Rotation::update(data);
}

glm::dmat3 GlobeRotation::matrix(const UpdateData&) const {
    if (!_globeNode) {
        // @TODO(abock): The const cast should be removed on a redesign of the rotation
        //               to make the matrix function not constant. Const casting this
        //               away is fine as the factories that create the rotations don't
        //               create them as constant objects
        const_cast<GlobeRotation*>(this)->findGlobe();
        _matrixIsDirty = true;
    }

    if (!_matrixIsDirty) {
        return _matrix;
    }

    if (!_globeNode) {
        LERRORC(
            "GlobeRotation",
            std::format("Could not find globe '{}'", _globe.value())
        );
        return _matrix;
    }

    double lat = _latitude;
    double lon = _longitude;

    if (_useCamera) {
        GlobeBrowsingModule* mod = global::moduleEngine->module<GlobeBrowsingModule>();
        const glm::dvec3 position = mod->geoPosition();
        lat = position.x;
        lon = position.y;
    }

    // Compute vector that points out of globe surface
    glm::dvec3 yAxis = glm::dvec3(0.0);
    if (_useHeightmap) {
        const double angleDiff = 0.00001; // corresponds to a meter-ish
        const glm::vec3 posCenter = computeSurfacePosition(lat, lon);
        const glm::vec3 pos1 = computeSurfacePosition(lat, lon + angleDiff);
        const glm::vec3 pos2 = computeSurfacePosition(lat + angleDiff, lon);

        const glm::vec3 v1 = pos1 - posCenter;
        const glm::vec3 v2 = pos2 - posCenter;
        yAxis = glm::dvec3(glm::cross(v1, v2));
    }
    else {
        const float latitudeRad = glm::radians(static_cast<float>(lat));
        const float longitudeRad = glm::radians(static_cast<float>(lon));
        yAxis = _globeNode->ellipsoid().geodeticSurfaceNormal(
            { latitudeRad, longitudeRad }
        );
    }
    yAxis = glm::normalize(yAxis);

    constexpr glm::dvec3 n = glm::dvec3(0.0, 0.0, 1.0);
    glm::dvec3 zAxis = glm::dvec3(
        zAxis.x = yAxis.y * n.z - yAxis.z * n.y,
        zAxis.y = yAxis.z * n.x - yAxis.x * n.z,
        zAxis.z = yAxis.x * n.y - yAxis.y * n.x
    );
    zAxis = glm::normalize(zAxis);

    const glm::dvec3 xAxis = glm::normalize(glm::cross(yAxis, zAxis));
    const glm::dmat3 mat = glm::dmat3(
        xAxis.x, xAxis.y, xAxis.z,
        yAxis.x, yAxis.y, yAxis.z,
        zAxis.x, zAxis.y, zAxis.z
    );

    const glm::dquat q = glm::angleAxis(glm::radians(_angle.value()), yAxis);
    _matrix = glm::toMat3(q) * mat;
    _matrixIsDirty = false;
    return _matrix;
}

} // namespace openspace::globebrowsing
