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

#include <modules/base/rotation/globerotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/ellipsoid.h>
#include <openspace/util/geodetic.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>

namespace {
    constexpr openspace::properties::Property::PropertyInfo GlobeInfo = {
        "Globe",
        "Attached Globe",
        "The node on which the longitude/latitude is specified. If the node is a globe, "
        "the correct height information for the globe is used. Otherwise, the position "
        "is specified based on the longitude and latitude on the node's interaction "
        "sphere",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LatitudeInfo = {
        "Latitude",
        "Latitude",
        "The latitude of the location on the globe's surface. The value can range from "
        "-90 to 90, with negative values representing the southern hemisphere of the "
        "globe.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LongitudeInfo = {
        "Longitude",
        "Longitude",
        "The longitude of the location on the globe's surface. The value can range from "
        "-180 to 180, with negative values representing the western hemisphere of the "
        "globe.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AngleInfo = {
        "Angle",
        "Angle",
        "A rotation angle (in degrees) that can be used to rotate the object around its "
        "own y-axis, which will be pointing out of the globe's surface.",
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
        "If this value is 'true', the latitute and longitude are updated each frame "
        "to match the location of the camera.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This `Rotation` orients the scene graph node in such a way that the y-axis points
    // away from the provided globe, the x-axis points towards the globe's southern pole
    // and the z-axis points in a western direction. Using this rotation generally means
    // using the [GlobeTranslation](#base_translation_globetranslation) to place the scene
    // graph node at the same position for which the rotation is calculated.
    struct [[codegen::Dictionary(GlobeRotation)]] Parameters {
        // [[codegen::verbatim(GlobeInfo.description)]]
        std::string globe [[codegen::identifier()]];

        // [[codegen::verbatim(LatitudeInfo.description)]]
        double latitude [[codegen::inrange(-90.0, 90.0)]];

        // [[codegen::verbatim(LongitudeInfo.description)]]
        double longitude [[codegen::inrange(-180.0, 180.0)]];

        // [[codegen::verbatim(AngleInfo.description)]]
        std::optional<double> angle;

        // [[codegen::verbatim(UseHeightmapInfo.description)]]
        std::optional<bool> useHeightmap;

        // [[codegen::verbatim(UseCameraInfo.description)]]
        std::optional<bool> useCamera;
    };
#include "globerotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation GlobeRotation::Documentation() {
    return codegen::doc<Parameters>("base_rotation_globerotation");
}

GlobeRotation::GlobeRotation(const ghoul::Dictionary& dictionary)
    : Rotation(dictionary)
    , _sceneGraphNode(GlobeInfo)
    , _latitude(LatitudeInfo, 0.0, -90.0, 90.0)
    , _longitude(LongitudeInfo, 0.0, -180.0, 180.0)
    , _angle(AngleInfo, 0.0, 0.0, 360.0)
    , _useHeightmap(UseHeightmapInfo, false)
    , _useCamera(UseCameraInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sceneGraphNode = p.globe;
    _sceneGraphNode.onChange([this]() {
        findNode();
        setUpdateVariables();
    });
    addProperty(_sceneGraphNode);

    _latitude = p.latitude;
    _latitude.onChange([this]() { setUpdateVariables(); });
    addProperty(_latitude);

    _longitude = p.longitude;
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

void GlobeRotation::findNode() {
    SceneGraphNode* n = sceneGraphNode(_sceneGraphNode);
    if (!n || !n->renderable()) {
        LERRORC(
            "GlobeRotation",
            "Could not set attached node as it does not have a Renderable"
        );
        return;
    }
    _attachedNode = n;
}

void GlobeRotation::setUpdateVariables() {
    _matrixIsDirty = true;
    requireUpdate();
}

glm::vec3 GlobeRotation::computeSurfacePosition(double latitude, double longitude) const {
    ghoul_assert(_attachedNode, "Renderable cannot be nullptr");

    const Geodetic3 pos = {
        { .lat = glm::radians(latitude), .lon = glm::radians(longitude) },
        altitudeFromCamera(*_attachedNode)
    };

    const glm::vec3 groundPos = cartesianCoordinatesFromGeo(
        *_attachedNode,
        latitude,
        longitude,
        0.0
    );

    const SurfacePositionHandle h =
        _attachedNode->calculateSurfacePositionHandle(groundPos);

    // Compute position including heightmap
    return cartesianCoordinatesFromGeo(
        *_attachedNode,
        latitude,
        longitude,
        h.heightToSurface
    );
}

void GlobeRotation::update(const UpdateData& data) {
    if (!_attachedNode) [[unlikely]] {
        findNode();
        _matrixIsDirty = true;
    }

    if (_useHeightmap || _useCamera) {
        // If we use the heightmap, we have to compute the height every frame
        setUpdateVariables();
    }

    Rotation::update(data);
}

glm::dmat3 GlobeRotation::matrix(const UpdateData&) const {
    if (!_matrixIsDirty) [[likely]] {
        return _matrix;
    }

    if (!_attachedNode) {
        LERRORC(
            "GlobeRotation",
            std::format("Could not find globe '{}'", _sceneGraphNode.value())
        );
        return _matrix;
    }

    double lat = _latitude;
    double lon = _longitude;

    if (_useCamera) {
        const glm::dvec3 position = geoPositionFromCamera();
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
        yAxis = _attachedNode->ellipsoid().geodeticSurfaceNormal(
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
