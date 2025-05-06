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

#include <modules/base/translation/globetranslation.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/geodetic.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>

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

    constexpr openspace::properties::Property::PropertyInfo AltitudeInfo = {
        "Altitude",
        "Altitude",
        "The altitude in meters. If the 'UseHeightmap' property is 'true', this is an "
        "offset from the actual surface of the globe. If not, this is an offset from the "
        "reference ellipsoid. The default value is 0.0.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UseHeightmapInfo = {
        "UseHeightmap",
        "Use Heightmap",
        "If this value is 'true', the altitude specified in 'Altitude' will be treated "
        "as an offset from the heightmap. Otherwise, it will be an offset from the "
        "globe's reference ellipsoid. The default value is 'false'.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UseCameraInfo = {
        "UseCamera",
        "Use Camera",
        "If this value is 'true', the lat and lon are updated to match the camera.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseCameraAltitudeInfo = {
        "UseCameraAltitude",
        "Use Camera Altitude",
        "If this value is 'true', the altitude is updated to match the camera.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This `Translation` places the scene graph node at a specific location relative to
    // another scene graph node. The position is identified via the longitude, latitude,
    // and altitude parameters. If the provided scene graph node is a globe, the positions
    // correspond to the native coordinate frame of that object. If it is a node without a
    // renderable or a non-globe renderable, the latitude/longitude grid used is the
    // node's interaction sphere.
    // This class is useful in conjunction with the
    // [GlobeRotation](#base_rotation_globerotation) rotation to orient a scene graph node
    // away from the center of the body.
    //
    // If the `UseCamera` value is set, the object's position automatically updates based
    // on the current camera location.
    struct [[codegen::Dictionary(GlobeTranslation)]] Parameters {
        // [[codegen::verbatim(GlobeInfo.description)]]
        std::string globe [[codegen::identifier()]];

        // [[codegen::verbatim(LatitudeInfo.description)]]
        std::optional<double> latitude;

        // [[codegen::verbatim(LongitudeInfo.description)]]
        std::optional<double> longitude;

        // [[codegen::verbatim(AltitudeInfo.description)]]
        std::optional<double> altitude;

        // [[codegen::verbatim(UseHeightmapInfo.description)]]
        std::optional<bool> useHeightmap;

        // [[codegen::verbatim(UseCameraInfo.description)]]
        std::optional<bool> useCamera;

        // [[codegen::verbatim(UseCameraAltitudeInfo.description)]]
        std::optional<bool> useCameraAltitude;
    };
#include "globetranslation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation GlobeTranslation::Documentation() {
    return codegen::doc<Parameters>("base_translation_globetranslation");
}

GlobeTranslation::GlobeTranslation(const ghoul::Dictionary& dictionary)
    : Translation(dictionary)
    , _sceneGraphNode(GlobeInfo)
    , _latitude(LatitudeInfo, 0.0, -90.0, 90.0)
    , _longitude(LongitudeInfo, 0.0, -180.0, 180.0)
    , _altitude(AltitudeInfo, 0.0, -1e12, 1e12)
    , _useHeightmap(UseHeightmapInfo, false)
    , _useCamera(UseCameraInfo, false)
    , _useCameraAltitude(UseCameraAltitudeInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sceneGraphNode = p.globe;
    _sceneGraphNode.onChange([this]() {
        fillAttachedNode();
        setUpdateVariables();
    });
    addProperty(_sceneGraphNode);

    _latitude = p.latitude.value_or(_latitude);
    _latitude.onChange([this]() { setUpdateVariables(); });
    addProperty(_latitude);

    _longitude = p.longitude.value_or(_longitude);
    _longitude.onChange([this]() { setUpdateVariables(); });
    addProperty(_longitude);

    _altitude = p.altitude.value_or(_altitude);
    // @TODO (emmbr) uncomment when ranges with negative values are supported
    //_altitude.setExponent(8.f);
    _altitude.onChange([this]() { setUpdateVariables(); });
    addProperty(_altitude);

    _useHeightmap = p.useHeightmap.value_or(_useHeightmap);
    _useHeightmap.onChange([this]() { setUpdateVariables(); });
    addProperty(_useHeightmap);

    _useCamera = p.useCamera.value_or(_useCamera);
    _useCamera.onChange([this]() { setUpdateVariables(); });
    addProperty(_useCamera);

    _useCameraAltitude = p.useCameraAltitude.value_or(_useCameraAltitude);
    _useCameraAltitude.onChange([this]() { setUpdateVariables(); });
    addProperty(_useCameraAltitude);
}

void GlobeTranslation::fillAttachedNode() {
    SceneGraphNode* n = sceneGraphNode(_sceneGraphNode);
    if (!n || !n->renderable()) {
        LERRORC(
            "GlobeTranslation",
            "Could not set attached node as it does not have a renderable"
        );
        return;
    }

    _attachedNode = n;
}

void GlobeTranslation::setUpdateVariables() {
    _positionIsDirty = true;
    requireUpdate();
}

void GlobeTranslation::update(const UpdateData& data) {
    if (!_attachedNode) [[unlikely]] {
        fillAttachedNode();
        _positionIsDirty = true;
    }

    if (_useHeightmap || _useCamera) {
        // If we use the heightmap, we have to compute the height every frame
        setUpdateVariables();
    }

    Translation::update(data);
}

glm::dvec3 GlobeTranslation::position(const UpdateData&) const {
    if (!_positionIsDirty) [[likely]] {
        return _position;
    }

    if (!_attachedNode) {
        LERRORC(
            "GlobeRotation",
            std::format("Could not find attached node '{}'", _sceneGraphNode.value())
        );
        return _position;
    }

    double lat = _latitude;
    double lon = _longitude;
    double alt = _altitude;

    if (_useCamera) {
        const glm::dvec3 position = geoPositionFromCamera();
        lat = position.x;
        lon = position.y;
        if (_useCameraAltitude) {
            alt = position.z;
        }
    }

    if (_useHeightmap) {
        const glm::vec3 groundPos = cartesianCoordinatesFromGeo(
            *_attachedNode,
            lat,
            lon,
            0.0
        );

        const SurfacePositionHandle h = _attachedNode->calculateSurfacePositionHandle(
            groundPos
        );

        _position = cartesianCoordinatesFromGeo(
            *_attachedNode,
            lat,
            lon,
            h.heightToSurface + alt
        );
    }
    else {
        _position = cartesianCoordinatesFromGeo(
            *_attachedNode,
            lat,
            lon,
            alt
        );
        _positionIsDirty = false;
    }
    return _position;
}

} // namespace openspace
