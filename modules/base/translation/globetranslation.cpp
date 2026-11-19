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

#include <modules/base/translation/globetranslation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/format.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/dictionary.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/geodetic.h>
#include <openspace/util/updatestructures.h>
#include <optional>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "GlobeTranslation";

    constexpr StringPropertyInfo GlobeInfo = {
        .identifier = "Globe",
        .name = "Attached globe",
        .description = "The node on which the longitude/latitude is specified. If the "
            "node is a globe, the correct height information for the globe is used. "
            "Otherwise, the position is specified based on the longitude and latitude on "
            "the node's interaction sphere. If not set, the parent of the scene graph "
            "node that this translation is attached to is used.",
        .visibility = Visibility::User
    };

    constexpr DoublePropertyInfo LatitudeInfo = {
        .identifier = "Latitude",
        .name = "Latitude",
        .description = "The latitude of the location on the globe's surface. The value "
            "can range from -90 to 90, with negative values representing the southern "
            "hemisphere of the globe.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0.0,
            .minMax = { -90.0, 90.0 }
        }
    };

    constexpr DoublePropertyInfo LongitudeInfo = {
        .identifier = "Longitude",
        .name = "Longitude",
        .description = "The longitude of the location on the globe's surface. The value "
            "can range from -180 to 180, with negative values representing the western "
            "hemisphere of the globe.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0.0,
            .minMax = { -180.0, 180.0 }
        }
    };

    constexpr DoublePropertyInfo AltitudeInfo = {
        .identifier = "Altitude",
        .name = "Altitude",
        .description = "The altitude in meters. If the 'UseHeightmap' property is "
            "'true', this is an offset from the actual surface of the globe. If not, "
            "this is an offset from the reference ellipsoid.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0.0,
            .minMax = { -1e12, 1e12 }
        }
    };

    constexpr BoolPropertyInfo UseHeightmapInfo = {
        .identifier = "UseHeightmap",
        .name = "Use heightmap",
        .description = "If this value is 'true', the altitude specified in 'Altitude' "
            "will be treated as an offset from the heightmap. Otherwise, it will be an "
            "offset from the globe's reference ellipsoid.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo UseCameraInfo = {
        .identifier = "UseCamera",
        .name = "Use camera",
        .description = "If this value is 'true', the lat and lon are updated to match "
            "the camera.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo UseCameraAltitudeInfo = {
        .identifier = "UseCameraAltitude",
        .name = "Use camera altitude",
        .description = "If this value is 'true', the altitude is updated to match the "
            "camera.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = false
        }
    };

    // Places the scene graph node at a specific location relative to another scene graph
    // node. The position is identified via the longitude, latitude, and altitude
    // parameters. If the provided scene graph node is a globe, the positions correspond
    // to the native coordinate frame of that object. If it is a node without a renderable
    // or a non-globe renderable, the latitude/longitude grid used is the node's
    // interaction sphere.
    //
    // This class is useful in conjunction with the
    // [GlobeRotation](#base_rotation_globe) rotation to orient a scene graph node away
    // from the center of the body.
    //
    // If the `UseCamera` value is set, the object's position automatically updates based
    // on the current camera location.
    struct [[codegen::Dictionary(GlobeTranslation)]] Parameters {
        std::optional<std::string> globe
            [[codegen::Info(GlobeInfo), codegen::identifier()]];

        std::optional<double> latitude
            [[codegen::Info(LatitudeInfo), codegen::inrange(-90.0, 90.0)]];

        std::optional<double> longitude
            [[codegen::Info(LongitudeInfo), codegen::inrange(-180.0, 180.0)]];

        std::optional<double> altitude [[codegen::Info(AltitudeInfo)]];

        std::optional<bool> useHeightmap [[codegen::Info(UseHeightmapInfo)]];

        std::optional<bool> useCamera [[codegen::Info(UseCameraInfo)]];

        std::optional<bool> useCameraAltitude [[codegen::Info(UseCameraAltitudeInfo)]];
    };
} // namespace
#include "globetranslation_codegen.cpp"

namespace openspace {

Documentation GlobeTranslation::Documentation() {
    return codegen::doc<Parameters>(
        "base_translation_globe",
        Translation::Documentation()
    );
}

GlobeTranslation::GlobeTranslation(const Dictionary& dictionary)
    : Translation(dictionary)
    , _sceneGraphNode(GlobeInfo)
    , _latitude(LatitudeInfo)
    , _longitude(LongitudeInfo)
    , _altitude(AltitudeInfo)
    , _useHeightmap(UseHeightmapInfo)
    , _useCamera(UseCameraInfo)
    , _useCameraAltitude(UseCameraAltitudeInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sceneGraphNode = p.globe.value_or("");
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

void GlobeTranslation::setOwnerNode(SceneGraphNode* node) {
    _ownerNode = node;
}

void GlobeTranslation::fillAttachedNode() {
    if (std::string_view(_sceneGraphNode).empty()) {
        // If the 'Globe' property is not set, use the parent of the scene graph node
        // that this translation is attached to
        _attachedNode = _ownerNode ? _ownerNode->parent() : nullptr;
        return;
    }
    SceneGraphNode* n = sceneGraphNode(_sceneGraphNode);
    if (!n) {
        LERROR(std::format("Could not find attached node '{}'", _sceneGraphNode.value()));
        return;
    }
    _attachedNode = n;
}

void GlobeTranslation::setUpdateVariables() {
    _positionIsDirty = true;
    requireUpdate();
}

void GlobeTranslation::initialize() {
    Translation::initialize();
    fillAttachedNode();
}

void GlobeTranslation::update(const UpdateData& data) {
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

    if (!_attachedNode) [[unlikely]] {
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
        const glm::vec3 p = cartesianCoordinatesFromGeo(*_attachedNode, lat, lon, 0.0);
        const SurfacePositionHandle h = _attachedNode->calculateSurfacePositionHandle(p);

        _position = cartesianCoordinatesFromGeo(
            *_attachedNode,
            lat,
            lon,
            h.heightToSurface + alt
        );
    }
    else {
        _position = cartesianCoordinatesFromGeo(*_attachedNode, lat, lon, alt);
        _positionIsDirty = false;
    }
    return _position;
}

} // namespace openspace
