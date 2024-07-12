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

#include <modules/globebrowsing/src/globetranslation.h>

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

    struct [[codegen::Dictionary(GlobeTranslation)]] Parameters {
        // [[codegen::verbatim(GlobeInfo.description)]]
        std::string globe
            [[codegen::annotation("A valid scene graph node with a RenderableGlobe")]];

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

namespace openspace::globebrowsing {

documentation::Documentation GlobeTranslation::Documentation() {
    return codegen::doc<Parameters>("space_translation_globetranslation");
}

GlobeTranslation::GlobeTranslation(const ghoul::Dictionary& dictionary)
    : _globe(GlobeInfo)
    , _latitude(LatitudeInfo, 0.0, -90.0, 90.0)
    , _longitude(LongitudeInfo, 0.0, -180.0, 180.0)
    , _altitude(AltitudeInfo, 0.0, -1e12, 1e12)
    , _useHeightmap(UseHeightmapInfo, false)
    , _useCamera(UseCameraInfo, false)
    , _useCameraAltitude(UseCameraAltitudeInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _globe = p.globe;
    _globe.onChange([this]() {
        fillAttachedNode();
        setUpdateVariables();
    });
    addProperty(_globe);

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
    SceneGraphNode* n = sceneGraphNode(_globe);
    if (n && n->renderable() && dynamic_cast<RenderableGlobe*>(n->renderable())) {
        _attachedNode = dynamic_cast<RenderableGlobe*>(n->renderable());
    }
    else {
        LERRORC(
            "GlobeTranslation",
            "Could not set attached node as it does not have a RenderableGlobe"
        );
        if (_attachedNode) {
            // Reset the globe name to its previous name
            _globe = _attachedNode->identifier();
        }
    }
}

void GlobeTranslation::setUpdateVariables() {
    _positionIsDirty = true;
    requireUpdate();
}

void GlobeTranslation::update(const UpdateData& data) {
    if (_useHeightmap || _useCamera) {
        // If we use the heightmap, we have to compute the height every frame
        setUpdateVariables();
    }

    Translation::update(data);
}

glm::dvec3 GlobeTranslation::position(const UpdateData&) const {
    if (!_attachedNode) {
        // @TODO(abock): The const cast should be removed on a redesign of the translation
        //               to make the position function not constant. Const casting this
        //               away is fine as the factories that create the translations don't
        //               create them as constant objects
        const_cast<GlobeTranslation*>(this)->fillAttachedNode();
        _positionIsDirty = true;
    }

    if (!_positionIsDirty) {
        return _position;
    }

    if (!_attachedNode) {
        LERRORC(
            "GlobeRotation",
            std::format("Could not find attached node '{}'", _globe.value())
        );
        return _position;
    }

    GlobeBrowsingModule* mod = global::moduleEngine->module<GlobeBrowsingModule>();

    double lat = _latitude;
    double lon = _longitude;
    double alt = _altitude;

    if (_useCamera) {
        const glm::dvec3 position = mod->geoPosition();
        lat = position.x;
        lon = position.y;
        if (_useCameraAltitude) {
            alt = position.z;
        }
    }

    if (_useHeightmap) {
        const glm::vec3 groundPos = mod->cartesianCoordinatesFromGeo(
            *_attachedNode,
            lat,
            lon,
            0.0
        );

        const SurfacePositionHandle h = _attachedNode->calculateSurfacePositionHandle(
            groundPos
        );

        _position = mod->cartesianCoordinatesFromGeo(
            *_attachedNode,
            lat,
            lon,
            h.heightToSurface + alt
        );
        return _position;
    }
    else {
        _position = mod->cartesianCoordinatesFromGeo(
            *_attachedNode,
            lat,
            lon,
            alt
        );
        _positionIsDirty = false;
        return _position;
    }
}

} // namespace openspace::globebrowsing
