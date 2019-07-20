/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
        "The globe on which the longitude/latitude is specified"
    };

    constexpr openspace::properties::Property::PropertyInfo LongitudeInfo = {
        "Longitude",
        "Longitude",
        "The longitude of the location on the globe's surface. The value can range from "
        "-180 to 180, with negative values representing the western hemisphere of the "
        "globe. The default value is 0.0"
    };

    constexpr openspace::properties::Property::PropertyInfo LatitudeInfo = {
        "Latitude",
        "Latitude",
        "The latitude of the location on the globe's surface. The value can range from "
        "-90 to 90, with negative values representing the southern hemisphere of the "
        "globe. The default value is 0.0"
    };

    constexpr openspace::properties::Property::PropertyInfo AltitudeInfo = {
        "Altitude",
        "Altitude",
        "The altitude in meters. "
        "If the 'UseHeightmap' property is 'true', this is an offset from the actual "
        "surface of the globe. If not, this is an offset from the reference ellipsoid."
        "The default value is 0.0"
    };

    constexpr openspace::properties::Property::PropertyInfo UseHeightmapInfo = {
        "UseHeightmap",
        "Use Heightmap",
        "If this value is 'true', the altitude specified in 'Altitude' will be treated "
        "as an offset from the heightmap. Otherwise, it will be an offset from the "
        "globe's reference ellipsoid. The default value is 'false'."
    };
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GlobeTranslation::Documentation() {
    using namespace openspace::documentation;

    return {
        "Globe Translation",
        "space_translation_globetranslation",
        {
            {
                "Type",
                new StringEqualVerifier("GlobeTranslation"),
                Optional::No
            },
            {
                GlobeInfo.identifier,
                new StringAnnotationVerifier(
                    "A valid scene graph node with a RenderableGlobe"
                ),
                Optional::No,
                GlobeInfo.description
            },
            {
                LongitudeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LongitudeInfo.description
            },
            {
                LatitudeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LatitudeInfo.description,
            },
            {
                AltitudeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                AltitudeInfo.description
            },
            {
                UseHeightmapInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                UseHeightmapInfo.description
            }
        }
    };
}

GlobeTranslation::GlobeTranslation(const ghoul::Dictionary& dictionary)
    : _globe(GlobeInfo)
    , _longitude(LongitudeInfo, 0.0, -180.0, 180.0)
    , _latitude(LatitudeInfo, 0.0, -90.0, 90.0)
    , _altitude(AltitudeInfo, 0.0, 0.0, 1e12)
    , _useHeightmap(UseHeightmapInfo, false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "GlobeTranslation"
    );

    _globe = dictionary.value<std::string>(GlobeInfo.identifier);
    if (dictionary.hasKey(LongitudeInfo.identifier)) {
        _longitude = dictionary.value<double>(LongitudeInfo.identifier);
    }
    if (dictionary.hasKey(LatitudeInfo.identifier)) {
        _latitude = dictionary.value<double>(LatitudeInfo.identifier);
    }
    if (dictionary.hasKey(AltitudeInfo.identifier)) {
        _altitude = dictionary.value<double>(AltitudeInfo.identifier);
    }
    if (dictionary.hasKey(UseHeightmapInfo.identifier)) {
        _useHeightmap = dictionary.value<bool>(UseHeightmapInfo.identifier);
    }

    _globe.onChange([this]() {
        fillAttachedNode();
        _positionIsDirty = true;
    });

    _longitude.onChange([this]() { _positionIsDirty = true; });
    _latitude.onChange([this]() { _positionIsDirty = true; });
    _altitude.onChange([this]() { _positionIsDirty = true; });
    _useHeightmap.onChange([this]() { _positionIsDirty = true; });

    addProperty(_longitude);
    addProperty(_latitude);
    addProperty(_altitude);
    addProperty(_useHeightmap);
}

void GlobeTranslation::fillAttachedNode() {
    SceneGraphNode* n = sceneGraphNode(_globe);
    if (n->renderable() && dynamic_cast<RenderableGlobe*>(n->renderable())) {
        _attachedNode = dynamic_cast<RenderableGlobe*>(n->renderable());
    }
    else {
        LERRORC(
            "GlobeTranslation",
            "Could not set attached node as it does not have a RenderableGlobe"
        );
        if (_attachedNode) {
            // Reset the globe name to it's previous name
            _globe = _attachedNode->identifier();
        }
    }
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

    if (_useHeightmap) {
        // If we use the heightmap, we have to compute the height every frame
        _positionIsDirty = true;
    }

    if (!_positionIsDirty) {
        return _position;
    }

    GlobeBrowsingModule& mod = *(global::moduleEngine.module<GlobeBrowsingModule>());

    if (_useHeightmap) {
        glm::vec3 groundPos = mod.cartesianCoordinatesFromGeo(
            *_attachedNode,
            _latitude,
            _longitude,
            0.0
        );

        SurfacePositionHandle h =
            _attachedNode->calculateSurfacePositionHandle(groundPos);

        _position = mod.cartesianCoordinatesFromGeo(
            *_attachedNode,
            _latitude,
            _longitude,
            h.heightToSurface + _altitude
        );
        return _position;
    }
    else {
        _position = mod.cartesianCoordinatesFromGeo(
            *_attachedNode,
            _latitude,
            _longitude,
            _altitude
        );
        _positionIsDirty = false;
        return _position;
    }
}

} // namespace openspace::globebrowsing
