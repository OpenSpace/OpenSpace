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

    constexpr openspace::properties::Property::PropertyInfo FixedAltitudeInfo = {
        "FixedAltitude",
        "Fixed Altitude",
        "The altitude in meters of the location on the globe's surface. This value is "
        "used if the 'UseFixedAltitude' property is 'true'. The default value is 10000km."
    };

    constexpr openspace::properties::Property::PropertyInfo UseFixedAltitudeInfo = {
        "UseFixedAltitude",
        "Use Fixed Altitude",
        "If this value is 'true', the altitude specified in 'FixedAltitude' is used for "
        "this translation. If it is 'false', the altitude will be computed based on the "
        "height information that is available about the globe to which this translation "
        "is attached. The default value is 'true'."
    };
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GlobeTranslation::Documentation() {
    using namespace openspace::documentation;

    return {
        "Spice Translation",
        "space_translation_spicetranslation",
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
                FixedAltitudeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FixedAltitudeInfo.description
            },
            {
                UseFixedAltitudeInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                UseFixedAltitudeInfo.description
            }
        }
    };
}

GlobeTranslation::GlobeTranslation(const ghoul::Dictionary& dictionary)
    : _globe(GlobeInfo)
    , _longitude(LongitudeInfo, 0.0, -180.0, 180.0)
    , _latitude(LatitudeInfo, 0.0, -90.0, 90.0)
    , _fixedAltitude(FixedAltitudeInfo, 1e8, 0.0, 1e12)
    , _useFixedAltitude(UseFixedAltitudeInfo, false)
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
    if (dictionary.hasKey(FixedAltitudeInfo.identifier)) {
        _fixedAltitude = dictionary.value<double>(FixedAltitudeInfo.identifier);
    }
    if (dictionary.hasKey(UseFixedAltitudeInfo.identifier)) {
        _useFixedAltitude = dictionary.value<bool>(UseFixedAltitudeInfo.identifier);
    }

    _globe.onChange([this]() {
        fillAttachedNode();
        _positionIsDirty = true;
    });

    _longitude.onChange([this]() { _positionIsDirty = true; });
    _latitude.onChange([this]() { _positionIsDirty = true; });
    _fixedAltitude.onChange([this]() { _positionIsDirty = true; });
    _useFixedAltitude.onChange([this]() { _positionIsDirty = true; });

    addProperty(_longitude);
    addProperty(_latitude);
    addProperty(_fixedAltitude);
    addProperty(_useFixedAltitude);
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

    if (!_useFixedAltitude) {
        // If we don't use the fixed altitude, we have to compute the height every frame
        _positionIsDirty = true;
    }

    if (!_positionIsDirty) {
        return _position;
    }


    GlobeBrowsingModule& mod = *(global::moduleEngine.module<GlobeBrowsingModule>());

    glm::vec3 pos = mod.cartesianCoordinatesFromGeo(
        *_attachedNode,
        _latitude,
        _longitude,
        _fixedAltitude
    );

    if (_useFixedAltitude) {
        _position = glm::dvec3(pos);
        _positionIsDirty = true;

        return _position;
    }
    else {
        SurfacePositionHandle h = _attachedNode->calculateSurfacePositionHandle(pos);

        pos = mod.cartesianCoordinatesFromGeo(
            *_attachedNode,
            _latitude,
            _longitude,
            h.heightToSurface
        );

        _position = glm::dvec3(pos);
        return _position;
    }
}

} // namespace openspace::globebrowsing
