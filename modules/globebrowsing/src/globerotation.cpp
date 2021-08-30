/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>

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

    constexpr openspace::properties::Property::PropertyInfo AngleInfo = {
        "Angle",
        "Angle",
        "A rotation angle that can be used to rotate the object around its own y-axis, "
        "which will be pointing out of the globe's surface. "
    };

    struct [[codegen::Dictionary(GlobeRotation)]] Parameters {
        // [[codegen::verbatim(GlobeInfo.description)]]
        std::string globe
            [[codegen::annotation("A valid scene graph node with a RenderableGlobe")]];

        // [[codegen::verbatim(LongitudeInfo.description)]]
        std::optional<double> longitude;

        // [[codegen::verbatim(LatitudeInfo.description)]]
        std::optional<double> latitude;

        // [[codegen::verbatim(AngleInfo.description)]]
        std::optional<double> angle;
    };
#include "globerotation_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GlobeRotation::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_rotation_globerotation");
}

GlobeRotation::GlobeRotation(const ghoul::Dictionary& dictionary)
    : _globe(GlobeInfo)
    , _longitude(LongitudeInfo, 0.0, -180.0, 180.0)
    , _latitude(LatitudeInfo, 0.0, -90.0, 90.0)
    , _angle(AngleInfo, 0.0, 0.0, 360.0)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _globe = p.globe;
    _globe.onChange([this]() {
        findGlobe();
        _matrixIsDirty = true;
    });

    _longitude = p.longitude.value_or(_longitude);
    _longitude.onChange([this]() { _matrixIsDirty = true; });
    addProperty(_longitude);

    _latitude = p.latitude.value_or(_latitude);
    _latitude.onChange([this]() { _matrixIsDirty = true; });
    addProperty(_latitude);

    _angle = p.angle.value_or(_angle);
    _angle.onChange([this]() { _matrixIsDirty = true; });
    addProperty(_angle);
}

void GlobeRotation::findGlobe() {
    SceneGraphNode* n = sceneGraphNode(_globe);
    if (n->renderable() && dynamic_cast<RenderableGlobe*>(n->renderable())) {
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

glm::dmat3 GlobeRotation::matrix(const UpdateData&) const {
    if (!_matrixIsDirty) {
        return _matrix;
    }

    // Compute vector outwards of globe
    float latitudeRad = glm::radians(static_cast<float>(_latitude));
    float longitudeRad = glm::radians(static_cast<float>(_longitude));

    glm::dvec3 yAxis = _globeNode->ellipsoid().geodeticSurfaceNormal(
        { latitudeRad, longitudeRad }
    );
    yAxis = glm::normalize(yAxis);

    const glm::dvec3 n = glm::vec3(0.0, 0.0, 1.0);
    glm::dvec3 zAxis = glm::dvec3(0.0);
    zAxis.x = yAxis.y * n.z - yAxis.z * n.y;
    zAxis.y = yAxis.z * n.x - yAxis.x * n.z;
    zAxis.z = yAxis.x * n.y - yAxis.y * n.x;
    zAxis = glm::normalize(zAxis);

    glm::dvec3 xAxis = glm::normalize(glm::cross(yAxis, zAxis));
    const glm::dmat3 mat = {
        xAxis.x, xAxis.y, xAxis.z,
        yAxis.x, yAxis.y, yAxis.z,
        zAxis.x, zAxis.y, zAxis.z
    };

    const glm::dquat q = glm::angleAxis(glm::radians(_angle.value()), yAxis);
    _matrix = glm::toMat3(q) * mat;
    _matrixIsDirty = false;
    return _matrix;
}

} // namespace openspace::globebrowsing
