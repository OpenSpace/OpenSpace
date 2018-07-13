/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/base/lightsource/cameralightsource.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo IntensityInfo = {
        "Intensity",
        "Intensity",
        "The intensity of this light source"
    };
} // namespace

namespace openspace {

documentation::Documentation CameraLightSource::Documentation() {
    using namespace openspace::documentation;
    return {
        "Camera Light Source",
        "base_camera_light_source",
        {
            {
                "Type",
                new StringEqualVerifier("CameraLightSource"),
                Optional::No,
                "The type of this light source"
            },
            {
                IntensityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                IntensityInfo.description
            }
        }
    };
}

CameraLightSource::CameraLightSource()
    : LightSource()
    , _intensity(IntensityInfo, 1.f, 0.f, 1.f)
{
    addProperty(_intensity);
}

CameraLightSource::CameraLightSource(const ghoul::Dictionary& dictionary)
    : LightSource(dictionary)
    , _intensity(IntensityInfo, 1.f, 0.f, 1.f)
{
    addProperty(_intensity);

    documentation::testSpecificationAndThrow(Documentation(),
                                             dictionary,
                                             "CameraLightSource");


    if (dictionary.hasValue<double>(IntensityInfo.identifier)) {
        _intensity = static_cast<float>(
            dictionary.value<double>(IntensityInfo.identifier)
        );
    }
}

float CameraLightSource::intensity() const {
    return _intensity;
}

glm::vec3 CameraLightSource::directionViewSpace(const RenderData&) const {
    return glm::vec3(0.f, 0.f, 1.f);
}

} // namespace openspace
