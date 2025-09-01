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

#include <modules/volume/rendering/volumeclipplanes.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/misc/dictionary.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo NormalInfo = {
        "Normal",
        "Normal",
        // @TODO Missing documentation
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo OffsetsInfo = {
        "Offsets",
        "Offsets",
        // @TODO Missing documentation
        ""
    };

    struct [[codegen::Dictionary(VolumeClipPlane)]] Parameters {
        // [[codegen::verbatim(NormalInfo.description)]]
        glm::vec3 normal;

        // [[codegen::verbatim(OffsetsInfo.description)]]
        glm::vec3 offsets;
    };
#include "volumeclipplane_codegen.cpp"
} // namespace

namespace openspace::volume {

VolumeClipPlane::VolumeClipPlane(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "" }) // @TODO Missing name
    , _normal(
        NormalInfo,
        glm::vec3(1.f, 0.f, 0.f),
        glm::vec3(-1.f),
        glm::vec3(1.f)
    )
    , _offsets(
        OffsetsInfo,
        glm::vec2(-2.f, 0.f),
        glm::vec2(-2.f, 0.f),
        glm::vec2(2.f, 1.f)
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _normal = p.normal;
    addProperty(_normal);

    _offsets = p.offsets;
    addProperty(_offsets);
}

glm::vec3 VolumeClipPlane::normal() const {
    return glm::normalize(_normal.value());
}

glm::vec2 VolumeClipPlane::offsets() const {
    return _offsets;
}

} // namespace openspace::volume
