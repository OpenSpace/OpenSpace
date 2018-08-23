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

#include <modules/volume/rendering/volumeclipplanes.h>

#include <ghoul/misc/dictionary.h>

namespace openspace::volume {

VolumeClipPlane::VolumeClipPlane(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "" }) // @TODO Missing name
    , _normal(
        { "Normal", "Normal", "" }, // @TODO Missing documentation
        glm::vec3(1.f, 0.f, 0.f),
        glm::vec3(-1.f),
        glm::vec3(1.f)
    )
    , _offsets(
        { "Offsets", "Offsets", "" }, // @TODO Missing documentation
        glm::vec2(-2.f, 0.f),
        glm::vec2(-2.f, 0.f),
        glm::vec2(2.f, 1.f)
    )
{
    _normal = dictionary.value<glm::vec3>("Normal");
    _offsets = dictionary.value<glm::vec2>("Offsets");

    addProperty(_normal);
    addProperty(_offsets);
}

glm::vec3 VolumeClipPlane::normal() const {
    return glm::normalize(_normal.value());
}

glm::vec2 VolumeClipPlane::offsets() const {
    return _offsets;
}

} // namespace openspace::volume
