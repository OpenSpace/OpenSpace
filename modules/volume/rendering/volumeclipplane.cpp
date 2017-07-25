
/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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


namespace openspace {

VolumeClipPlane::VolumeClipPlane(const ghoul::Dictionary& dictionary)
    : _normal(
        "normal",
        "Normal",
        glm::vec3(1.f, 0.f, 0.f),
        glm::vec3(-1.f),
        glm::vec3(1.f)
    )
    , _offsets(
        "offsets",
        "Offsets",
        glm::vec2(-2.f, 0.f),
        glm::vec2(-2.f, 0.f),
        glm::vec2(2.f, 1.f)
    )
{
    glm::vec3 normal;
    glm::vec2 offsets;
    dictionary.getValue("Normal", normal);
    dictionary.getValue("Offsets", offsets);

    _normal = normal;
    _offsets = offsets;
}

void VolumeClipPlane::initialize() {
    addProperty(_normal);
    addProperty(_offsets);
}

glm::vec3 VolumeClipPlane::normal() {
    return glm::normalize(_normal.value());
}

glm::vec2 VolumeClipPlane::offsets() {
    return _offsets;
}

} // namespace openspace
