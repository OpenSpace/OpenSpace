
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

VolumeClipPlanes::VolumeClipPlanes(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "" }) // @TODO Missing name
    // @TODO Missing documentation
    , _nClipPlanes({ "nClipPlanes", "Number of clip planes", "" }, 0, 0, 10)
{
    const std::vector<std::string>& keys = dictionary.keys();
    for (const std::string& key : keys) {
        ghoul::Dictionary cutPlaneDictionary = dictionary.value<ghoul::Dictionary>(key);
        VolumeClipPlane clipPlane = VolumeClipPlane(cutPlaneDictionary);
        clipPlane.setIdentifier(key);
        _clipPlanes.push_back(std::move(clipPlane));
    }
    _nClipPlanes = static_cast<int>(keys.size());
}

void VolumeClipPlanes::initialize() {
    addProperty(_nClipPlanes);
    for (VolumeClipPlane& clipPlane : _clipPlanes) {
        addPropertySubOwner(clipPlane);
    }
}

void VolumeClipPlanes::deinitialize() {}

std::vector<glm::vec3> VolumeClipPlanes::normals() {
    std::vector<glm::vec3> normals;
    for (const VolumeClipPlane& clipPlane : _clipPlanes) {
        normals.push_back(clipPlane.normal());
    }
    return normals;
}

std::vector<glm::vec2> VolumeClipPlanes::offsets() {
    std::vector<glm::vec2> offsets;
    for (const VolumeClipPlane& clipPlane : _clipPlanes) {
        offsets.push_back(clipPlane.offsets());
    }
    return offsets;
}

} // namespace openspace::volume
