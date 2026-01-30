
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

#include <modules/volume/rendering/volumeclipplanes.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/misc/dictionary.h>
#include <utility>

namespace {

    //constexpr openspace::properties::PropertyOwner::PropertyOwnerInfo ClipPlanesInfo = {
    //"ClipPlanes",
    //"Clip Planes",
    //"Documentation"
    //};


    constexpr openspace::properties::Property::PropertyInfo NClipPlanesInfo = {
        "nClipPlanes",
        "# Clip Planes",
        "Number of clip planes"
    };

} // namespace

namespace openspace::volume {



VolumeClipPlanes::VolumeClipPlanes(const std::vector<ghoul::Dictionary>& planes)
    : properties::PropertyOwner({ "ClipPlanes", "Clip Planes" }) // @TODO Missing name
    // @TODO Missing documentation
    , _nClipPlanes(NClipPlanesInfo, 0, 0, 10)
{
    int index = 0;
    for (const ghoul::Dictionary& c : planes) {
        std::unique_ptr<VolumeClipPlane> clipPlane = std::make_unique<VolumeClipPlane>(c);
        clipPlane->setIdentifier(std::format("clipPlane_{}", index++)); // TODO 2025-05-06 check if this is ok to do with Alex / Emma
        addPropertySubOwner(clipPlane.get());
        _clipPlanes.push_back(std::move(clipPlane));

    }

    _nClipPlanes = static_cast<int>(_clipPlanes.size());
    addProperty(_nClipPlanes);
}

void VolumeClipPlanes::initialize() {
}

std::vector<glm::vec3> VolumeClipPlanes::normals() {
    std::vector<glm::vec3> normals;
    normals.reserve(_clipPlanes.size());
    for (const std::unique_ptr<VolumeClipPlane>& clipPlane : _clipPlanes) {
        normals.push_back(clipPlane->normal());
    }
    return normals;
}

std::vector<glm::vec2> VolumeClipPlanes::offsets() {
    std::vector<glm::vec2> offsets;
    offsets.reserve(_clipPlanes.size());
    for (const std::unique_ptr<VolumeClipPlane>& clipPlane : _clipPlanes) {
        offsets.push_back(clipPlane->offsets());
    }
    return offsets;
}

} // namespace openspace::volume
