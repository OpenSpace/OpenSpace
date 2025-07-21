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

#ifndef __OPENSPACE_MODULE_VOLUME___RAWVOLUMEMETADATA___H__
#define __OPENSPACE_MODULE_VOLUME___RAWVOLUMEMETADATA___H__

#include <modules/volume/volumegridtype.h>
#include <ghoul/glm.h>

namespace openspace::documentation { struct Documentation; }
namespace ghoul { class Dictionary; }

namespace openspace::volume {

struct RawVolumeMetadata {
    static RawVolumeMetadata createFromDictionary(const ghoul::Dictionary& dictionary);
    static documentation::Documentation Documentation();

    ghoul::Dictionary dictionary() const;

    glm::uvec3 dimensions = glm::uvec3(0);
    VolumeGridType gridType;

    bool hasTime = false;
    double time = 0.0;

    bool hasValueRange = false;
    float minValue = 0.f;
    float maxValue = 0.f;
    bool hasValueUnit = false;
    std::string valueUnit;

    bool hasDomainBounds = false;
    glm::vec3 lowerDomainBound = glm::vec3(0.f);
    glm::vec3 upperDomainBound = glm::vec3(0.f);
    bool hasDomainUnit = false;
    std::string domainUnit;
};

} // namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___RAWVOLUMEMETADATA___H__
