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

#include "volumeutils.h"

namespace openspace::volumeutils {
    
size_t coordsToIndex(const glm::uvec3& coords, const glm::uvec3& dims) {
    size_t w = dims.x;
    size_t h = dims.y;
//    size_t d = dims.z;
//    
//    size_t x = coords.x;
//    size_t y = coords.y;
//    size_t z = coords.z;
    
    return coords.z * (h * w) + coords.y * w + coords.x;
}

glm::uvec3 indexToCoords(size_t index, const glm::uvec3& dims) {
    size_t w = dims.x;
    size_t h = dims.y;
    size_t d = dims.z;
    
    size_t x = index % w;
    size_t y = (index / w) % h;
    size_t z = index / w / h;
    
    return glm::uvec3(x, y, z);
}
    
} // namespace openspace::volumeutils
