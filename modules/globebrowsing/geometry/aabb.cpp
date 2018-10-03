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

#include <modules/globebrowsing/geometry/aabb.h>

#include <limits>

namespace openspace::globebrowsing {

AABB3::AABB3(glm::vec3 minValue, glm::vec3 maxValue)
    : min(std::move(minValue))
    , max(std::move(maxValue))
{}

void AABB3::expand(const glm::vec3& p) {
    min = glm::min(min, p);
    max = glm::max(max, p);
}

glm::vec3 AABB3::center() const {
    return 0.5f * (min + max);
}

glm::vec3 AABB3::size() const {
    return max - min;
}

bool AABB3::contains(const glm::vec3& p) const {
    return (min.x <= p.x) && (p.x <= max.x)
        && (min.y <= p.y) && (p.y <= max.y)
        && (min.z <= p.z) && (p.z <= max.z);
}

bool AABB3::contains(const AABB3& o) const {
    return (min.x <= o.min.x) && (o.max.x <= max.x)
        && (min.y <= o.min.y) && (o.max.y <= max.y)
        && (min.z <= o.min.z) && (o.max.z <= max.z);
}

bool AABB3::intersects(const AABB3& o) const {
    return (min.x <= o.max.x) && (o.min.x <= max.x)
        && (min.y <= o.max.y) && (o.min.y <= max.y)
        && (min.z <= o.max.z) && (o.min.z <= max.z);
}

} // namespace openspace::globebrowsing
