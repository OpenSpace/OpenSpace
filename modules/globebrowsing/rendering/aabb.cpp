/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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


#include <modules/globebrowsing/rendering/aabb.h>

#include <string>

namespace {
    const std::string _loggerCat = "AABB";
}

namespace openspace {


    AABB2::AABB2() : min(1e35), max(-1e35) { }
    AABB2::AABB2(const dvec2& min, const dvec2& max) : min(min), max(max) { }

    void AABB2::expand(const dvec2& p) {
        min.x = glm::min(min.x, p.x);
        min.y = glm::min(min.y, p.y);
        max.x = glm::max(max.x, p.x);
        max.y = glm::max(max.y, p.y);
    }

    dvec2 AABB2::center() const {
        return 0.5 * (min + max);
    }

    dvec2 AABB2::size() const {
        return max - min;
    }

    bool AABB2::intersects(const dvec2& p) const {
        return (min.x < p.x) && (p.x < max.x)
            && (min.y < p.y) && (p.y < max.y);
    }

    bool AABB2::intersects(const AABB2& o) const {
        return (min.x < o.max.x) && (o.min.x < max.x)
            && (min.y < o.max.y) && (o.min.y < max.y);
    }





    AABB3::AABB3() : min(1e35), max(-1e35) { }
    AABB3::AABB3(const dvec3& min, const dvec3& max) : min(min), max(max) { }

    void AABB3::expand(const dvec3 p) {
        min.x = glm::min(min.x, p.x);
        min.y = glm::min(min.y, p.y);
        min.z = glm::min(min.z, p.z);
        max.x = glm::max(max.x, p.x);
        max.y = glm::max(max.y, p.y);
        max.z = glm::max(max.z, p.z);
    }

    dvec3 AABB3::center() const {
        return 0.5 * (min + max);
    }

    dvec3 AABB3::size() const {
        return max - min;
    }

    bool AABB3::intersects(const dvec3& p) const {
        return (min.x < p.x) && (p.x < max.x)
            && (min.y < p.y) && (p.y < max.y)
            && (min.z < p.z) && (p.z < max.z);
    }

    bool AABB3::intersects(const AABB3& o) const {
        return (min.x < o.max.x) && (o.min.x < max.x)
            && (min.y < o.max.y) && (o.min.y < max.y)
            && (min.z < o.max.z) && (o.min.z < max.z);
    }


}  // namespace openspace
