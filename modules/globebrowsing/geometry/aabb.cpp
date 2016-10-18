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


#include <modules/globebrowsing/geometry/aabb.h>

#include <string>

namespace {
    const std::string _loggerCat = "AABB";
}

namespace openspace {
namespace globebrowsing {

    AABB1::AABB1() : min(1e35), max(-1e35) { }
    AABB1::AABB1(float min, float max) : min(min), max(max) { }

    void AABB1::expand(float p) {
        min = glm::min(min, p);
        max = glm::max(max, p);
    }

    float AABB1::center() const {
        return 0.5f * (min + max);
    }

    float AABB1::size() const {
        return max - min;
    }

    bool AABB1::contains(float  p) const {
        return (min <= p) && (p <= max);
    }

    bool AABB1::contains(const AABB1& o) const {
        return (min <= o.min) && (o.max <= max);
    }

    bool AABB1::intersects(const AABB1& o) const {
        return (min <= o.max) && (o.min <= max);
    }

    AABBSpatialRelation AABB1::relationTo(const AABB1& o) const {
        if (intersects(o)) {
            if (contains(o)) return AABBSpatialRelation::Containing;
            if (o.contains(*this)) return AABBSpatialRelation::Contained;
            return AABBSpatialRelation::Intersecting;
        }
        return AABBSpatialRelation::None;
    }

    AABB2::AABB2() : min(1e35), max(-1e35) { }
    AABB2::AABB2(const vec2& min, const vec2& max) : min(min), max(max) { }

    void AABB2::expand(const vec2& p) {
        min.x = glm::min(min.x, p.x);
        min.y = glm::min(min.y, p.y);
        max.x = glm::max(max.x, p.x);
        max.y = glm::max(max.y, p.y);
    }

    vec2 AABB2::center() const {
        return 0.5f * (min + max);
    }

    vec2 AABB2::size() const {
        return max - min;
    }

    bool AABB2::contains(const vec2& p) const {
        return (min.x <= p.x) && (p.x <= max.x)
            && (min.y <= p.y) && (p.y <= max.y);
    }

    bool AABB2::contains(const AABB2& o) const {
        return (min.x <= o.min.x) && (o.max.x <= max.x)
            && (min.y <= o.min.y) && (o.max.y <= max.y);
    }

    bool AABB2::intersects(const AABB2& o) const {
        return (min.x <= o.max.x) && (o.min.x <= max.x)
            && (min.y <= o.max.y) && (o.min.y <= max.y);
    }

    AABBSpatialRelation AABB2::relationTo(const AABB2& o) const {
        if (intersects(o)) {
            if (contains(o)) return AABBSpatialRelation::Containing;
            if (o.contains(*this)) return AABBSpatialRelation::Contained;
            return AABBSpatialRelation::Intersecting;
        }
        return AABBSpatialRelation::None;
    }

    AABB3::AABB3() : min(1e35), max(-1e35) { }
    AABB3::AABB3(const vec3& min, const vec3& max) : min(min), max(max) { }

    void AABB3::expand(const vec3 p) {
        min.x = glm::min(min.x, p.x);
        min.y = glm::min(min.y, p.y);
        min.z = glm::min(min.z, p.z);
        max.x = glm::max(max.x, p.x);
        max.y = glm::max(max.y, p.y);
        max.z = glm::max(max.z, p.z);
    }

    vec3 AABB3::center() const {
        return 0.5f * (min + max);
    }

    vec3 AABB3::size() const {
        return max - min;
    }

    bool AABB3::contains(const vec3& p) const {
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

    AABBSpatialRelation AABB3::relationTo(const AABB3& o) const {
        if (intersects(o)) {
            if (contains(o)) return AABBSpatialRelation::Containing;
            if (o.contains(*this)) return AABBSpatialRelation::Contained;
            return AABBSpatialRelation::Intersecting;
        }
        return AABBSpatialRelation::None;
    }

} // namespace globebrowsing
} // namespace openspace
