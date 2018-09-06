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

AABB1::AABB1(float minValue, float maxValue)
    : min(minValue)
    , max(maxValue)
{}

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

bool AABB1::contains(float p) const {
    return (min <= p) && (p <= max);
}

bool AABB1::contains(const AABB1& o) const {
    return (min <= o.min) && (o.max <= max);
}

bool AABB1::intersects(const AABB1& o) const {
    return (min <= o.max) && (o.min <= max);
}

AABB1::AABBSpatialRelation AABB1::relationTo(const AABB1& o) const {
    if (intersects(o)) {
        if (contains(o)) {
            return AABB1::AABBSpatialRelation::Containing;
        }
        if (o.contains(*this)) {
            return AABB1::AABBSpatialRelation::Contained;
        }
        return AABB1::AABBSpatialRelation::Intersecting;
    }
    return AABB1::AABBSpatialRelation::None;
}

AABB2::AABB2(glm::vec2 minValue, glm::vec2 maxValue)
    : min(std::move(minValue))
    , max(std::move(maxValue))
{}

void AABB2::expand(const glm::vec2& p) {
    min = glm::min(min, p);
    max = glm::max(max, p);
}

glm::vec2 AABB2::center() const {
    return 0.5f * (min + max);
}

glm::vec2 AABB2::size() const {
    return max - min;
}

bool AABB2::contains(const glm::vec2& p) const {
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

AABB2::AABBSpatialRelation AABB2::relationTo(const AABB2& o) const {
    if (intersects(o)) {
        if (contains(o)) {
            return AABB2::AABBSpatialRelation::Containing;
        }
        if (o.contains(*this)) {
            return AABB2::AABBSpatialRelation::Contained;
        }
        return AABB2::AABBSpatialRelation::Intersecting;
    }
    return AABB2::AABBSpatialRelation::None;
}

AABB3::AABB3(glm::vec3 minValue, glm::vec3 maxValue)
    : min(std::move(minValue))
    , max(std::move(maxValue))
{}

void AABB3::expand(glm::vec3 p) {
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

AABB3::AABBSpatialRelation AABB3::relationTo(const AABB3& o) const {
    if (intersects(o)) {
        if (contains(o)) {
            return AABB3::AABBSpatialRelation::Containing;
        }
        if (o.contains(*this)) {
            return AABB3::AABBSpatialRelation::Contained;
        }
        return AABB3::AABBSpatialRelation::Intersecting;
    }
    return AABB3::AABBSpatialRelation::None;
}

} // namespace openspace::globebrowsing
