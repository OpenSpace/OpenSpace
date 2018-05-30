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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___AABB___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___AABB___H__

#include <ghoul/glm.h>

namespace openspace::globebrowsing {

struct AABB1 {
    enum class AABBSpatialRelation {
        None,
        Intersecting,
        Contained,
        Containing
    };

    AABB1(float minValue = std::numeric_limits<float>::max(),
        float maxValue = -std::numeric_limits<float>::max());

    void expand(float p);
    float center() const;
    float size() const;
    bool contains(float p) const;
    bool contains(const AABB1& o) const;
    bool intersects(const AABB1& o) const;
    AABBSpatialRelation relationTo(const AABB1& o) const;

    float min;
    float max;
};

struct AABB2 {
    enum class AABBSpatialRelation {
        None,
        Intersecting,
        Contained,
        Containing
    };

    AABB2(glm::vec2 minValue = glm::vec2(std::numeric_limits<float>::max()),
        glm::vec2 maxValue = glm::vec2(-std::numeric_limits<float>::max()));

    void expand(const glm::vec2& p);
    glm::vec2 center() const;
    glm::vec2 size() const;
    bool contains(const glm::vec2& p) const;
    bool contains(const AABB2& o) const;
    bool intersects(const AABB2& o) const;
    AABBSpatialRelation relationTo(const AABB2& o) const;

    glm::vec2 min;
    glm::vec2 max;
};

struct AABB3 {
    enum class AABBSpatialRelation {
        None,
        Intersecting,
        Contained,
        Containing
    };

    AABB3(glm::vec3 minValue = glm::vec3(std::numeric_limits<float>::max()),
        glm::vec3 maxValue = glm::vec3(-std::numeric_limits<float>::max()));

    void expand(const glm::vec3 p);
    glm::vec3 center() const;
    glm::vec3 size() const;
    bool contains(const glm::vec3& p) const;
    bool contains(const AABB3& o) const;
    bool intersects(const AABB3& o) const;
    AABBSpatialRelation relationTo(const AABB3& o) const;

    glm::vec3 min;
    glm::vec3 max;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___AABB___H__
