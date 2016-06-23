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

#ifndef __AABB_H__
#define __AABB_H__

#include <memory>
#include <glm/glm.hpp>

// open space includes



namespace openspace {

    using namespace glm;

    enum class AABBSpatialRelation {
        None,
        Intersecting,
        Contained,
        Containing
    };

    struct AABB1 {
        AABB1();
        AABB1(float min, float max);

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
        AABB2();
        AABB2(const vec2& min, const vec2& max);

        void expand(const vec2& p);
        vec2 center() const;
        vec2 size() const;
        bool contains(const vec2& p) const;
        bool contains(const AABB2& o) const;
        bool intersects(const AABB2& o) const;
        AABBSpatialRelation relationTo(const AABB2& o) const;

        vec2 min;
        vec2 max;
    };


    struct AABB3 {
        AABB3();
        AABB3(const vec3& min, const vec3& max);

        void expand(const vec3 p);
        vec3 center() const;
        vec3 size() const;
        bool contains(const vec3& p) const;
        bool contains(const AABB3& o) const;
        bool intersects(const AABB3& o) const;
        AABBSpatialRelation relationTo(const AABB3& o) const;


        vec3 min;
        vec3 max;
    };
   
}  // namespace openspace

#endif  // __AABB_H__