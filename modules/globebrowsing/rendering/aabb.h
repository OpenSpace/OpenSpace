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


    struct AABB2 {
        AABB2();
        AABB2(const dvec2& min, const dvec2& max);

        void expand(const dvec2& p);
        dvec2 center() const;
        dvec2 size() const;
        bool intersects(const dvec2& p) const;
        bool intersects(const AABB2& o) const;

        dvec2 min;
        dvec2 max;
    };


    struct AABB3 {
        AABB3();
        AABB3(const dvec3& min, const dvec3& max);

        void expand(const dvec3 p);
        dvec3 center() const;
        dvec3 size() const;
        bool intersects(const dvec3& p) const;
        bool intersects(const AABB3& o) const;

        dvec3 min;
        dvec3 max;
    };
   
}  // namespace openspace

#endif  // __AABB_H__