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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___CONVEXHULL___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___CONVEXHULL___H__

#include <openspace/glm.h>
#include <optional>
#include <vector>

namespace openspace::exoplanets {

struct ConvexHull {
    /// One plane per face: outward unit normal in xyz and offset in w
    std::vector<glm::dvec4> planes;

    /// Triangle list of the faces, wound counter-clockwise when viewed from outside
    std::vector<glm::dvec3> triangles;

    /// Distance outside the faces that still counts as inside
    double tolerance = 0.0;

    bool contains(const glm::dvec3& point) const;
};

/**
 * Computes the convex hull of \p points with an incremental algorithm. Returns
 * std::nullopt if there are fewer than four points or if they are (nearly) coplanar.
 */
std::optional<ConvexHull> computeConvexHull(const std::vector<glm::dvec3>& points);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___CONVEXHULL___H__
