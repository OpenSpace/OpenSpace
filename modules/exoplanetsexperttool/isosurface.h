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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___ISOSURFACE___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___ISOSURFACE___H__

#include <openspace/glm.h>
#include <vector>

namespace openspace::exoplanets {

/**
 * Scalar field sampled on a uniform grid, with x varying fastest, then y, then z.
 */
struct VolumeGrid {
    glm::uvec3 dimensions = glm::uvec3(0);
    std::vector<float> values;

    float value(unsigned int x, unsigned int y, unsigned int z) const {
        return values[x + static_cast<size_t>(dimensions.x) *
            (y + static_cast<size_t>(dimensions.y) * z)];
    }

    /**
     * Returns the trilinearly interpolated value at \p position, given in grid index
     * coordinates, or 0 if the position lies outside the grid.
     */
    float interpolate(const glm::dvec3& position) const;
};

/**
 * Extracts the surface enclosing all regions where \p grid >= \p isoValue using
 * marching tetrahedra (a marching cubes variant without ambiguous cases). The result is
 * a triangle list in grid index coordinates, wound counter-clockwise when viewed from
 * the side with lower values.
 */
std::vector<glm::dvec3> extractIsosurface(const VolumeGrid& grid, float isoValue);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___ISOSURFACE___H__
