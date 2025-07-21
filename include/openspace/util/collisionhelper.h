/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___COLLISIONHELPER___H__
#define __OPENSPACE_CORE___COLLISIONHELPER___H__

#include <ghoul/glm.h>

namespace openspace::collision {

/**
 * Calculate the intersection of a line segment and a sphere. The line segment is defined
 * from \p p1 to \p p2. The sphere is defined by the radius \p r and center point
 * \p center. The resulting intersection point is stored in the \p intersectionPoint
 * parameter.
 *
 * In the case of two intersection points, only care about the first one.
 *
 * \param p1 The start point for the line segment
 * \param p2 The end point for the line segment
 * \param center The center point for the sphere
 * \param r The radius of the sphere
 * \param intersectionPoint A variable to store the resulting intersection point in
 * \return True if the line between \p p1 and \p p2 intersects the sphere given by
 *         \p r and \p center, and false otherwise
 */
bool lineSphereIntersection(const glm::dvec3& p1, const glm::dvec3& p2,
    const glm::dvec3& center, double r, glm::dvec3& intersectionPoint);

/**
 * Check if the point \p p is inside of the sphere defined by radius \p r and center
 * point \p c
 */
bool isPointInsideSphere(const glm::dvec3& p, const glm::dvec3& c, double r);

} // namespace openspace::collision

#endif // __OPENSPACE_CORE___COLLISIONHELPER___H__
