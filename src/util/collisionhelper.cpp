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

#include <openspace/util/collisionhelper.h>

#include <ghoul/misc/exception.h>

namespace openspace::collision {

// Source: http://paulbourke.net/geometry/circlesphere/raysphere.c
bool lineSphereIntersection(const glm::dvec3& p1, const glm::dvec3& p2,
                            const glm::dvec3& center, double r,
                            glm::dvec3& intersectionPoint)
{
    const glm::dvec3 diffp = p2 - p1;

    const double a = diffp.x * diffp.x + diffp.y * diffp.y + diffp.z * diffp.z;
    const double b = 2.0 * (diffp.x * (p1.x - center.x) + diffp.y * (p1.y - center.y) +
        diffp.z * (p1.z - center.z));
    double c = center.x * center.x + center.y * center.y + center.z * center.z;
    c += p1.x * p1.x + p1.y * p1.y + p1.z * p1.z;
    c -= 2.0 * (center.x * p1.x + center.y * p1.y + center.z * p1.z);
    c -= r * r;

    const double intersectionTest = b * b - 4.0 * a * c;

    // No intersection
    if (std::abs(a) < 0 || intersectionTest < 0.0) {
        return false;
    }
    // Intersection
    else {
        // Only care about the first intersection point if we have two
        const double t = (-b - std::sqrt(intersectionTest)) / (2.0 * a);

        // Check if utside of line segment between p1 and p2
        if (t <= 0 || t >= 1.0) {
            return false;
        }

        intersectionPoint = p1 + t * diffp;
        return true;
    }
}

bool isPointInsideSphere(const glm::dvec3& p, const glm::dvec3& c, double r) {
    const glm::dvec3 v = c - p;
    const double squaredDistance = v.x * v.x + v.y * v.y + v.z * v.z;
    const double squaredRadius = r * r;

    return (squaredDistance <= squaredRadius);
}

}  // namespace openspace::collision
