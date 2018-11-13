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

#define VOLUME_PI      3.14159265358979323846  /* pi */
#define VOLUME_SQRT1_3 0.57735026919           /* 1/sqrt(3) */

vec3 volume_cartesianToSpherical(vec3 zeroToOneCoords) {
    // Put cartesian in [-1..1] range first
    vec3 cartesian = vec3(-1.0,-1.0,-1.0) + zeroToOneCoords * 2.0f;

    float r = length(cartesian);

    float theta = 0.0;
    float phi = 0.0;

    if (r != 0.0) {
        theta = acos(cartesian.z / r) / VOLUME_PI;
        phi = (VOLUME_PI + atan(cartesian.y, cartesian.x)) / (2.0 * VOLUME_PI );
    }

    return vec3(r, theta, phi);
}
