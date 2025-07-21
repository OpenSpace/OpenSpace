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

#define MULTIRES_PI      3.14159265358979323846  /* pi */
#define MULTIRES_SQRT1_3 0.57735026919           /* 1/sqrt(3) */
#define MULTIRES_OPACITY_THRESHOLD 0.01

vec3 multires_cartesianToSpherical(vec3 _cartesian) {
    // Put cartesian in [-1..1] range first
    vec3 cartesian = vec3(-1.0) + _cartesian * 2.0f;

    float r = length(cartesian);
    float theta = 0.0;
    float phi = 0.0;

    if (r != 0.0) {
        theta = acos(cartesian.z / r) / MULTIRES_PI;
        phi = (MULTIRES_PI + atan(cartesian.y, cartesian.x)) / (2.0 * MULTIRES_PI);
    }
    r *= MULTIRES_SQRT1_3;
    return vec3(r, theta, phi);
}

int multires_intCoord(ivec3 vec3Coords, ivec3 spaceDim) {
    return vec3Coords.x +
           spaceDim.x * vec3Coords.y +
           spaceDim.x * spaceDim.y * vec3Coords.z;
}

vec3 multires_vec3Coords(uint intCoord, ivec3 spaceDim) {
    return vec3(
        mod(intCoord, spaceDim.x),
        mod(intCoord / spaceDim.x, spaceDim.y),
        intCoord / spaceDim.x / spaceDim.y
    );
}
