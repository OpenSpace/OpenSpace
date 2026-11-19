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

const float M_PI = 3.14159265358979323846;
const float SQRT1_3 = 0.57735026919;
const float OpacityThreshold = 0.01;


vec3 cartesianToSpherical(vec3 _cartesian) {
  // Put cartesian in [-1..1] range first
  vec3 cartesian = vec3(-1.0) + _cartesian * 2.0;

  float r = length(cartesian);
  float theta = 0.0;
  float phi = 0.0;

  if (r != 0.0) {
    // Rounding errors can move the argument of `acos` outside of [-1, 1] and `atan` is
    // undefined if both arguments are 0, so these cases are handled explicitly
    bool isOnAxis = cartesian.x == 0.0 && cartesian.y == 0.0;
    float azimuth = isOnAxis ? 0.0 : atan(cartesian.y, cartesian.x);
    theta = acos(clamp(cartesian.z / r, -1.0, 1.0)) / M_PI;
    phi = (M_PI + azimuth) / (2.0 * M_PI);
  }
  r *= SQRT1_3;
  return vec3(r, theta, phi);
}

int intCoord(ivec3 coords, ivec3 dim) {
  return coords.x + dim.x * coords.y + dim.x * dim.y * coords.z;
}

vec3 vec3Coords(uint coords, ivec3 dim) {
  return vec3(mod(coords, dim.x), mod(coords / dim.x, dim.y), coords / dim.x / dim.y);
}
