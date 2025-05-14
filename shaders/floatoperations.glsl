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

#ifndef _FLOATOPERATIONS_GLSL_
#define _FLOATOPERATIONS_GLSL_

/**
 * Convert a positive floating point distance [0, 10^27]
 * (size of observable universe)
 * to a float in the range [-1, 1], suitable for depth buffer storage.
 * Note: This needs to be a monotonic function, so that the value can
 * still be used for depth comparison.
 */
float normalizeFloat(float inpt) {
  if (inpt > 1.0) {
    return inpt / pow(10, 30);
  }
  else {
    return inpt - 1.0;
  }
}

float denormalizeFloat(float inpt) {
  if (inpt < 0.0) {
    return inpt + 1.0;
  }
  else {
    return inpt * pow(10, 30);
  }
}

/**
 * Compute the length of a vector.
 * Supporting huge vectors, where the square of any of the components is too large to be
 * represented as a float.
 */
float safeLength(vec4 v) {
  float m = max(max(max(abs(v.x), abs(v.y)), abs(v.z)), abs(v.w));
  if (m > 0.f) {
    return length(v / m) * m;
  }
  else {
    return 0.f;
  }
}

float safeLength(vec3 v) {
  return safeLength(vec4(v, 0.0));
}
float safeLength(vec2 v) {
  return safeLength(vec4(v, 0.0, 0.0));
}

#endif // _FLOATOPERATIONS_GLSL_
