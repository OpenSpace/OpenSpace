/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#version __CONTEXT__

#include "PowerScaling/powerScaling_vs.hglsl"

in vec4 in_position;
in vec4 in_position_1;
in float in_colorParameter;
in float in_colorParameter_1;
in float in_scalingParameter;
in float in_scalingParameter_1;

uniform float interpolationValue;

flat out float colorParameter;
flat out float scalingParameter;

float interpolateDataValue(float v0, float v1, float t) {
  const float Epsilon = 1E-7;
  const float NaN = log(-1.0); // undefined
  // To make sure we render values at knots with neighboring missing values,
  // check 0 and 1 expicitly
  if (abs(t) < Epsilon) {
      return v0;
  }
  if (abs(1.0 - t) < Epsilon) {
      return v1;
  }
  bool isMissing = isnan(v0) || isnan(v1);
  return isMissing ? NaN : mix(v0, v1, t);
};

void main() {
  float t = interpolationValue;

  colorParameter = interpolateDataValue(in_colorParameter, in_colorParameter_1, t);
  scalingParameter = interpolateDataValue(in_scalingParameter, in_scalingParameter_1, t);

  // @TODO: Handle spline

  gl_Position = mix(in_position, in_position_1, t);
}
