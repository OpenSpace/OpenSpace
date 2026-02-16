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

#version __CONTEXT__

#include "powerscaling/powerscaling_vs.glsl"

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_color;

out Data {
  vec4 position;
  vec3 color;
  float screenSpaceDepth;
  float starBrightness;
} out_data;

uniform dmat4 viewProjectionMatrix;
uniform dmat4 modelMatrix;
uniform dvec3 eyePosition;

const double Parsec = 3.08567756E16;


void main() {
  out_data.position = vec4(in_position, 1.0);

  double distanceToStar = length(dvec3(out_data.position.xyz) - eyePosition);
  out_data.starBrightness = clamp(float(8000.0 * Parsec / distanceToStar), 0.0, 1.0);

  dvec4 dpos = dvec4(out_data.position) * dvec4(8.0, 8.0, 8.0, 1.0);
  dpos.xyz *= 8.0;
  dpos = modelMatrix * dpos;
  dpos /= Parsec;

  gl_Position = z_normalization(vec4(viewProjectionMatrix * dpos));
  out_data.color = in_color;
  out_data.screenSpaceDepth = gl_Position.w;
}
