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

#version __CONTEXT__

#include "PowerScaling/powerScaling_vs.hglsl"

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_color;

out vec4 vs_position;
out vec3 vs_color;
out float vs_screenSpaceDepth;
out float vs_starBrightness;

uniform dmat4 viewProjectionMatrix;
uniform dmat4 modelMatrix;
uniform dvec3 eyePosition;

const double PARSEC = 3.08567756E16;


void main() {
  vs_position = vec4(in_position, 1.0);
  dvec4 dpos = dvec4(vs_position);

  double distanceToStar = length(dpos.xyz - eyePosition);
  vs_starBrightness = clamp(float(8000.0 * PARSEC / distanceToStar), 0.0, 1.0);

  dpos.xyz *= 8.0;
  dpos = modelMatrix * dpos;
  dpos /= PARSEC;

  vec4 positionScreenSpace = z_normalization(vec4(viewProjectionMatrix * dpos));
  vs_color = in_color;
  vs_screenSpaceDepth = positionScreenSpace.w;
  gl_Position = positionScreenSpace;
}
