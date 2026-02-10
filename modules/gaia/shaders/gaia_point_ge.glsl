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

#include "floatoperations.glsl"

layout(points) in;
in Data {
  vec4 gPosition;
  vec2 brightness;
  float starDistFromSun;
  float cameraDistFromSun;
} in_data[];

layout(points, max_vertices = 1) out;
out Data {
  vec4 gPosition;
  vec2 brightness;
  float starDistFromSun;
  float cameraDistFromSun;
  float observedDist;
} out_data;

uniform dmat4 view;
uniform float viewScaling;
uniform float cutOffThreshold;

const float Eps = 1e-5;


void main() {
  out_data.brightness = in_data[0].brightness;
  out_data.starDistFromSun = in_data[0].starDistFromSun;
  out_data.cameraDistFromSun = in_data[0].cameraDistFromSun;

  vec4 viewPosition = vec4(view * in_data[0].gPosition);

  out_data.observedDist = safeLength(viewPosition / viewScaling);
  float distThreshold = cutOffThreshold - log(out_data.observedDist) / log(4.0);

  vec4 position = gl_in[0].gl_Position;

  // Discard geometry if star has no position (but wasn't a nullArray).
  // Or if observed distance is above threshold set by cutOffThreshold.
  // By discarding in gs instead of fs we save computations for when nothing is visible.
  if (length(position) < Eps || distThreshold <= 0.0) {
    return;
  }

  //gl_PointSize = 1.0;
  gl_Position = position;
  gl_Position.z = 0.0;
  out_data.gPosition = viewPosition;

  EmitVertex();
  EndPrimitive();
}
