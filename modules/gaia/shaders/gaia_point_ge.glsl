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

#include "floatoperations.glsl"

layout(points) in;
in vec2 vs_brightness[];
in vec4 vs_gPosition[];
in float vs_starDistFromSun[];
in float vs_cameraDistFromSun[];

layout(points, max_vertices = 1) out;
out vec2 ge_brightness;
out vec4 ge_gPosition;
out float ge_starDistFromSun;
out float ge_cameraDistFromSun;
out float ge_observedDist;

uniform dmat4 view;
uniform float viewScaling;
uniform float cutOffThreshold;

const float EPS = 1e-5;


void main() {
  ge_brightness = vs_brightness[0];
  ge_starDistFromSun = vs_starDistFromSun[0];
  ge_cameraDistFromSun = vs_cameraDistFromSun[0];

  vec4 viewPosition = vec4(view * vs_gPosition[0]);

  ge_observedDist = safeLength(viewPosition / viewScaling);
  float distThreshold = cutOffThreshold - log(ge_observedDist) / log(4.0);

  vec4 position = gl_in[0].gl_Position;

  // Discard geometry if star has no position (but wasn't a nullArray).
  // Or if observed distance is above threshold set by cutOffThreshold.
  // By discarding in gs instead of fs we save computations for when nothing is visible.
  if (length(position) < EPS || distThreshold <= 0) {
    return;
  }

  //gl_PointSize = 1.0;
  gl_Position = position;
  gl_Position.z = 0.0;
  ge_gPosition = viewPosition;

  EmitVertex();

  EndPrimitive();
}
