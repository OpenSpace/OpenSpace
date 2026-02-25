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

#include "powerScaling/powerScaling_vs.glsl"

// The shader expands a single point vertex into an arrow originally pointing along +X
// direction, it is then rotated to match the direction of the incomming vector.
layout (points) in;
in Data {
  vec3 vs_position;
  vec3 vs_direction;
  float vs_magnitude;
} in_data[];

layout (line_strip, max_vertices = 6) out;
out Data {
  flat vec3 direction;
  flat float magnitude;
  float positionDepth;
} out_data;

uniform mat4 modelViewProjection;
uniform float arrowScale;

mat3 makeRotation(vec3 dir) {
  vec3 x = normalize(dir);
  // Pick an up vector that is not parallel to x. Avoids numerical instability issues
  vec3 up = abs(x.z) < 0.999 ? vec3(0.0, 0.0, 1.0) : vec3(0.0, 1.0, 0.0);
  vec3 y = normalize(cross(up, x));
  vec3 z = cross(x, y);
  return mat3(x, y, z);
}

float exponentialScale(float sliderValue, float minExp, float maxExp) {
  // Normalize slider to 0-1
  float t = (sliderValue - 1.0) / (100.0 - 1.0);

  // Interpolate exponent
  float exponent = minExp + t * (maxExp - minExp);

  // Base-10 exponential
  return pow(10.0, exponent);
}

void emitWorldVertex(vec3 worldPos) {
  vec4 positionClipSpace = modelViewProjection * vec4(worldPos, 1.0);
  out_data.positionDepth = positionClipSpace.w;
  gl_Position = z_normalization(positionClipSpace);
  EmitVertex();
}

void main() {
  vec3 origin = in_data[0].position;
  vec3 direction = in_data[0].direction;
  float magnitude = in_data[0].magnitude;

  out_data.direction = direction;
  out_data.magnitude = magnitude;

  // The min and max exponents are arbitrarily chosen, going from 1m resolution to ~3Mpc
  float scale = exponentialScale(arrowScale, 1, 23.0);
  mat3 rotationMatrix = makeRotation(direction);
  float arrowLength = magnitude * scale;

  // The arrow is defined pointing along the +X direction
  //       x2
  //         \
  // x0 ---- x1
  //         /
  //       x3
  vec3 tail = origin; // x0
  vec3 tip = origin + rotationMatrix * vec3(1.0, 0.0, 0.0) * arrowLength; // x1
  vec3 topWing = origin + rotationMatrix * vec3(0.8, 0.1, 0.0) * arrowLength; // x2
  vec3 bottomWing = origin + rotationMatrix * vec3(0.8, -0.1, 0.0) * arrowLength; // x3

  emitWorldVertex(tail);
  emitWorldVertex(tip);
  EndPrimitive();

  emitWorldVertex(tip);
  emitWorldVertex(topWing);
  EndPrimitive();

  emitWorldVertex(tip);
  emitWorldVertex(bottomWing);
  EndPrimitive();
}
