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

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

out Data {
  vec3 normal;
  vec2 st;
  float depth;
} out_data;

uniform mat4 modelTransform;
uniform mat4 modelViewProjectionTransform;
uniform bool hasHeightMap;
uniform float heightExaggeration;
uniform sampler2D heightTexture;
uniform bool meridianShift;


void main() {
  vec3 tmp = in_position.xyz;

  if (hasHeightMap) {
    vec2 st = out_data.st;
    if (meridianShift) {
      st += vec2(0.5, 0.0);
    }
    float height = texture(heightTexture, st).s;
    vec3 displacementDirection = normalize(tmp);
    float displacementFactor = height * heightExaggeration;
    tmp += displacementDirection * displacementFactor;
  }

  // convert from psc to homogeneous coordinates
  vec4 position = vec4(tmp, 1.0);
  vec4 positionClipSpace = modelViewProjectionTransform * position;
  vec4 p = z_normalization(positionClipSpace);

  // This is wrong for the normal.
  // The normal transform is the transposed inverse of the model transform
  out_data.normal = normalize(modelTransform * vec4(in_normal, 0.0)).xyz;
  out_data.st = in_st;
  out_data.depth = p.w;
  gl_Position = p;
}
