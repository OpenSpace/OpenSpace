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

const int MaxSpacecraftObservatories = 7;

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;

out Data {
  vec4 positionScreenSpace;
  vec3 vUv[MaxSpacecraftObservatories];
  vec3 positionModelSpace;
} out_data;

uniform mat4 modelViewProjectionTransform;
uniform bool isCoronaGraph[MaxSpacecraftObservatories];
uniform bool isEnabled[MaxSpacecraftObservatories];
uniform dmat4 sunToSpacecraftReferenceFrame[MaxSpacecraftObservatories];
uniform int numSpacecraftCameraPlanes;

void main() {
  vec4 position = vec4(in_position.xyz, 1.0);
  out_data.positionModelSpace = position.xyz;

  // Transform the positions to the reference frame of the spacecraft to get tex coords
  for (int i = 0; i < numSpacecraftCameraPlanes; i++) {
    out_data.vUv[i] = vec3(0.0);
    if (isCoronaGraph[i] || !isEnabled[i])  {
      continue;
    };
    out_data.vUv[i] = vec3(sunToSpacecraftReferenceFrame[i] * dvec4(position)).xyz;
  }

  vec4 positionClipSpace = modelViewProjectionTransform * position;
  out_data.positionScreenSpace = z_normalization(positionClipSpace);
  gl_Position = out_data.positionScreenSpace;
}
