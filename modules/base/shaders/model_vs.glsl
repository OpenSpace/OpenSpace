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
layout(location = 3) in vec3 in_tangent;
layout(location = 4) in vec3 in_color;

out Data {
  mat3 tbn;
  vec4 positionCameraSpace;
  vec3 normalViewSpace;
  vec3 color;
  vec2 st;
  float screenSpaceDepth;
} out_data;

uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform mat4 normalTransform;
uniform mat4 meshTransform;
uniform mat4 meshNormalTransform;

out vec4 lightspace_position;
uniform dmat4 model;
uniform dmat4 light_vp;


void main() {
  out_data.positionCameraSpace = modelViewTransform * (meshTransform * in_position);
  vec4 positionClipSpace = projectionTransform * out_data.positionCameraSpace;
  vec4 positionScreenSpace = z_normalization(positionClipSpace);

  gl_Position = positionScreenSpace;
  out_data.st = in_st;
  out_data.color = in_color;
  out_data.screenSpaceDepth = positionScreenSpace.w;

  out_data.normalViewSpace =
    normalize(mat3(normalTransform) * (mat3(meshNormalTransform) * in_normal));

  // TBN matrix for normal mapping
  vec3 T = normalize(mat3(normalTransform) * (mat3(meshNormalTransform) * in_tangent));
  vec3 N = normalize(mat3(normalTransform) * (mat3(meshNormalTransform) * in_normal));

  // Re-orthogonalize T with respect to N
  T = normalize(T - dot(T, N) * N);

  // Retrieve perpendicular vector B with cross product of T and N
  vec3 B = normalize(cross(N, T));

  out_data.tbn = mat3(T, B, N);

  lightspace_position = vec4(light_vp * model * meshTransform * in_position);
}
