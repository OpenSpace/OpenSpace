/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;
layout(location = 3) in vec3 in_tangent;
layout(location = 4) in vec3 in_color;

out vec2 vs_st;
out vec3 vs_normalViewSpace;
out float vs_screenSpaceDepth;
out vec4 vs_positionCameraSpace;
out mat3 vs_TBN;
out vec3 vs_color;

uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform mat4 normalTransform;
uniform mat4 meshTransform;
uniform mat4 meshNormalTransform;


void main() {
  vs_positionCameraSpace = modelViewTransform * (meshTransform * in_position);
  vec4 positionClipSpace = projectionTransform * vs_positionCameraSpace;
  vec4 positionScreenSpace = z_normalization(positionClipSpace);

  gl_Position = positionScreenSpace;
  vs_st = in_st;
  vs_color = in_color;
  vs_screenSpaceDepth = positionScreenSpace.w;

  vs_normalViewSpace =
    normalize(mat3(normalTransform) * (mat3(meshNormalTransform) * in_normal));

  // TBN matrix for normal mapping
  vec3 T = normalize(mat3(normalTransform) * (mat3(meshNormalTransform) * in_tangent));
  vec3 N = normalize(mat3(normalTransform) * (mat3(meshNormalTransform) * in_normal));

  // Re-orthogonalize T with respect to N
  T = normalize(T - dot(T, N) * N);

  // Retrieve perpendicular vector B with cross product of T and N
  vec3 B = normalize(cross(N, T));

  vs_TBN = mat3(T, B, N);
}
