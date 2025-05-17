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

layout(location = 0) in vec2 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

out vec2 vs_st;
out float vs_screenSpaceDepth;
out vec4 shadowCoords;
out vec3 vs_normal;

uniform dmat4 modelViewProjectionMatrix;

// ShadowMatrix is the matrix defined by:
// textureCoordsMatrix * projectionMatrix * combinedViewMatrix * modelMatrix
// where textureCoordsMatrix is just a scale and bias computation: [-1,1] to [0,1]
uniform dmat4 shadowMatrix;


void main() {
  vs_st = in_st;
  vs_normal = mat3(modelViewProjectionMatrix) * in_normal;

  dvec4 positionClipSpace  = modelViewProjectionMatrix * dvec4(in_position, 0.0, 1.0);
  vec4 positionClipSpaceZNorm = z_normalization(vec4(positionClipSpace));

  shadowCoords = vec4(shadowMatrix * dvec4(in_position, 0.0, 1.0));
  vs_screenSpaceDepth  = positionClipSpaceZNorm.w;
  gl_Position = positionClipSpaceZNorm;
}
