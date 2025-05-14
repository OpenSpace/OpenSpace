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

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

out vec4 vs_ndc;
out vec4 vs_normal;

uniform mat4 ProjectorMatrix;
uniform mat4 ModelTransform;
uniform mat4 meshTransform;
uniform mat4 meshNormalTransform;


void main() {
  vec4 raw_pos = psc_to_meter(meshTransform * in_position, vec2(1.0, 0.0));
  vec4 position = ProjectorMatrix * ModelTransform * raw_pos;
  vs_normal = normalize(ModelTransform * meshNormalTransform * vec4(in_normal, 0.0));
  vs_ndc = position / position.w;

  vec2 texco = (in_st * 2.0) - 1.0;
  gl_Position = vec4(texco, 0.0, 1.0);
}
