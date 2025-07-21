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

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_textureCoords;

out vec2 vs_textureCoords;
out vec4 vs_position;
out vec3 vs_normal;
out float vs_screenSpaceDepth;

uniform mat4 modelViewProjection;
uniform mat4 modelViewTransform;
uniform mat3 modelViewRotation;


void main() {
  vs_normal = modelViewRotation * normalize(in_position.xyz);
  vs_textureCoords = in_textureCoords;

  vec4 position = modelViewProjection * vec4(in_position.xyz, 1.0);
  vs_position = modelViewTransform * vec4(in_position.xyz, 1.0);

  // Set z to 0 to disable near/far-plane clipping
  gl_Position = vec4(position.xy, 0.0, position.w);

  vs_screenSpaceDepth = position.w;
}
