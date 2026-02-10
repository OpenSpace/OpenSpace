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
layout(location = 1) in vec2 in_texCoord;

out Data {
  vec4 gPosition;
  vec3 gNormal;
  vec2 texCoord;
  float screenSpaceDepth;
} out_data;

uniform mat4 modelViewProjectionTransform;
uniform mat4 modelViewTransform;


void main() {
  vec4 position = vec4(in_position.xyz * pow(10.0, in_position.w), 1.0);
  vec4 positionClipSpace = modelViewProjectionTransform * position;
  vec4 positionScreenSpace = z_normalization(positionClipSpace);

  gl_Position = positionScreenSpace;

  // G-Buffer
  out_data.gNormal = vec3(0.0);
  out_data.gPosition = vec4(modelViewTransform * position); // Must be in SGCT eye space;
  out_data.texCoord = in_texCoord;
  out_data.screenSpaceDepth = positionScreenSpace.w;
}
