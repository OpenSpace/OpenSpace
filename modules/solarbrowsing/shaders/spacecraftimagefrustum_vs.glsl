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

layout(location = 0) in vec4 in_position;

out Data {
  float depth;
} out_data;

uniform mat4 modelViewProjectionTransform;
uniform mat4 modelViewProjectionTransformPlane;

uniform float scale;
uniform vec2 centerPixel;

void main() {
  // Transform in either planes or spacecraft coordinate system
  vec4 positionScreenSpace = vec4(0.0);
  if (in_position.w == 1.0) {
    vec4 position = in_position;
    position.x += centerPixel.x;
    position.y += centerPixel.y;
    position.xy *= 1.0 / scale;
    vec4 positionClipSpace = modelViewProjectionTransformPlane * vec4(position.xyz, 1.0);
    positionScreenSpace = z_normalization(positionClipSpace);
  }
  else {
    vec4 positionClipSpace = modelViewProjectionTransform * vec4(in_position.xyz, 1.0);
    positionScreenSpace = z_normalization(positionClipSpace);
  }
  out_data.depth = positionScreenSpace.w;
  gl_Position = positionScreenSpace;
}
