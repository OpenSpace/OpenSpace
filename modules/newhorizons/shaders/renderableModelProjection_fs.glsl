/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

in vec4 vs_position;
in vec4 vs_ndc;
in vec4 vs_normal;

layout (location = 0) out vec4 color;
// Even though the stencel texture is only a single channel, we still need to
// output a vec4, or the result will disappear
layout (location = 1) out vec4 stencil;

uniform sampler2D projectionTexture;
uniform sampler2D depthTexture;

uniform mat4 ModelTransform;
uniform vec3 boresight;
uniform vec4 debugColor;

bool inRange(float x, float a, float b) {
    return (x >= a && x <= b);
} 

void main() {
  vec3 n = normalize(vs_normal.xyz);
  vec4 projected = vs_ndc;
  vec2 uv = vec2(0.5) * projected.xy + vec2(0.5);

  float thisDepth = projected.z * 0.5 + 0.5;
  float closestDepth = texture(depthTexture, uv).r;
  float epsilon = 0.001;

  if (inRange(uv.x, 0.0, 1.0) && inRange(uv.y, 0.0, 1.0) &&
      dot(n, boresight) < 0 && thisDepth <= closestDepth + epsilon) {
        color = texture(projectionTexture, vec2(1.0) - uv);
        color.a = 1.0;
        stencil = vec4(1.0);
  } else {
    color = vec4(vec3(0.0), 0.0);
    stencil = vec4(0.0);
  }
}
