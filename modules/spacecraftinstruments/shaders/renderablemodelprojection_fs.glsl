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

in Data {
  vec4 ndc;
  vec4 normal;
} in_data;

layout (location = 0) out vec4 out_color;
layout (location = 1) out vec4 out_stencil;

uniform sampler2D projectionTexture;
uniform sampler2D depthTexture;
uniform bool needShadowMap;
uniform vec3 boresight;


bool inRange(float x, float a, float b) {
  return (x >= a && x <= b);
}


void main() {
  vec3 n = normalize(in_data.normal.xyz);
  vec2 uv = vec2(0.5) * in_data.ndc.xy + vec2(0.5);

  if (needShadowMap) {
    float thisDepth = in_data.ndc.z * 0.5 + 0.5;
    float closestDepth = texture(depthTexture, uv).r;
    const float Epsilon = 0.001;

    if (inRange(uv.x, 0.0, 1.0) && inRange(uv.y, 0.0, 1.0) &&
        dot(n, boresight) < 0 && thisDepth <= closestDepth + Epsilon)
    {
      out_color = texture(projectionTexture, vec2(1.0) - uv);
      out_color.a = 1.0;
      out_stencil = vec4(1.0);
    }
    else {
      discard;
    }
  }
  else {
    if (inRange(uv.x, 0.0, 1.0) && inRange(uv.y, 0.0, 1.0) && dot(n, boresight) < 0) {
      out_color = texture(projectionTexture, vec2(1.0) - uv);
      out_color.a = 1.0;
      out_stencil = vec4(1.0);
    }
    else {
      discard;
    }
  }
}
