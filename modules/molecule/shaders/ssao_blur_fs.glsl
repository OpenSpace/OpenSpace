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

#pragma optionNV(unroll all)

const float KernelRadius = 3;

in vec2 tc;
out vec4 fragColor;

uniform sampler2D texLinearDepth;
uniform sampler2D texAo;
uniform float sharpness;
uniform vec2 invResDir; // either set x to 1/width or y to 1/height


float blurFunction(vec2 uv, float r, float centerC, float centerD, inout float wTotal) {
  const float Sigma = KernelRadius * 0.5;
  const float Falloff = 1.0 / (2.0 * Sigma * Sigma);

  float c = texture(texAo, uv).x;
  float d = texture(texLinearDepth, uv).x;

  float ddiff = (d - centerD) * sharpness;
  float w = exp2(-r * r * Falloff - ddiff * ddiff);
  wTotal += w;

  return c * w;
}

void main() {
  float centerC = texture(texAo, tc).x;
  float centerD = texture(texLinearDepth, tc).x;

  float cTotal = centerC;
  float wTotal = 1.0;

  for (float r = 1; r <= KernelRadius; r++) {
    vec2 uv = tc + invResDir * r;
    cTotal += blurFunction(uv, r, centerC, centerD, wTotal);
  }

  for (float r = 1; r <= KernelRadius; r++) {
    vec2 uv = tc - invResDir * r;
    cTotal += blurFunction(uv, r, centerC, centerD, wTotal);
  }

  fragColor = vec4(vec3(cTotal / wTotal), 1.0);
}
