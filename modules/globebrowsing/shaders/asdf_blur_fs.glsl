/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include "fragment.glsl"

uniform sampler2D tex0;
uniform vec2 resolution;
uniform vec2 direction;
uniform int kernelSize;

in vec2 uv;

vec4 blur5(sampler2D tex, vec2 uv, vec2 res, vec2 dir) {
  vec4 color = vec4(0.0);
  vec2 off1 = vec2(1.3333333333333333) * dir;
  color += texture2D(tex, uv) * 0.29411764705882354;
  color += texture2D(tex, uv + (off1 / res)) * 0.35294117647058826;
  color += texture2D(tex, uv - (off1 / res)) * 0.35294117647058826;
  return color; 
}

vec4 blur9(sampler2D tex, vec2 uv, vec2 res, vec2 dir) {
  vec4 color = vec4(0.0);
  vec2 off1 = vec2(1.3846153846) * dir;
  vec2 off2 = vec2(3.2307692308) * dir;
  color += texture2D(tex, uv) * 0.2270270270;
  color += texture2D(tex, uv + (off1 / res)) * 0.3162162162;
  color += texture2D(tex, uv - (off1 / res)) * 0.3162162162;
  color += texture2D(tex, uv + (off2 / res)) * 0.0702702703;
  color += texture2D(tex, uv - (off2 / res)) * 0.0702702703;
  return color;
}

vec4 blur13(sampler2D tex, vec2 uv, vec2 res, vec2 dir) {
  vec4 color;
  vec2 off1 = vec2(1.3846153846) * dir;
  vec2 off2 = vec2(3.2307692308) * dir;
  vec2 off3 = vec2(5.176470588235294) * dir;
  color += texture2D(tex, uv) * 0.1964825501511404;
  color += texture2D(tex, uv + (off1 / res)) * 0.2969069646728344;
  color += texture2D(tex, uv - (off1 / res)) * 0.2969069646728344;
  color += texture2D(tex, uv + (off2 / res)) * 0.09447039785044732;
  color += texture2D(tex, uv - (off2 / res)) * 0.09447039785044732;
  color += texture2D(tex, uv + (off3 / res)) * 0.010381362401148057;
  color += texture2D(tex, uv - (off3 / res)) * 0.010381362401148057;

  return color;
}

Fragment getFragment() {
  Fragment frag;
  frag.disableLDR2HDR = true;
  if (kernelSize == 0) {
    frag.color = texture(tex0, uv);
  }
  if (kernelSize == 5) {
    frag.color = blur5(tex0, uv, resolution, direction);
  }
  if (kernelSize == 9) {
    frag.color = blur9(tex0, uv, resolution, direction);
  }
  if (kernelSize == 13) {
    frag.color = blur13(tex0, uv, resolution, direction);
  }
  return frag;
}
