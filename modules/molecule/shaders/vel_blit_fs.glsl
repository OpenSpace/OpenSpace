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

in vec2 tc;
out vec4 fragColor;

uniform sampler2D u_tex_depth;
uniform mat4 u_curr_clip_to_prev_clip_mat;
uniform vec4 u_jitter_uv;

void main() {
  float d = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy), 0).x;
  if (d == 1.0) {
    discard;
  }

  vec2 pUv = tc;
  vec3 pVs = vec3(tc, d);
  vec4 pCs = vec4(pVs * 2.0 - 1.0, 1.0); // [0, 1] -> [-1, 1]

  vec4 qCs = u_curr_clip_to_prev_clip_mat * pCs;
  vec2 qUv = (qCs.xy / qCs.w) * 0.5 + 0.5; // [-1, 1] -> [0, 1]

  vec2 ssVel = (pUv - qUv) + (u_jitter_uv.xy - u_jitter_uv.zw);
  fragColor = vec4(ssVel, 0.0, 0.0);
}
