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

uniform sampler2D u_tex;
out vec4 out_frag;

void main() {
  vec3 cc = texelFetch(u_tex, ivec2(gl_FragCoord.xy), 0).rgb;
  vec3 cl = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2(-1, 0), 0).rgb;
  vec3 ct = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2( 0, 1), 0).rgb;
  vec3 cr = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2( 1, 0), 0).rgb;
  vec3 cb = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2( 0,-1), 0).rgb;

  const float weight[2] = float[2](1.4, -0.1);
  out_frag = vec4(vec3(weight[0] * cc + weight[1] * (cl + ct + cr + cb)), 1.0);
}
