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

const float KERNEL_RADIUS = 3;

uniform float u_sharpness;
uniform vec2  u_inv_res_dir; // either set x to 1/width or y to 1/height
uniform sampler2D u_tex_ao;
uniform sampler2D u_tex_linear_depth;

in vec2 tc;
out vec4 out_frag;

float blur_function(vec2 uv, float r, float center_c, float center_d, inout float w_total)
{
  float c = texture(u_tex_ao, uv).x;
  float d = texture(u_tex_linear_depth, uv).x;

  const float sigma = KERNEL_RADIUS * 0.5;
  const float falloff = 1.0 / (2.0 * sigma * sigma);

  float ddiff = (d - center_d) * u_sharpness;
  float w = exp2(-r * r * falloff - ddiff * ddiff);
  w_total += w;

  return c * w;
}

void main() {
  float center_c = texture(u_tex_ao, tc).x;
  float center_d = texture(u_tex_linear_depth, tc).x;

  float c_total = center_c;
  float w_total = 1.0;

  for (float r = 1; r <= KERNEL_RADIUS; ++r) {
    vec2 uv = tc + u_inv_res_dir * r;
    c_total += blur_function(uv, r, center_c, center_d, w_total);
  }

  for (float r = 1; r <= KERNEL_RADIUS; ++r) {
    vec2 uv = tc - u_inv_res_dir * r;
    c_total += blur_function(uv, r, center_c, center_d, w_total);
  }

  out_frag = vec4(vec3(c_total/w_total), 1);
}
