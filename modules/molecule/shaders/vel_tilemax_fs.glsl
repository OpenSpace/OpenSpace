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

const int TileSize = #{TileSize};

in vec2 tc;
out vec4 fragColor;

uniform sampler2D u_tex_vel;
uniform vec2 u_tex_vel_texel_size;

void main() {
  vec2 base = tc + (0.5 - 0.5 * TileSize) * u_tex_vel_texel_size;

  vec2 mv = vec2(0.0);
  float mv2 = 0.0;

  for (int i = 0; i < TileSize; i++) {
    for (int j = 0; j < TileSize; j++) {
      vec2 v = texture(u_tex_vel, base + vec2(i, j) * u_tex_vel_texel_size).xy;
      float v2 = dot(v, v);
      if (v2 > mv2) {
        mv = v;
        mv2 = v2;
      }
    }
  }

  fragColor = vec4(min(mv, length(u_tex_vel_texel_size) * TileSize), 0.0, 0.0);
}
