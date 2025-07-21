/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

in vec2 vs_st;
in vec4 vs_position;

uniform sampler2D textures[6];
uniform sampler2D transferFunctions[6];

uniform int numTextures;
uniform int numTransferFunctions;
uniform bool averageValues;
uniform vec2 backgroundValues;
uniform float transparency;

const vec4 Transparent = vec4(0.0);


Fragment getFragment() {
  vec4 position = vs_position;
  float depth = pscDepth(position);
  vec4 diffuse = Transparent;

  float x = backgroundValues.x;
  float y = backgroundValues.y;

  if ((numTransferFunctions == 1) || (numTextures > numTransferFunctions)) {
    float v = 0;
    for (int i = 0; i < numTextures; i++) {
      v += texture(textures[i], vec2(vs_st.t, vs_st.s)).r;
    }
    v /= numTextures;

    vec4 color = texture(transferFunctions[0], vec2(v, 0.0));
    if ((v < (x + y)) && v > (x - y)) {
      color = mix(Transparent, color, clamp(1.0, 0.0, abs(v - x)));
    }

    diffuse = color;
  }
  else {
    for (int i = 0; i < numTextures; i++) {
      float v = texture(textures[i], vec2(vs_st.t, vs_st.s)).r;
      vec4 color = texture(transferFunctions[i], vec2(v, 0.0));
      if ((v < (x + y)) && v > (x - y)) {
          color = mix(Transparent, color, clamp(1.0, 0.0, abs(v - x)));
      }
      diffuse += color;
    }
  }

  if (diffuse.a <= backgroundValues.y) {
    discard;
  }

  Fragment frag;
  frag.color = diffuse * vec4(1.0, 1.0, 1.0, transparency);
  frag.depth = depth;
  return frag;
}
