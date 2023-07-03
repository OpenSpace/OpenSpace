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

in vec4 vs_gPosition;
in vec3 vs_gNormal;
in float vs_screenSpaceDepth;
in vec2 vs_st;


uniform sampler2D texture1;
uniform float opacity = 1.0;
uniform bool mirrorBackside = true;

// Uniforms needed to color by quantity
uniform sampler1D colorTable;
uniform vec2 colorTableRange;


Fragment getFragment() {
  Fragment frag;

  float value = texture(texture1, vs_st).r;
  float lookUpVal = (value - colorTableRange.x) / (colorTableRange.y - colorTableRange.x);

  vec4 color = texture(colorTable, lookUpVal);

  vec4 vs_color = vec4(color.xyz, 1 * color.a);

    vec4 fragColor = vs_color;
  frag.color = fragColor;

  if (gl_FrontFacing) {
    frag.color =  fragColor;
  }
  else {
    if (mirrorBackside) {
      frag.color = texture(texture1, vec2(1.0 - vs_st.s, vs_st.t));
    }
    else {
      frag.color =  fragColor;
    }
  }


  frag.color.a *= opacity;
  if (frag.color.a == 0.0) {
    discard;
  }

  frag.depth = vs_screenSpaceDepth;

 frag.blend = BLEND_MODE_ADDITIVE;

  // G-Buffer
  frag.gPosition = vs_gPosition;
  frag.gNormal = vec4(vs_gNormal, 1.0);

  return frag;
}
