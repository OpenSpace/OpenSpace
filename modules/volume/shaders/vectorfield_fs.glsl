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

#include "fragment.glsl"

in Data {
  flat vec3 direction;
  flat float magnitude;
  float positionDepth;
} in_data;


uniform float opacity;
uniform vec2 dataRangeFilter;
uniform int colorMode;
uniform vec2 magDomain;
uniform sampler1D colorTexture;
uniform vec4 fixedColor;

const int ColorModeFixed = 0;
const int ColorModeMagnitude = 1;
const int ColorModeDirection = 2;

Fragment getFragment() {
  Fragment frag;

  if (opacity == 0.0) {
      discard;
  }

  // vec4 fixedColor = vec4(1.0, 0.0, 0.0, 1.0);
  if (colorMode == ColorModeFixed) {
    frag.color = fixedColor;
  }
  else if (colorMode == ColorModeMagnitude) {
    float t = (in_data.magnitude - magDomain.x) / (magDomain.y - magDomain.x);
    t = clamp(t, 0.0, 1.0);
    frag.color = texture(colorTexture, t);
  }
  else { // colorMode == ColorModeDirection
    vec3 dir = normalize(in_data.direction);
    vec3 color = 0.5 * (dir + vec3(1.0)); // remaps [-1, 1] -> [0, 1]
    frag.color = vec4(color, 1.0);
  }

  frag.color.a *= opacity;
  frag.depth = gs_positionDepth;
  return frag;
}
