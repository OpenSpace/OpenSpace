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

flat in vec3 gs_direction;
flat in float gs_magnitude;
in float gs_positionDepth;

out vec4 outColor;

uniform float opacity;
uniform vec2 dataRangeFilter;
uniform bool filterOutOfRange;
uniform bool colorByMag;
uniform vec2 magDomain;
uniform sampler1D colorTexture;

Fragment getFragment() {
  Fragment frag;

  if (opacity == 0.0) {
      discard;
  }

  bool magnitudeOutOfRange =
    gs_magnitude < dataRangeFilter.x || gs_magnitude > dataRangeFilter.y;
  if (filterOutOfRange && magnitudeOutOfRange) {
    discard;
  }

  if (colorByMag) {
    float t = (gs_magnitude - magDomain.x) / (magDomain.y - magDomain.x);
    t = clamp(t, 0.0, 1.0);
    frag.color = texture(colorTexture, t);
  }
  else {
    vec3 dir = normalize(gs_direction);
    vec3 color = 0.5 * (dir + vec3(1.0)); // remaps [-1, 1] -> [0, 1]
    frag.color = vec4(color, 1.0);
  }
  frag.color.a *= opacity;
  frag.depth = gs_positionDepth;
  return frag;
}
