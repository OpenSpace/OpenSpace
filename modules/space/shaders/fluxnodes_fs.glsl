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

#include "fragment.glsl"

in vec2 vs_st;
in vec4 vs_color;
in float vs_depth;
in float vs_closeToEarth;

uniform bool drawCircles;
uniform bool drawHollow;
uniform bool useGaussian;
uniform bool usingCameraPerspective;
uniform bool usingGaussianPulse;
uniform vec3 cameraPos;


Fragment getFragment() {
  vec4 fragColor = vs_color;
  if (vs_color.a == 0) {
    discard;
  }

  vec2 pos = vec2(0.5) - vs_st;

  float r = length(pos) * 2.0;
  float a = atan(pos.y, pos.x);
  float f = cos(a * 3.0);

  vec3 color = vec3(0.0);
  color = vec3(1.0 - smoothstep(f, f, r));

  Fragment frag;
  frag.depth = vs_depth;
  frag.color = fragColor;
  vec2 coord = gl_PointCoord - vec2(0.5);

  if (drawCircles && length(coord) > 0.5) {
    discard;
  }

  if (drawHollow &&
      length(coord) < 0.4 &&
      (vs_closeToEarth > 0.5 || distance(cameraPos, vec3(0.0)) < 500000000000.0))
  {
    if (usingGaussianPulse && usingCameraPerspective && vs_closeToEarth > 0.5) {
      if (length(coord) < 0.3) {
        const float e = 2.718055;
        float y = pow(e, - (pow(length(coord), 2.0)) / (2.0 * pow(0.2, 2.0)));
        if (y < 0.05) {
          discard;
        }
        frag.color.a = y;
      }
    }
    else {
        discard;
    }
  }

  if (useGaussian) {
    float e = 2.718055;
    float y = pow(e, - (pow(length(coord), 2.0)) / (2.0 * pow(0.2, 2.0)));
    if (y < 0.05) {
      discard;
    }
    frag.color.a = y;
  }

  frag.gPosition  = vec4(1e27, 1e27, 1e27, 1.0);
  return frag;
}
