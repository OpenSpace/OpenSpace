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
  vec4 positionViewSpace;
  vec3 positionModelSpace;
  float screenSpaceDepth;
} in_data;

uniform vec3 xColor;
uniform vec3 yColor;
uniform vec3 zColor;
uniform float opacity;


Fragment getFragment() {
  // We compare against a small value as the first vertex doesn't have a positional
  // information (or rather it is 0) and we don't want to miss out on the color close to
  // the origin
  vec3 colorComp = step(2e-32, in_data.positionModelSpace);

  Fragment frag;
  frag.color.rgb = colorComp.x * xColor + colorComp.y * yColor + colorComp.z * zColor;
  frag.color.a = opacity;
  frag.depth = in_data.screenSpaceDepth;
  frag.gPosition = in_data.positionViewSpace;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  return frag;
}
