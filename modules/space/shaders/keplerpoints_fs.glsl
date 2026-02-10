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
  vec4 viewSpace;
  vec2 texCoord;
  float projectionViewDepth;
} in_data;

uniform bool enableOutline;
uniform vec3 outlineColor;
uniform float outlineWeight;
uniform vec3 color;
uniform float opacity;


Fragment getFragment() {
  // Only draw circle instead of entire quad
  vec2 st = (in_data.texCoord - vec2(0.5)) * 2.0;
  if (length(st) > 1.0) {
    discard;
  }

  // Creates outline for circle
  vec3 c = color;
  if (enableOutline && (length(st) > (1.0 - outlineWeight) && length(st) < 1.0)) {
    c = outlineColor;
  }

  Fragment frag;
  frag.color = vec4(c, opacity);
  frag.depth = in_data.projectionViewDepth;
  frag.gPosition = in_data.viewSpace;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  return frag;
}
