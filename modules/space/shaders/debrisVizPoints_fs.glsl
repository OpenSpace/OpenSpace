/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

in float projectionViewDepth;
in vec4 viewSpace;
in vec2 texCoord;
flat in int skip;

uniform bool enableOutline;
uniform vec3 outlineColor;
uniform float outlineWeight;
uniform vec3 color;
uniform float opacity;

Fragment getFragment() {
  Fragment frag;
  
  if (skip == 1) {
    discard;
  }

  // Only draw circle instead of entire quad
  vec2 st = (texCoord - vec2(0.5)) * 2.0;
  if (length(st) > 1.0) {
    discard;
  }

  // Creates outline for circle
  vec3 _color = color;
  if (enableOutline && (length(st) > (1.0 - outlineWeight) && length(st) < 1.0)) {
    _color = outlineColor;
  }

  frag.color = vec4(_color, opacity);
  frag.depth = projectionViewDepth;
  frag.gPosition = viewSpace;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);

  return frag;
}
