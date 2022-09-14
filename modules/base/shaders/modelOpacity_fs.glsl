/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include "floatoperations.glsl"

in vec2 vs_st;

uniform float opacity = 1.0;
uniform bool opacityBlending = false;

uniform sampler2D colorTexture;
uniform sampler2D depthTexture;
uniform sampler2D positionTexture;
uniform sampler2D normalTexture;

Fragment getFragment() {
  Fragment frag;

  if (opacity == 0.0) {
    discard;
  }

  if (opacityBlending) {
    // frag.color.a = opacity * (frag.color.r + frag.color.g + frag.color.b)/3.0;
    frag.color.a = opacity * max(max(frag.color.r, frag.color.g), frag.color.b);
  }
  else {
    frag.color.a = opacity;
  }

  frag.color.rgb = texture(colorTexture, vs_st).rgb;
  frag.depth = denormalizeFloat(texture(depthTexture, vs_st).x);
  frag.gPosition = texture(positionTexture, vs_st);
  frag.gNormal = vec4(texture(normalTexture, vs_st).rgb, 0.0);

  return frag;
}
