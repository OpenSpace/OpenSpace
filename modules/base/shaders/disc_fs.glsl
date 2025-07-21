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
in float vs_screenSpaceDepth;

uniform sampler1D colorTexture;
uniform float width;
uniform float opacity;


Fragment getFragment() {
  // The length of the texture coordinates vector is our distance from the center
  float radius = length(vs_st);

  // We only want to consider ring-like objects so we need to discard everything else
  if (radius > 1.0) {
    discard;
  }

  // Remapping the texture coordinates
  // Radius \in [0,1],
  float inner = 1.0 - width;
  float texCoord = (radius - inner) / (1.0 - inner);
  if (texCoord < 0.0 || texCoord > 1.0) {
    discard;
  }

  vec4 diffuse = texture(colorTexture, texCoord);
  diffuse.a *= opacity;

  Fragment frag;
  frag.color = diffuse;
  frag.depth = vs_screenSpaceDepth;
  return frag;
}
