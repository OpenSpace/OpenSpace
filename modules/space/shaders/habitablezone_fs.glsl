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

uniform sampler1D transferFunctionTexture;
uniform float width;
uniform float opacity;
uniform vec2 conservativeBounds;
uniform bool showOptimistic;


// Remap the radius to texture coordinates in the trasfer function texture. The texture
// is treated as a linear scale where the color represent too cold to too hot. Account
// for the conservative bounds my mapping one third of the texture ouside each boundary.
// All parameters \in [0,1], where 1.0 corresponds to the max radius.
float computeTextureCoord(float radius, float innerRadius,
                          float conservativeInner, float conservativeOuter)
{
  const float t1 = 1.0 / 3.0;
  const float t2 = 2.0 / 3.0;

  if (radius < conservativeInner) {
    float t = (radius - innerRadius) / (conservativeInner - innerRadius);
    return mix(0.0, t1, t);
  }
  else if (radius > conservativeOuter) {
    float t = (radius - conservativeOuter) / (1.0 - conservativeOuter);
    return mix(t2, 1.0, t);
  }
  else {
    float t = (radius - conservativeInner) / (conservativeOuter - conservativeInner);
    return mix(t1, t2, t);
  }
}


Fragment getFragment() {
  // The length of the texture coordinates vector is our distance from the center
  float radius = length(vs_st);
  float innerRadius = 1.0 - width;

  // We only want to consider ring-like objects so we need to discard everything else
  if (radius > 1.0 || radius < innerRadius) {
    discard;
  }

  float consInner = conservativeBounds.x;
  float consOuter = conservativeBounds.y;
  bool outsideConservative = (radius < consInner) || (radius > consOuter);

  if (!showOptimistic && outsideConservative) {
    discard;
  }

  float texCoord = computeTextureCoord(radius, innerRadius, consInner, consOuter);

  vec4 diffuse = texture(transferFunctionTexture, texCoord);
  diffuse.a *= opacity;

  Fragment frag;
  frag.color = diffuse;
  frag.depth = vs_screenSpaceDepth;
  return frag;
}
