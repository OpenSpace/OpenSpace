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
  vec2 texCoords;
  float depth;
} in_data;

uniform sampler2D imageryTexture;
uniform sampler1D lut;

uniform float contrastValue;
uniform float gammaValue;
uniform float planeOpacity;
uniform bool hasLut;
uniform bool isCoronaGraph;
uniform int faceMode;

const int FrontOnly = 0;
const int SolidBack = 1;
const int DoubleSided = 2;


float contrast(float intensity) {
  return min(
    clamp(0.5 + (intensity - 0.5) * (1.0 + contrastValue / 10.0), 0.0, 1.0),
    sqrt(intensity) + intensity
  );
}

Fragment getFragment() {
  float intensityOrg = texture(
    imageryTexture,
    vec2(in_data.texCoords.s, 1.0 - in_data.texCoords.t)
  ).r;
  intensityOrg = contrast(intensityOrg);

  vec4 outColor;
  if (hasLut) {
    outColor = texture(lut, intensityOrg);
  }
  else {
    outColor = vec4(intensityOrg, intensityOrg, intensityOrg, 1.0);
  }

  if (!gl_FrontFacing) {
    if (faceMode == SolidBack) {
      outColor = vec4(vec3(0.2), planeOpacity);
    }
    // Doublesided, do nothing as we want the same image on the back side
  }

  outColor.r = pow(outColor.r, gammaValue);
  outColor.g = pow(outColor.g, gammaValue);
  outColor.b = pow(outColor.b, gammaValue);

  if (planeOpacity == 0.0) {
    discard;
  }

  vec2 center = abs(vec2(0.5 - in_data.texCoords);

  if (isCoronaGraph && length(outColor.xyz) < 0.10 &&
     ((center.y * center.y + center.x * center.x) > 0.25))
  {
    discard;
  }

  Fragment frag;
  frag.color = vec4(outColor.rgb, planeOpacity);
  frag.depth = in_data.depth;
  frag.blend = BlendModeAdditive;
  return frag;
}
