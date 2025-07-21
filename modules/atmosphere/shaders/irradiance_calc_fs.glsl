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

#version __CONTEXT__

#include "atmosphere_common.glsl"

out vec4 renderTableColor;

uniform float Rg;
uniform float Rt;
uniform ivec2 OTHER_TEXTURES;
uniform sampler2D transmittanceTexture;


void main() {
  // See Bruneton and Collienne to understand the mapping
  float muSun = -0.2 + (gl_FragCoord.x - 0.5) / (float(OTHER_TEXTURES.x) - 1.0) * 1.2;
  float r = Rg + (gl_FragCoord.y - 0.5) / (float(OTHER_TEXTURES.y)) * (Rt - Rg);

  // We are calculating the Irradiance for L0, i.e., only the radiance coming from the Sun
  // direction is accounted for:
  // E[L0](x,s) = L0*dot(w,n) or 0 (if v!=s or the sun is occluded).
  // Because we consider the planet as a perfect sphere and we are considering only single
  // scattering here, the dot product dot(w,n) is equal to dot(s,n) that is equal to
  // dot(s, r/||r||) = muSun.
  renderTableColor = vec4(
    transmittance(transmittanceTexture, r, muSun, Rg, Rt) * max(muSun, 0.0),
    0.0
  );
}
