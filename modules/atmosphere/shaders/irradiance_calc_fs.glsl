/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#version 330

#include "atmosphere_common.glsl"

out vec4 renderTableColor;

uniform sampler2D transmittanceTexture;

void unmappingRAndMuSun(out float r, out float muSun) {
  // See Bruneton and Colliene to understand the mapping.
  muSun = -0.2f + (gl_FragCoord.x - 0.5f) / (float(OTHER_TEXTURES_W) - 1.0f) * (1.0f + 0.2f);
  r  = Rg + (gl_FragCoord.y - 0.5f) / (float(OTHER_TEXTURES_H) - 1.0f) * (Rt - Rg);
}

vec3 transmittanceLUT(const float r, const float mu) {
  // Given the position x (here the altitude r) and the view
  // angle v (here the cosine(v)= mu), we map this
  float u_r  = sqrt((r - Rg) / (Rt - Rg));
  // See Colliene to understand the different mapping.
  float u_mu = atan((mu + 0.15f) / (1.0f + 0.15f) * tan(1.5f)) / 1.5f;
  
  return texture(transmittanceTexture, vec2(u_mu, u_r)).rgb;
}

void main(void) {
  float muSun, r;
  unmappingRAndMuSun(r, muSun);
  // We are calculating the Irradiance for L0, i.e.,
  // only the radiance comming from sun direction is accounted:
  // E[L0](x,s) = L0*dot(w,n) or 0 (if v!=s or the sun is occluded).
  // Because we consider the Planet as a perfect sphere and we are
  // considering only single scattering here, the
  // dot product dot(w,n) is equal to dot(s,n) that is equal to
  // dot(s, r/||r||) = muSun.
  renderTableColor = vec4(transmittanceLUT(r, muSun) * clamp(muSun, 0.0, 1.0), 1.0);     
}
