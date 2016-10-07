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

void getRAndMu(out float r, out float mu) {
    // See Bruneton and Colliene to understand the mapping.
    mu = -0.2 + (gl_FragCoord.x - 0.5) / (float(OTHER_TEXTURES_W) - 1.0) * (1.0 + 0.2);
    r  = Rg + (gl_FragCoord.y - 0.5) / (float(OTHER_TEXTURES_H) - 1.0) * (Rt - Rg);
}

vec3 transmittance(const float r, const float mu) {
    float u_r  = sqrt((r - Rg) / (Rt - Rg));
    // See Colliene to understand the different mapping.
    float u_mu = atan((mu + 0.15) / (1.0 + 0.15) * tan(1.5)) / 1.5;
    
    return texture(transmittanceTexture, vec2(u_mu, u_r)).rgb;
}

void main(void) {
    float mu, r;
    getRAndMu(r, mu);
    renderTableColor = vec4(transmittance(r, mu) * max(mu, 0.0), 1.0);     
}