/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
uniform sampler2D imageryTexture;
uniform sampler1D lut;
uniform bool additiveBlending;

uniform float contrastValue;
uniform float gammaValue;
uniform float planeOpacity;
uniform bool hasLut;
uniform bool isCoronaGraph;

in vec2 vs_st;
in vec4 vs_positionScreenSpace;

#include "fragment.glsl"

float contrast(float intensity) {
    return min(clamp(0.5 + (intensity - 0.5) * (1 + contrastValue/10.0), 0.0, 1.0), sqrt(intensity) + intensity);
}

Fragment getFragment() {
    float intensityOrg = texture(imageryTexture, vec2(vs_st.x, 1 - vs_st.y)).r;
    intensityOrg = contrast(intensityOrg);

    vec4 outColor;
    if (hasLut) {
        outColor = texture(lut, intensityOrg);
    } else {
        outColor = vec4(intensityOrg, intensityOrg, intensityOrg, 1.0);
    }

    outColor.r = pow(outColor.r, gammaValue);
    outColor.g = pow(outColor.g, gammaValue);
    outColor.b = pow(outColor.b, gammaValue);

    if (planeOpacity == 0.0)
        discard;

    float absy = abs(0.5 - vs_st.y);
    float absx = abs(0.5 - vs_st.x);

    if (isCoronaGraph && length(outColor.xyz) < 0.10 && ( (absy * absy + absx * absx) > 0.25)) discard;

    outColor = vec4(outColor.xyz, planeOpacity);

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    if (additiveBlending) {
        frag.blend = BLEND_MODE_ADDITIVE;
    }
    return frag;
}