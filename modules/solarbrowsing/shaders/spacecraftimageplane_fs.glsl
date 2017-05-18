/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

uniform float sharpenValue;
uniform float contrastValue;
uniform float opacityValue;
uniform float gammaValue;
uniform float planeOpacity;

uniform float imageSize;

uniform bool hasLut;

uniform dvec2 magicPlaneOffset;

in vec2 vs_st;
in vec4 vs_positionScreenSpace;

//float intensityOrg = texture(imageryTexture, vec2(vs_st.x /* +  ( (1024.0 - 1021.81 ) / 2048  )*/ , vs_st.y /* + (  (1024.0 - 926.171) / 2048) */)).r;

#include "fragment.glsl"

/* shapen 5x5 filter
  0  -4  -6  -4  0  14
 -4 -16 -24 -16 -4  64
 -6 -24 220 -24 -6  60
 -4 -16 -24 -16 -4  64
  0  -4  -6  -4  0  14
  */
float sharpen(vec2 texPos) {
    float x = 1.0 / (imageSize);
    float y = 1.0 / (imageSize);

    float accumulator = 0.0;
    accumulator -= texture(imageryTexture,texPos + vec2(-  x, -y-y)).r * 4;
    accumulator -= texture(imageryTexture,texPos + vec2(   0, -y-y)).r * 6;
    accumulator -= texture(imageryTexture,texPos + vec2(   x, -y-y)).r * 4;

    accumulator -= texture(imageryTexture,texPos + vec2(-x-x,   -y)).r * 4;
    accumulator -= texture(imageryTexture,texPos + vec2(-  x,   -y)).r * 16;
    accumulator -= texture(imageryTexture,texPos + vec2(   0,   -y)).r * 24;
    accumulator -= texture(imageryTexture,texPos + vec2(   x,   -y)).r * 16;
    accumulator -= texture(imageryTexture,texPos + vec2( x+x,   -y)).r * 4;

    accumulator -= texture(imageryTexture,texPos + vec2(-x-x,    0)).r * 6;
    accumulator -= texture(imageryTexture,texPos + vec2(-  x,    0)).r * 24;
    accumulator += texture(imageryTexture,texPos + vec2(   0,    0)).r * 220;
    accumulator -= texture(imageryTexture,texPos + vec2(   x,    0)).r * 24;
    accumulator -= texture(imageryTexture,texPos + vec2( x+x,    0)).r * 6;

    accumulator -= texture(imageryTexture,texPos + vec2(-x-x,    y)).r * 4;
    accumulator -= texture(imageryTexture,texPos + vec2(-  x,    y)).r * 16;
    accumulator -= texture(imageryTexture,texPos + vec2(   0,    y)).r * 24;
    accumulator -= texture(imageryTexture,texPos + vec2(   x,    y)).r * 16;
    accumulator -= texture(imageryTexture,texPos + vec2( x+x,    y)).r * 4;

    accumulator -= texture(imageryTexture,texPos + vec2(-  x,  y+y)).r * 4;
    accumulator -= texture(imageryTexture,texPos + vec2(   0,  y+y)).r * 6;
    accumulator -= texture(imageryTexture,texPos + vec2(   x,  y+y)).r * 4;

    return clamp(texture(imageryTexture,texPos).r + (accumulator / 30.0) * sharpenValue, 0.0, 1.0);
}

float contrast(float intensity) {
    return min(clamp(0.5 + (intensity - 0.5) * (1 + contrastValue/10.0), 0.0, 1.0), sqrt(intensity) + intensity);
}


Fragment getFragment() {

    //float intensityOrg = texture(imageryTexture, vec2(vs_st.x + magicPlaneOffset.x, vs_st.y + magicPlaneOffset.y )).r;
    float intensityOrg = sharpen(vec2( vs_st.x + magicPlaneOffset.x, vs_st.y + magicPlaneOffset.y));
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

    outColor = vec4(outColor.xyz, planeOpacity);

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    if (additiveBlending) {
        frag.blend = BLEND_MODE_ADDITIVE;
    }
    return frag;
}
