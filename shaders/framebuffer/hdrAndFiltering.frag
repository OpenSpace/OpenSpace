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

#version __CONTEXT__

#include "hdr.glsl"

#define HSV_COLOR 0u
#define HSL_COLOR 1u

layout (location = 0) out vec4 finalColor;

uniform float backgroundConstant;
uniform float hdrExposure;
uniform float blackoutFactor;
uniform float gamma;
uniform float maxWhite;
uniform float Hue;
uniform float Saturation;
uniform float Value;
uniform float Lightness;
uniform int toneMapOperator;
uniform uint colorSpace;
uniform int nAaSamples;

uniform sampler2DMS hdrFeedingTexture;

in vec2 texCoord;

void main() {
    vec4 color = vec4(0.0);

    // Resolving...
    for (int i = 0; i < nAaSamples; i++) {
        color += texelFetch(hdrFeedingTexture, ivec2(gl_FragCoord), i);
    }

    color /= nAaSamples;
    color.rgb *= blackoutFactor;
    
    vec3 tColor = vec3(0.0);
    if (toneMapOperator == EXPONENTIAL) {
        tColor = exponentialToneMapping(color.rgb, hdrExposure, gamma);
    } else if (toneMapOperator == LINEAR) {
        tColor = linearToneMapping(color.rgb, hdrExposure);
    } else if (toneMapOperator == SIMPLE_REINHARD) {
        tColor = simpleReinhardToneMapping(color.rgb, hdrExposure);
    } else if (toneMapOperator == LUM_BASED_REINHARD) {
        tColor = lumaBasedReinhardToneMapping(color.rgb);
    } else if (toneMapOperator == WHITE_PRESERVING) {
        tColor = whitePreservingLumaBasedReinhardToneMapping(color.rgb, maxWhite);
    } else if (toneMapOperator == ROM_BIN_DA_HOUSE) {
        tColor = RomBinDaHouseToneMapping(color.rgb);
    } else if (toneMapOperator == FILMIC) {
        tColor = filmicToneMapping(color.rgb);
    } else if (toneMapOperator == UNCHARTED) {
        tColor = Uncharted2ToneMapping(color.rgb, hdrExposure);
    } else if (toneMapOperator == COSTA) {
        tColor = jToneMapping(color.rgb, hdrExposure);
    } else if (toneMapOperator == PHOTOGRAPHIC_REINHARD) {
        tColor = photographicReinhardToneMapping(color.rgb);
    }

    if (colorSpace == HSL_COLOR) {
        vec3 hslColor = rgb2hsl(tColor);
        hslColor.x *= Hue;
        hslColor.y *= Saturation;
        hslColor.z *= Lightness;

        finalColor = vec4(gammaCorrection(hsl2rgb(hslColor), gamma), color.a); 
    } else if (colorSpace == HSV_COLOR) {
        vec3 hsvColor = rgb2hsv(tColor);
        hsvColor.x *= Hue;
        hsvColor.y *= Saturation;
        hsvColor.z *= Value;

        finalColor = vec4(gammaCorrection(hsv2rgb(hsvColor), gamma), color.a);
    }
}