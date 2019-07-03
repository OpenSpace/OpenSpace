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
uniform float aveLum;
uniform float Hue;
uniform float Saturation;
uniform float Value;
uniform float Lightness;
uniform int toneMapOperator;
uniform uint colorSpace;
uniform int nAaSamples;

//uniform sampler2D hdrFeedingTexture;
uniform sampler2DMS hdrFeedingTexture;

in vec2 texCoord;

// JCC: Change the next function to work with a MSAA texture
vec4 adaptiveToneMap() {
    // int i;
    // float lum[25];
    // // Non MSAAA variant:
    // //vec2 tex_scale = vec2(1.0) / textureSize(hdrFeedingTexture, 0);
    // vec2 tex_scale = vec2(1.0) / textureSize(hdrFeedingTexture);

    // for (i = 0; i < 25; i++)
    // {
    //     vec2 tc = (gl_FragCoord.xy + 3.5 * vec2(i % 5 - 2, i / 5 - 2));
    //     vec3 col = texture(hdrFeedingTexture, tc * tex_scale).rgb;
    //     lum[i] = dot(col, vec3(0.3, 0.59, 0.11));
    // }

    // // Calculate weighted color of region
    // vec3 vColor = texelFetch(hdrFeedingTexture, ivec2(gl_FragCoord.xy), 0).rgb;

    // float kernelLuminance = (
    //       (1.0  * (lum[0] + lum[4] + lum[20] + lum[24])) +
    //       (4.0  * (lum[1] + lum[3] + lum[5] + lum[9] +
    //               lum[15] + lum[19] + lum[21] + lum[23])) +
    //       (7.0  * (lum[2] + lum[10] + lum[14] + lum[22])) +
    //       (16.0 * (lum[6] + lum[8] + lum[16] + lum[18])) +
    //       (26.0 * (lum[7] + lum[11] + lum[13] + lum[17])) +
    //       (41.0 * lum[12])
    //       ) / 273.0;

    // // Compute the corresponding exposure
    // float exposure = sqrt(8.0 / (kernelLuminance + 0.25));

    // // Apply the exposure to this texel
    // vec4 fColor; 
    // fColor.rgb = 1.0 - exp2(-vColor * exposure);
    // fColor.a = 1.0f;

    // return fColor;
    return vec4(1.0, 0.0, 0.0, 1.0);
}


void main() {
    vec4 color = vec4(0.0);

    //color = texture(hdrFeedingTexture, texCoord);    
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
    } else if (toneMapOperator == ADAPTIVE) {
        tColor = vec3(adaptiveToneMap());
    } else if (toneMapOperator == GLOBAL) {
        tColor = globalToneMappingOperatorRTR(color.rgb, hdrExposure, maxWhite, aveLum);
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