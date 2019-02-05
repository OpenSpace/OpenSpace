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

layout (location = 0) out vec4 finalColor;

uniform float backgroundConstant;
uniform float backgroundExposure;
uniform float blackoutFactor;
uniform float gamma;
uniform float maxWhite;
uniform float aveLum;
uniform int toneMapOperator;

uniform sampler2D deferredResultsTexture;

in vec2 texCoord;

vec4 adaptiveToneMap() {
    int i;
    float lum[25];
    vec2 tex_scale = vec2(1.0) / textureSize(deferredResultsTexture, 0);

    for (i = 0; i < 25; i++)
    {
        vec2 tc = (gl_FragCoord.xy + 3.5 * vec2(i % 5 - 2, i / 5 - 2));
        vec3 col = texture(deferredResultsTexture, tc * tex_scale).rgb;
        lum[i] = dot(col, vec3(0.3, 0.59, 0.11));
    }

    // Calculate weighted color of region
    vec3 vColor = texelFetch(deferredResultsTexture, ivec2(gl_FragCoord.xy), 0).rgb;

    float kernelLuminance = (
          (1.0  * (lum[0] + lum[4] + lum[20] + lum[24])) +
          (4.0  * (lum[1] + lum[3] + lum[5] + lum[9] +
                  lum[15] + lum[19] + lum[21] + lum[23])) +
          (7.0  * (lum[2] + lum[10] + lum[14] + lum[22])) +
          (16.0 * (lum[6] + lum[8] + lum[16] + lum[18])) +
          (26.0 * (lum[7] + lum[11] + lum[13] + lum[17])) +
          (41.0 * lum[12])
          ) / 273.0;

    // Compute the corresponding exposure
    float exposure = sqrt(8.0 / (kernelLuminance + 0.25));

    // Apply the exposure to this texel
    vec4 fColor; 
    fColor.rgb = 1.0 - exp2(-vColor * exposure);
    fColor.a = 1.0f;

    return fColor;
}


void main() {
    vec4 color = vec4(0.0);
    color = texture(deferredResultsTexture, texCoord);    
    //color = texelFetch(deferredResultsTexture, ivec2(gl_FragCoord.xy), 0);
    color.a *= blackoutFactor;
    
    if (toneMapOperator == EXPONENTIAL) {
        vec3 tColor = exponentialToneMapping(color.rgb, backgroundExposure, gamma);
        finalColor = vec4(tColor, color.a);    
    } else if (toneMapOperator == LINEAR) {
        vec3 tColor = linearToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == SIMPLE_REINHARD) {
        vec3 tColor = simpleReinhardToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == LUM_BASED_REINHARD) {
        vec3 tColor = lumaBasedReinhardToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == WHITE_PRESERVING) {
        vec3 tColor = whitePreservingLumaBasedReinhardToneMapping(color.rgb, backgroundExposure, maxWhite);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == ROM_BIN_DA_HOUSE) {
        vec3 tColor = RomBinDaHouseToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == FILMIC) {
        vec3 tColor = filmicToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == UNCHARTED) {
        vec3 tColor = Uncharted2ToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == COSTA) {
        vec3 tColor = jToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == ADAPTIVE) {
        vec3 tColor = vec3(adaptiveToneMap());
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == GLOBAL) {
        vec3 tColor = globalToneMappingOperatorRTR(color.rgb, backgroundExposure, maxWhite, aveLum);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    } else if (toneMapOperator == PHOTOGRAPHIC_REINHARD) {
        vec3 tColor = photographicReinhardToneMapping(color.rgb, backgroundExposure);
        finalColor = vec4(gammaCorrection(tColor, gamma), color.a);
    }
}