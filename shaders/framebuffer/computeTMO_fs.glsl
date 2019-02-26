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

uniform float key;
uniform float Ywhite;
uniform float sat;

uniform sampler2D hdrSampler;

in vec2 texCoord;

vec3 toneMapGlobal(vec3 hdrColor, float logAvgLum) {
    vec3 XYZ = srgbToXYZ(hdrColor);

    float Y = (key / logAvgLum) * XYZ.y;
    float Yd = (Y * (1.0 + Y/(Ywhite * Ywhite))) / (1.0 + Y);

    return pow(hdrColor / XYZ.y, vec3(sat)) * Yd;
}

vec3 toneMapLocal(vec3 hdrColor, float logAvgLum) {
    vec3 XYZ = srgbToXYZ(hdrColor);

    float Y = (key / logAvgLum) * XYZ.y;
    float LocalAdaptation;
    float factor = key / logAvgLum;
    float epsilon = 0.05;
    float phi = 8.0;
    float scale[7] = float[7](1, 2, 4, 8, 16, 32, 64);
    
    for (int i = 0; i < 7; ++i) {
        float V1 = exp(texture(hdrSampler, texCoord, i).a) * factor;
        float V2 = exp(texture(hdrSampler, texCoord, i+1).a) * factor;

        if ( abs(V1-V2) / ((key * pow(2, phi) / (scale[i] * scale[i])) + V1)
        > epsilon ) {
            LocalAdaptation = V1;
            break;
        } else {
            LocalAdaptation = V2;
        }
    }

    float Yd = Y / (1.0 + LocalAdaptation);

    return pow(hdrColor / XYZ.y, vec3(sat)) * Yd;
}

void main() {
    vec3 hdrColor = texture(hdrSampler, texCoord).rgb;

    float logAvgLum = exp(texture(hdrSampler, texCoord, 20).a);

    //finalColor.rgb = toneMapGlobal(hdrColor, logAvgLum);

    finalColor = vec4(toneMapGlobal(hdrColor, logAvgLum), 1.0);
    finalColor = vec4(1.0, 0.0, 0.0, 1.0);
}
