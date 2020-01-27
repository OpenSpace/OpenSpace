/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

uniform float hdrExposure;
uniform float blackoutFactor;
uniform float gamma;
uniform float Hue;
uniform float Saturation;
uniform float Value;
uniform float Lightness;

uniform sampler2D hdrFeedingTexture;

in vec2 texCoord;

void main() {
    vec4 color = texture(hdrFeedingTexture, texCoord);
    color.rgb *= blackoutFactor;
    
    // Applies TMO
    vec3 tColor = toneMappingOperator(color.rgb, hdrExposure);
    
    // Color control
    vec3 hsvColor = rgb2hsv(tColor);
    hsvColor.x = (hsvColor.x + Hue);
    if (hsvColor.x > 360.0) {
        hsvColor -= 360.0;
    }
    hsvColor.y = clamp(hsvColor.y * Saturation, 0.0, 1.0);
    hsvColor.z = clamp(hsvColor.z * Value, 0.0, 1.0);

    // Gamma Correction
    finalColor = vec4(gammaCorrection(hsv2rgb(hsvColor), gamma), color.a);
}
