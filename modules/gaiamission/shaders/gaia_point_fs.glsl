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

#include "floatoperations.glsl"

layout (location = 0) out vec4 outColor;

// Keep in sync with renderablegaiastars.h:ColumnOption enum
const int COLUMNOPTION_STATIC = 0;
const int COLUMNOPTION_MOTION = 1; 
const int COLUMNOPTION_COLOR = 2;
const float ONE_PARSEC = 3.08567758e16; // 1 Parsec

in vec2 ge_brightness;
in vec4 ge_gPosition;

uniform sampler1D colorTexture;
uniform float luminosityMultiplier;
uniform int columnOption;
uniform float viewScaling;

vec4 bv2rgb(float bv) {
    // BV is [-0.4, 2.0]
    float st = (bv + 0.4) / (2.0 + 0.4);
    return texture(colorTexture, st);
}

void main() {

    // Assume all stars has equal luminosity as the Sun when no magnitude is loaded.
    float luminosity = 1.0;
    vec4 color = vec4(luminosity);

    // Calculate the color and luminosity if we have the magnitude and B-V color.
    if ( columnOption == COLUMNOPTION_COLOR ) {
        color = bv2rgb(ge_brightness.y);

        // Absolute magnitude is brightness a star would have at 10 pc away.
        float absoluteMagnitude = ge_brightness.x;

        // From formula: MagSun - MagStar = 2.5*log(LumStar / LumSun), it gives that:
        // LumStar = 10^(1.89 - 0.4*Magstar) , if LumSun = 1 and MagSun = 4.72
        luminosity = pow(10.0, 1.89 - 0.4 * absoluteMagnitude);

        // If luminosity is really really small then set it to a static low number.
        if (luminosity < 0.001) {
            luminosity = 0.001;
        }
    }

    // Luminosity decrease by squared distance [measured in Pc].
    float observedDistance = safeLength(ge_gPosition / viewScaling) / ONE_PARSEC;
    luminosity /= pow(observedDistance, 2.0);

    // Multiply our color with the mluminosity as well as a user-controlled property.
    color *= luminosity * pow(luminosityMultiplier, 3.0);

    if (color.a < 0.0001) {
        discard;
    }

    outColor = color;
}
