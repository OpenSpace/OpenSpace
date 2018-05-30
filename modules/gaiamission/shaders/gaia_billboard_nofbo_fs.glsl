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
 
#include "fragment.glsl"
#include "floatoperations.glsl"

// Keep in sync with gaiaoptions.h:RenderOption enum
const int RENDEROPTION_STATIC = 0;
const int RENDEROPTION_COLOR = 1;
const int RENDEROPTION_MOTION = 2; 
const float ONE_PARSEC = 3.08567758e16; // 1 Parsec
const float DEFAULT_DEPTH = 3.08567758e19; // 1000 Pc

in vec2 ge_brightness;
in vec4 ge_gPosition;
in vec2 texCoord;
in float ge_starDistFromSun;
in float ge_cameraDistFromSun;

uniform sampler2D psfTexture;
uniform sampler1D colorTexture;
uniform float luminosityMultiplier;
uniform float sharpness;
uniform int renderOption;
uniform float viewScaling;

vec3 bv2rgb(float bv) {
    // BV is [-0.4, 2.0]
    float st = (bv + 0.4) / (2.0 + 0.4);
    return texture(colorTexture, st).rgb;
}

Fragment getFragment() {

    // Assume all stars has equal luminosity as the Sun when no magnitude is loaded.
    float luminosity = 1.0;
    vec3 color = vec3(luminosity);
    float ratioMultiplier = 0.03;

    vec4 textureColor = texture(psfTexture, texCoord);
    textureColor.a = pow(textureColor.a, sharpness);
    if (textureColor.a < 0.01) {
        discard;
    }

    // Calculate the color and luminosity if we have the magnitude and B-V color.
    if ( renderOption != RENDEROPTION_STATIC ) {
        color = bv2rgb(ge_brightness.y);
        ratioMultiplier = 0.5;

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

    // Luminosity decrease by {squared} distance [measured in Pc].
    float observedDistance = safeLength(ge_gPosition / viewScaling) / ONE_PARSEC;
    luminosity /= observedDistance; //pow(observedDistance, 2.0);

    // Multiply our color with the luminosity as well as a user-controlled property.
    color *= luminosity * pow(luminosityMultiplier, 2.0);

    // Decrease contributing brightness for stars in central cluster.
    if ( ge_cameraDistFromSun > ge_starDistFromSun ) {
        float ratio = ge_starDistFromSun / ge_cameraDistFromSun;
        color *= ratio * ratioMultiplier;
    }

    // Use truncating tonemapping here so we don't overexposure individual stars.
    color = 1.0 - 1.0 * exp(-5.0 * color.rgb);

    Fragment frag;
    frag.color = vec4(color, textureColor.a);;
    // Place stars at back to begin with. 
    frag.depth = DEFAULT_DEPTH;
    frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
    frag.blend = BLEND_MODE_NORMAL;

    return frag;
}

