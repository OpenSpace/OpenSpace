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
//#include "fragment.glsl"
#include "floatoperations.glsl"

layout (location = 0) out vec4 outColor;

// Keep in sync with renderablegaiastars.h:ColumnOption enum
const int COLUMNOPTION_STATIC = 0;
const int COLUMNOPTION_MOTION = 1; 
const int COLUMNOPTION_COLOR = 2;
const float MIN_DIST = 3.08567758e18; // 100 Pc - When we should star to decrease alpha

in vec4 vs_position;
in vec3 ge_velocity;
in vec2 ge_brightness;
in vec4 ge_gPosition;
in vec2 texCoord;
in float ge_cameraDist;
in float ge_starDistFromOrigin;

uniform sampler2D psfTexture;
uniform sampler1D colorTexture;
uniform float luminosityMultiplier;
uniform float sharpness;
uniform int columnOption;
uniform float viewScaling;

vec4 bv2rgb(float bv) {
    // BV is [-0.4, 2.0]
    float st = (bv + 0.4) / (2.0 + 0.4);
    return texture(colorTexture, st);
}

void main() {
//Fragment getFragment() {

    //outColor = vec4(1.0);
    //return;

    vec4 color = vec4(1.0);

    // Calculate the color and luminosity.
    if ( columnOption == COLUMNOPTION_COLOR ) {
        color = bv2rgb(ge_brightness.y);

        // Absolute magnitude is brightness a star would have at 10 pc away.
        float absoluteMagnitude = ge_brightness.x;

        // From formula: MagSun - MagStar = 2.5*log(LumStar / LumSun), it gives that:
        // LumStar = 10^(1.89 - 0.4*Magstar) , if LumSun = 1 and MagSun = 4.72
        float luminosity = pow(10.0, 1.89 - 0.4 * absoluteMagnitude);

        // If luminosity is really really small then set it to a static low number.
        if (luminosity < 0.001) {
            luminosity = 0.001;
        }

        // Luminosity decrease by squared distance.
        float observedDistance = safeLength(ge_gPosition / viewScaling);
        //luminosity /= pow(observedDistance, 2.0);

        // TODO: Save color to FBO.
        color *= luminosity * luminosityMultiplier;
        outColor = color;
        return;
    }

    vec4 textureColor = texture(psfTexture, texCoord);
    vec4 fullColor = vec4(color.rgb, textureColor.a);
    fullColor.a = pow(fullColor.a, sharpness);
    //fullColor.a = clamp(fullColor.a, 0.0f, 1.0f);

    // Decrease alpha in center when camera moves further away.
    /*if (ge_cameraDist > MIN_DIST && ge_starDistFromOrigin < (ge_cameraDist / 5.0f)) {
        float normDist = ((ge_cameraDist / 5.0f) - ge_starDistFromOrigin) / (ge_cameraDist * 3.0);
        fullColor.a -= clamp(normDist, 0.0f, 1.0f);
    }*/

    if (fullColor.a == 0.0) {
        discard;
    }

    outColor = fullColor;

    /*Fragment frag;
    frag.color = fullColor;
    frag.depth = safeLength(vs_position);
    frag.gPosition = ge_gPosition;
    frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
    frag.blend = BLEND_MODE_NORMAL;

    return frag;*/
}
