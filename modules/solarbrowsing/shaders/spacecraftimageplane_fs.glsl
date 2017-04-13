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

uniform float time;
uniform sampler2D texture1;
uniform sampler1D texture2;
uniform bool additiveBlending;

uniform int currentActiveChannel;
uniform int minIntensity;
uniform int maxIntensity;
uniform float expTime;

in vec2 vs_st;
in vec4 vs_positionScreenSpace;

const float clipmins[10] = float[](20.0, 220.0, 4000.0, 0.1, 0.7, 10.0, 20.0, 7.0, 0.2, 0.4);
const float clipmax[10]  = float[](400.0, 5000.0, 20000.0, 30.0, 500.0, 2000.0, 2500.0, 1500.0, 150.0, 80.0);
const float log10inv = 1.0 / log(10);

#include "fragment.glsl"

Fragment getFragment() {

    float intensityOrg = float(texture(texture1, vs_st).r);
    float outa;

    float intensity = intensityOrg * (1.0 / expTime);
    float mini = float(minIntensity) * (1.0 / expTime);
    float maxi = float(maxIntensity) * (1.0 / expTime);

    // Handle 171 differently
    if (currentActiveChannel == 5) {
        intensity -= 5;
        maxi -= 5;
        mini -= 5;

        intensity = clamp(intensity, 0.01, maxi);
        mini = clamp(mini, 0.01, maxi);
        maxi = clamp(maxi, 0.01, maxi); // Remove

        intensity = pow(intensity, 0.35);
        mini = pow(mini, 0.35);
        maxi = pow(maxi, 0.35);

        intensity = clamp(intensity, 0.01, 13.0);
        mini = clamp(mini, 0.01, 13.0);
        maxi = clamp(maxi, 0.01, 13.0);

    } else {
        const float cmin = clipmins[currentActiveChannel];
        const float cmax = clipmax[currentActiveChannel];
        intensity = clamp(intensity, cmin, cmax);
        mini = clamp(mini, cmin, cmax);
        maxi = clamp(maxi, cmin, cmax);

        intensity = log(intensity) / log(10);
        mini = log(mini) / log(10);
        maxi = log(maxi)/ log(10);
    }

    // Normalize
    intensity = (intensity - mini) / (maxi - mini);

    vec4 diffuse = texture(texture2, intensity);

    if (diffuse.a == 0.0)
        discard;

    Fragment frag;
    frag.color = diffuse;
    frag.depth = vs_positionScreenSpace.w;

    if (additiveBlending) {
        frag.blend = BLEND_MODE_ADDITIVE;
    }
    return frag;
}
