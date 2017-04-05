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

in vec2 vs_st;
in vec4 vs_positionScreenSpace;

#include "fragment.glsl"

float colormap_red(float x) {
    return 1.448953446096850 * x - 5.02253539008443e-1;
}

float colormap_green(float x) {
    return 1.889376646180860 * x - 2.272028094820020e2;
}

float colormap_blue(float x) {
    return 3.92613636363636 * x - 7.46528409090909e+2;
}

Fragment getFragment() {
    float intensityScaled = texture(texture1, vs_st).r;

    vec4 diffuse;

    // const float c0t = temp;
    // const float c1t = sqrt(temp) * sqrt(255.0);
    // const float c2t = pow(temp, 2.0) / 255.0;
    // const float c3t = ( ( c1t + c2t / 2.0) * 255.0 / ( 255.0 + 255.0 / 2.0));

    // float c0 = clamp(c0t / 255.0, 0.0, 1.0);
    // float c1 = clamp(c1t / 255.0, 0.0, 1.0);
    // float c2 = clamp(c2t / 255.0, 0.0, 1.0);
    // float c3 = clamp(c3t / 255.0, 0.0, 1.0);

    float c0 = clamp(texture(texture2, intensityScaled).r, 0.0, 1.0);
    float c1 = clamp(texture(texture2, intensityScaled).g, 0.0, 1.0);
    float c2 = clamp(texture(texture2, intensityScaled).b, 0.0, 1.0);
    float c3 = clamp(texture(texture2, intensityScaled).a, 0.0, 1.0);

    float r0 = clamp(colormap_red(intensityScaled * 255.0) / 255.0, 0.0, 1.0);
    float g0 = clamp(colormap_green(intensityScaled * 255.0) / 255.0, 0.0, 1.0);
    float b0 = clamp(colormap_blue(intensityScaled * 255.0) / 255.0, 0.0, 1.0);

    float r = c2;
    float g = c3;
    float b = c0;

    diffuse = vec4(r, g, b, 1.0);

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
