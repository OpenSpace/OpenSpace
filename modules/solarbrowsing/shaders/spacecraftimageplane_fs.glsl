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

Fragment getFragment() {
    float intensityScaled = texture(texture1, vs_st).r;

    vec4 diffuse;

    float intensityScaled2 = max(0.0, min(1.0, intensityScaled));
    float temp = floor(intensityScaled2 == 1.0 ? 255.0 : intensityScaled2 * 256.0);

    const float c0 = temp;
    const float c1 = sqrt(temp) * sqrt(255.0);
    const float c2 = pow(temp, 2.0) / 255.0;
    const float c3 = ( (c1 + c2 / 2.0) * 255.0 / ( 255.0 + 255.0 / 2.0));

    float rr = c2 / 255.0;
    float rg = c3 / 255.0;
    float rb = c0 / 255.0;

    // rr = clamp(rr, 0.0, 1.0);
    // rg = clamp(rg, 0.0, 1.0);
    // rb = clamp(rb, 0.0, 1.0);
    diffuse = vec4(rr, rg, rb, 1.0);

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
