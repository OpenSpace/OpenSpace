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
uniform sampler2D imageryTexture;
uniform sampler1D lut;
uniform bool additiveBlending;

uniform int hasLut;

uniform dvec2 magicPlaneOffset;

in vec2 vs_st;
in vec4 vs_positionScreenSpace;

#include "fragment.glsl"

Fragment getFragment() {

    float intensityOrg = texture(imageryTexture, vec2(vs_st.x + magicPlaneOffset.x, vs_st.y + magicPlaneOffset.y)).r;

    vec4 outColor;
    if (hasLut == 1) {
        outColor = texture(lut, intensityOrg);
      //  outColor = vec4(1.0, 0.0, 0.0, 1.0);
    } else {
        outColor = vec4(intensityOrg, intensityOrg, intensityOrg, 1.0);
    }

    //utColor = texture(lut, intensityOrg);
   // outColor = clamp(outColor + intensityOrg, 0.0, 1.0);

    // if (hasLut) {
    //     outColor = vec4(1.0, 0.0, 0.0, 1.0);
    // } else {
    //     outColor = vec4(1.0, 1.0, 1.0, 1.0);
    // }

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    if (additiveBlending) {
        frag.blend = BLEND_MODE_ADDITIVE;
    }
    return frag;
}
