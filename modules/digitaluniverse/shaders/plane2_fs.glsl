/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2017                                                             *
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

in float vs_screenSpaceDepth;
in vec2 vs_st;

uniform sampler2D galaxyTexture;
//uniform bool additiveBlending;
uniform float alphaValue;


Fragment getFragment() {
    Fragment frag;
    // if (gl_FrontFacing) {
    //     frag.color = texture(galaxyTexture, vs_st);
    // }
    // else {
    //     frag.color = texture(galaxyTexture, vec2(1 - vs_st.s, vs_st.t));
    // }

    frag.color = texture(galaxyTexture, vs_st);
    frag.color *= alphaValue;

    if (frag.color.a == 0.0) {
        discard;
    }

    // if (additiveBlending) {
    //     frag.blend = BLEND_MODE_ADDITIVE;
    // }

    frag.color = texture(galaxyTexture, vs_st);
    frag.depth = vs_screenSpaceDepth;

    return frag;
}
