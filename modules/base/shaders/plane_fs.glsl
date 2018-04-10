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

in float vs_screenSpaceDepth;
in vec2 vs_st;
in vec4 vs_gPosition;
in vec3 vs_gNormal;

uniform sampler2D texture1;
uniform bool additiveBlending;
uniform float opacity = 1.0;


Fragment getFragment() {
    Fragment frag;
    if (gl_FrontFacing) {
        frag.color = texture(texture1, vs_st);
    }
    else {
        frag.color = texture(texture1, vec2(1 - vs_st.s, vs_st.t));
    }

    frag.color.a *= opacity;
    if (frag.color.a == 0.0) {
        discard;
    }

    frag.depth = vs_screenSpaceDepth;

    if (additiveBlending) {
        frag.blend = BLEND_MODE_ADDITIVE;
    }

    // G-Buffer 
    frag.gPosition  = vs_gPosition;
    frag.gNormal    = vec4(vs_gNormal, 1.0);
    
    return frag;
}
