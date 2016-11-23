/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

// Fragile! Keep in sync with RenderableTrail::render::RenderPhase 
#define RenderPhaseLines 0
#define RenderPhasePoints 1

#define Delta 0.25

in vec4 vs_positionScreenSpace;
in float fade;

uniform vec3 color;
uniform int renderPhase;

#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;
    frag.color = vec4(color * fade, fade);
    frag.depth = vs_positionScreenSpace.w;
    frag.blend = BLEND_MODE_ADDITIVE;

    if (renderPhase == RenderPhasePoints) {
        // Use the length of the vector (dot(circCoord, circCoord)) as factor in the
        // smoothstep to gradually decrease the alpha on the edges of the point
        vec2 circCoord = 2.0 * gl_PointCoord - 1.0;
        frag.color.a *= 1.0 - smoothstep(1.0 - Delta, 1.0, dot(circCoord, circCoord));



        // if (dot(circCoord, circCoord) > 1.0) {

        // }
        // frag.color.a *= smoothstep();

        // // Check for length > 1.0 without a square root
        // frag.color.a *= smoothstep(dot(circCoord, circCoord), 1.0, 1.0 - 1.0 / pointSize);
        // if (dot(circCoord, circCoord) > 1.0) {
        //     discard;
        // }
    }


    return frag;
}
