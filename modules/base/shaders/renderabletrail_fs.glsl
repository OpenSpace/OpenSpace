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

in vec4 vs_positionScreenSpace;
in vec4 vs_gPosition;
in float fade;
in float v_pointSize;

uniform vec3 color;
uniform int renderPhase;
uniform float opacity = 1.0;

// Fragile! Keep in sync with RenderableTrail::render::RenderPhase 
#define RenderPhaseLines 0
#define RenderPhasePoints 1

#define Delta 0.25


Fragment getFragment() {
    Fragment frag;
    frag.color = vec4(color * fade, fade * opacity);
    frag.depth = vs_positionScreenSpace.w;
    frag.blend = BLEND_MODE_ADDITIVE;

    vec4 depthCorrection = vec4(0.0, 0.0, 100.0, 0.0);

    if (renderPhase == RenderPhasePoints) {
        // Use the length of the vector (dot(circCoord, circCoord)) as factor in the
        // smoothstep to gradually decrease the alpha on the edges of the point
        vec2 circCoord = 2.0 * gl_PointCoord - 1.0;
        //float circleClipping = 1.0 - smoothstep(1.0 - Delta, 1.0, dot(circCoord, circCoord));        
        float circleClipping = smoothstep(1.0, 1.0 - Delta, dot(circCoord, circCoord));
        float transparencyCorrection = frag.color.a * circleClipping;
        if (transparencyCorrection < 0.9) {
            discard;
        }

        frag.color.a = transparencyCorrection;
    }    


    // G-Buffer
    // JCC: The depthCorrection here is a temporary tweak
    // to fix precision problems.
    frag.gPosition = vs_gPosition + depthCorrection;

    // There is no normal here
    frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);

    return frag;
}
