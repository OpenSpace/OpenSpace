/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

in float vs_positionDepth;
in vec4 vs_gPosition;
in float fade;
noperspective in vec2 mathLine;

uniform vec3 color;
uniform int renderPhase;
uniform float opacity = 1.0;
uniform float lineWidth;

// Fragile! Keep in sync with RenderableTrail::render::RenderPhase 
#define RenderPhaseLines 0
#define RenderPhasePoints 1

#define Delta 0.25


Fragment getFragment() {
    Fragment frag;
    frag.color = vec4(color * fade, fade * opacity);
    frag.depth = vs_positionDepth;
    frag.blend = BLEND_MODE_ADDITIVE;

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

    double distanceCenter = length(mathLine - vec2(gl_FragCoord.xy));
    double dLW = double(lineWidth);
    float blendFactor = 20;
    
    if (distanceCenter > dLW) {
        frag.color.a = 0.0;
    }
    else {
        frag.color.a *= pow(float((dLW - distanceCenter) / dLW), blendFactor);
    }

    frag.gPosition = vs_gPosition;

    // There is no normal here
    frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);

    return frag;
}
