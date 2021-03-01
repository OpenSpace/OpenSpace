/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

in vec4 vs_position;
in float vertexID;
in float trailS;
in vec4 interpColor;
uniform float opacity = 1.0;

Fragment getFragment() {
    Fragment frag;


    
    //float invert = pow((1.0 - vertexDistance_f), lineFade);
    //float fade = clamp(invert, 0.0, 1.0);

    // Currently even fully transparent lines can occlude other lines, thus we discard
    // these fragments since debris and satellites are rendered so close to each other
   // if (interpColor.w < 0.05) {
     //   discard;
    //}

    // Use additive blending for some values to make the discarding less abrupt
    //if (interpColor.w < 0.15) {
      //  frag.blend = BLEND_MODE_ADDITIVE;
    //}

    //frag.color = vec4(1.0, 0.0, 0.0, fade * opacity);

    frag.color = interpColor;//vec4(1.0, 0.0, 0.0, distance(vec2(vs_position), gl_PointCoord);
    frag.gPosition = vs_position;
    frag.depth = vs_position.w;
    frag.gNormal = vec4(1.0, 1.0, 1.0, 0.0);

    return frag;
}
