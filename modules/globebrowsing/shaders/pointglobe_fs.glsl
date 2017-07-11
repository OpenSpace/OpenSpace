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

#include "fragment.glsl"

//uniform vec3 directionToSunViewSpace;
uniform vec3 positionCameraSpace;
//uniform float lightOverflow;

in vec4 vs_positionClipSpace;
in vec2 vs_positionModelSpace;

Fragment getFragment() {

    float alpha = 1 - sqrt(pow(vs_positionModelSpace.x, 2) + pow(vs_positionModelSpace.y, 2));
    alpha = pow(alpha, 3);
    Fragment frag;
    frag.color = vec4(1,1,1, alpha);// + vec4(1,1,1,1) * 0.0000000001 * directionToSunViewSpace.x;
    //frag.color *= 1 + (lightOverflow * lightOverflow) * 0.0001;
    frag.depth = vs_positionClipSpace.w;
    frag.blend = BLEND_MODE_ADDITIVE;
    return frag;
}

