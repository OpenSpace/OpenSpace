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

//in float gs_screenSpaceDepth;
in float vs_screenSpaceDepth;
in vec4 colorMap;

uniform vec3 color;
uniform float alphaValue;
uniform bool hasColorMap;

Fragment getFragment() {
    Fragment frag;

    if (alphaValue == 0.0) {
        discard;
    }

    if (hasColorMap) {
        frag.color = vec4(colorMap.xyz, alphaValue);        
    }
    else {
        frag.color = vec4(color, alphaValue);
    }
    
    //frag.depth = gs_screenSpaceDepth;
    frag.depth = vs_screenSpaceDepth;
    frag.gPosition  = vec4(1e27, 1e27, 1e27, 1.0);
    frag.gNormal    = vec4(0.0, 0.0, 0.0, 1.0);

    return frag;
}
