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
#include "floatoperations.glsl"

in vec4 vs_positionScreenSpace;
in vec4 vs_gPosition;
in vec4 vs_color;

Fragment getFragment() {

    Fragment frag;
    frag.depth = vs_positionScreenSpace.w;
    //frag.blend = BLEND_MODE_ADDITIVE;

    frag.color = vs_color;

    // G-Buffer
    // JCC: The depthCorrection here is a temporary tweak
    // to fix precision problems.
    
    vec4 depthCorrection = vec4(0.0,0.0,100,0.0);
    frag.gPosition = vs_gPosition + depthCorrection;

    // For rendering inside earth atmosphere we need to set a normal for our line
    // Todo: calculate normal correctly 
    // currently normal is in object space
    frag.gNormal= vec4(0.0, 0.0, -1.0, 1.0); 

    return frag;
}
