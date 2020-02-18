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

#version __CONTEXT__

#include "PowerScaling/powerScaling_vs.hglsl"

in dvec4 in_position;
in dvec4 in_colormap;

uniform dmat4 modelViewProjectionTransform;
uniform float scaleFactor;

out float vs_screenSpaceDepth;
out float vs_scaleFactor;
out vec4 colorMap;

void main() {
    vec4 positionClipSpace = vec4(modelViewProjectionTransform * in_position);
    // positionClipSpace = vec4( modelViewProjectionTransform * 
    // vec4(0,0,0,1));
    vec4 positionScreenSpace = vec4(z_normalization(positionClipSpace));

    vs_screenSpaceDepth = positionScreenSpace.w;
    vs_scaleFactor = scaleFactor;
    colorMap = vec4(in_colormap);

    gl_PointSize = scaleFactor;
    gl_Position = positionScreenSpace;
}
