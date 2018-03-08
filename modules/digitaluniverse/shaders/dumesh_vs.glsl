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

#version __CONTEXT__

#include "PowerScaling/powerScaling_vs.hglsl"

in vec3 in_position;

uniform mat4 modelViewProjectionTransform;
uniform dmat4 modelViewTransform;
uniform dmat4 projectionTransform;
//uniform float scaleFactor;

uniform dmat4 offsetMVPMatrix;
uniform dmat4 modelMatrix;
uniform dvec4 offset;
//uniform dvec3 cameraPosition;

out float vs_screenSpaceDepth;
out vec4 vs_positionViewSpace;

void main() {
    //vec4 positionClipSpace   = modelViewProjectionTransform * vec4(in_position, 1.0);
    //vec4 positionScreenSpace = z_normalization(positionClipSpace);
    //dvec4 offset = dvec4(0.308567756E17, 0.308567756E17, 0.308567756E17, 0.0);
    //dvec4 offset = dvec4(0.1E17, 0.1E17, 0.1E17, 0.0);
    //dvec4 offset = dvec4(0.9999 * cameraPosition, 0.0);
    // dvec4 positionClipSpace = offsetMVPMatrix * 
    // (-(dvec4(cameraPosition,0.0) - offset) + ((modelMatrix * dvec4(in_position, 1.0)) - offset));
    dvec4 noffset = offset;
    noffset.w = 0.0;
    dvec4 positionClipSpace = offsetMVPMatrix * ((modelMatrix * dvec4(in_position, 1.0)) - noffset);
    vec4 positionScreenSpace = z_normalization(vec4(positionClipSpace));

    vs_screenSpaceDepth  = positionScreenSpace.w;
    //vs_positionViewSpace = vec4(positionViewSpace);

    gl_Position = positionScreenSpace;
}
