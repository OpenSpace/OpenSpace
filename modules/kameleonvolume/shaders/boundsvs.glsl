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
 
#version __CONTEXT__

layout(location = 0) in vec3 vertPosition;

uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;

out vec4 positionLocalSpace;
out vec4 positionCameraSpace;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {

    positionLocalSpace = vec4(vertPosition, 1.0);
    positionCameraSpace = modelViewTransform * positionLocalSpace;

    vec4 positionClipSpace = projectionTransform * positionCameraSpace;
    vec4 positionScreenSpace = z_normalization(positionClipSpace);
    
    //positionScreenSpace.z = 1.0;
    // project the position to view space
    gl_Position = positionScreenSpace;
}
