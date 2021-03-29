
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

#version __CONTEXT__

layout(location = 0) in vec3 in_position;

out float t;
out float vs_depth;
out vec4 vs_positionViewSpace;
out vec4 smoothColor;

uniform vec3 lineColor;
uniform float opacity;
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;

void main(){

    vs_positionViewSpace = vec4(modelViewTransform * dvec4(in_position, 1));
    vec4 positionScreenSpace = projectionTransform * vs_positionViewSpace;
    vs_depth = positionScreenSpace.w;
    gl_Position = positionScreenSpace;

    t = 1.f;

    if (gl_VertexID == 0) {
        smoothColor = vec4(0.0, 0.0, 0.0, opacity);
    }
    
    else if (gl_VertexID == 1) {
        smoothColor = vec4(lineColor, opacity);
    }

    else if (gl_VertexID == 2 ) {
        smoothColor = vec4(lineColor, opacity);
    }

    else {
        smoothColor = vec4(1.0, 1.0, 0.0, 1.0);
    }

    gl_Position.z = 0.f;
    
}
