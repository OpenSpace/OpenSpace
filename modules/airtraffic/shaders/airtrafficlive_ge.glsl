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
#include "airtraffic_utilities.glsl"

layout (lines) in;
layout (line_strip, max_vertices = 2) out;
/*
layout(std430, binding = 5) buffer buf
{
    int nVertices;
};
*/

const float distThreshold = 20000.0;


//in float vs_vertexID[];
in vec4 vs_position[];
in vec4 vs_interpColor[];
in vec2 vs_latlon[];
in float vs_vertexID[];
noperspective in vec2 mathLine[];

out vec4 ge_position;
out vec4 ge_interpColor;
noperspective out vec2 ge_mathLine;

uniform float trailSize;

 void main(){

    float dist = greatCircleDistance(vs_latlon[0].x, vs_latlon[0].y, vs_latlon[1].x, vs_latlon[1].y);
    vec4 color = vs_interpColor[0];
    
    if(length(gl_in[0].gl_Position) < EPSILON || length(gl_in[1].gl_Position) < EPSILON || dist > distThreshold) {
        return;
    }

    //atomicAdd(nVertices, 1);

    ge_position = vs_position[0];
    ge_interpColor = vs_interpColor[0];
    ge_mathLine = mathLine[0];
    gl_Position = gl_in[0].gl_Position;
    EmitVertex();

    ge_position = vs_position[1];
    ge_interpColor = vs_interpColor[1];
    ge_mathLine = mathLine[1];
    gl_Position = gl_in[1].gl_Position;
    EmitVertex();

    EndPrimitive();
 }

