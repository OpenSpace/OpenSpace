/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2017                                                             *
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

layout(points) in;
//layout(points, max_vertices = 13) out;
layout(line_strip, max_vertices = 13) out;

in float vs_screenSpaceDepth[];
in float vs_scaleFactor[];
in vec4 colorMap[]

uniform int sides;

out float gs_screenSpaceDepth;

const float PI = 3.1415926;

void main()
{
    gs_screenSpaceDepth = vs_screenSpaceDepth[0];
    
    for (int i = 0; i <= sides; i++) {
        // Angle between each side in radians
        float ang = PI * 2.0 / float(sides) * i;

        // Offset from center of point (0.3 to accomodate for aspect ratio)
        //vec4 offset = vec4(cos(ang) * 0.003, -sin(ang) * 0.004, 0.0, 0.0);
        //gl_Position = gl_in[0].gl_Position + offset;

        vec4 offset = vec4(cos(ang) * gl_in[0].gl_Position[0], -sin(ang) * gl_in[0].gl_Position[1],
                            gl_in[0].gl_Position[2] , gl_in[0].gl_Position[3]);
        gl_Position = offset;
        
        EmitVertex();
    }
    
    //gl_Position = gl_in[0].gl_Position;// + offset;
    //EmitVertex();

    EndPrimitive();
}