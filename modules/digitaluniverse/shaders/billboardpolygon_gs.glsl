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
//layout(line_strip, max_vertices = 19) out;
layout(triangle_strip, max_vertices = 63) out;

uniform int sides;

const float PI = 3.1415926;

void main()
{
    // for (int i = 0; i <= sides; i++) {
    //     // Angle between each side in radians
    //     float ang = PI * 2.0 / float(sides) * i;

    //     // Offset from center of point (0.3 to accomodate for aspect ratio)
    //     //vec4 offset = vec4(cos(ang) * 0.003, -sin(ang) * 0.004, 0.0, 0.0);
    //     vec4 offset = vec4(cos(ang) * 0.8, -sin(ang) * 0.8, 0.0, 0.0);
    //     gl_Position = gl_in[0].gl_Position + offset;

    //     // vec4 offset = vec4(cos(ang) * gl_in[0].gl_Position[0], -sin(ang) * gl_in[0].gl_Position[1],
    //     //                     gl_in[0].gl_Position[2] , gl_in[0].gl_Position[3]);
    //     // gl_Position = offset;
        
    //     EmitVertex();
    // }
    // EndPrimitive();
    
    vec4 v0 = gl_in[0].gl_Position;
    
    for (int i = sides; i > 0; --i) {
        // Angle between each side in radians
        float ang = PI * 2.0 / float(sides) * i;

        gl_Position = v0;
        EmitVertex();

        vec4 vi = v0 + vec4(cos(ang) * 0.8, -sin(ang) * 0.8, 0.0, 0.0);
        gl_Position = vi;
        EmitVertex();

        ang = PI * 2.0 / float(sides) * (i-1);
        vec4 vii = v0 + vec4(cos(ang) * 0.8, -sin(ang) * 0.8, 0.0, 0.0);
        gl_Position = vii;
        EmitVertex();

        EndPrimitive();
    }
}