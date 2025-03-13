/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
layout(triangle_strip, max_vertices = 63) out;

uniform int sides;

const float PI = 3.1415926;


void main() {
  vec4 v0 = gl_in[0].gl_Position;

  for (int i = sides; i > 0; --i) {
    // Angle between each side in radians
    float ang = 2.0 * PI / float(sides) * i;

    gl_Position = v0;
    EmitVertex();

    vec4 vi = v0 + vec4(cos(ang) * 0.8, -sin(ang) * 0.8, 0.0, 0.0);
    gl_Position = vi;
    EmitVertex();

    ang = 2.0 * PI / float(sides) * (i - 1);
    vec4 vii = v0 + vec4(cos(ang) * 0.8, -sin(ang) * 0.8, 0.0, 0.0);
    gl_Position = vii;
    EmitVertex();

    EndPrimitive();
  }
}
