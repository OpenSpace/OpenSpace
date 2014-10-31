/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

// Based on http://prideout.net/blog/?p=64

layout(points) in;
layout(triangle_strip, max_vertices = 24) out;
uniform mat4 modelViewProjection;
in vec4 vPosition[1];

vec4 objCube[8]; // Object space coordinate of cube corner
vec4 ndcCube[8]; // Normalized device coordinate of cube corner
ivec4 faces[6];  // Vertex indices of the cube faces

void emit_vert(int vert) {
    gl_Position = ndcCube[vert];
    EmitVertex();
}

void emit_face(int face) {
    emit_vert(faces[face][1]); emit_vert(faces[face][0]);
    emit_vert(faces[face][3]); emit_vert(faces[face][2]);
    EndPrimitive();
}

void main() {
    faces[0] = ivec4(0,1,3,2); faces[1] = ivec4(5,4,6,7);
    faces[2] = ivec4(4,5,0,1); faces[3] = ivec4(3,2,7,6);
    faces[4] = ivec4(0,3,4,7); faces[5] = ivec4(2,1,6,5);

    float size = 0.5;
    vec4 P = vPosition[0];
    vec4 I = vec4(size, 0,      0,      0);
    vec4 J = vec4(0,    size,   0,      0);
    vec4 K = vec4(0,    0,      size,   0);

    objCube[0] = P+K+I+J; objCube[1] = P+K+I-J;
    objCube[2] = P+K-I-J; objCube[3] = P+K-I+J;
    objCube[4] = P-K+I+J; objCube[5] = P-K+I-J;
    objCube[6] = P-K-I-J; objCube[7] = P-K-I+J;

    // Transform the corners of the box:
    for (int vert = 0; vert < 8; vert++)
        ndcCube[vert] = modelViewProjection * objCube[vert];

    // Emit the six faces:
    for (int face = 0; face < 6; face++)
        emit_face(face);
}
