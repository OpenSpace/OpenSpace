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

#version __CONTEXT__

uniform mat4 modelViewProjection;
uniform mat4 modelTransform;
uniform vec3 cameraViewDir;

in vec4 vs_color[];
out vec4 gs_color;
out vec4 gs_position;
out vec3 gs_normal;
out vec2 gs_texcoord;

#include "PowerScaling/powerScaling_vs.hglsl"

layout(lines_adjacency) in;
layout(triangle_strip, max_vertices = 4) out;

vec4 prismoid[4];
vec2 texcoords[4];
float EARTH_RADIUS = 6371000.0;

// Calculate the correct powerscaled position and depth for the ABuffer
void ABufferEmitVertex(vec4 pos) {
    // calculate psc position
    vec4 tmp = pos;
    vec4 position = pscTransform(tmp, modelTransform);
    gs_position = tmp;

    // project the position to view space
    position =  modelViewProjection*position;
    gl_Position = position;
    EmitVertex();
}

void emitFace(int a, int b, int c, int d) {
    gs_texcoord = texcoords[a];
    ABufferEmitVertex(prismoid[a]);
    gs_texcoord = texcoords[b];
    ABufferEmitVertex(prismoid[b]);
    gs_texcoord = texcoords[c];
    ABufferEmitVertex(prismoid[c]);
    gs_texcoord = texcoords[d];
    ABufferEmitVertex(prismoid[d]);
    EndPrimitive();
}

// Original code from http://prideout.net/blog/?p=61
void main() {
    gs_color = vs_color[0];
    vec3 p0, p1, p2, p3;
    p0 = gl_in[0].gl_Position.xyz; p1 = gl_in[1].gl_Position.xyz;
    p2 = gl_in[2].gl_Position.xyz; p3 = gl_in[3].gl_Position.xyz;
    vec3 n0 = normalize(p1-p0);
    vec3 n1 = normalize(p2-p1);
    vec3 n2 = normalize(p3-p2);
    vec3 u = normalize(n0+n1);
    vec3 v = normalize(n1+n2);

    // Declare scratch variables for basis vectors:
    float width = 0.15*EARTH_RADIUS;
    vec3 normals[2];
    normals[0] = normalize(cross(cameraViewDir,u));
    normals[1] = normalize(cross(cameraViewDir,v));

    texcoords[0] = vec2(1,1);
    texcoords[1] = vec2(1,0);
    texcoords[2] = vec2(0,0);
    texcoords[3] = vec2(0,1);

    prismoid[0] = vec4(p1 + normals[0]*width, 0);
    prismoid[1] = vec4(p1 - normals[0]*width, 0);
    prismoid[2] = vec4(p2 - normals[1]*width, 0);
    prismoid[3] = vec4(p2 + normals[1]*width, 0);

    gs_normal = n1;
    gs_texcoord = texcoords[0];
    ABufferEmitVertex(prismoid[0]);

    gs_texcoord = texcoords[1];
    ABufferEmitVertex(prismoid[1]);

    gs_normal = n2;
    gs_texcoord = texcoords[3];
    ABufferEmitVertex(prismoid[3]);

    gs_texcoord = texcoords[2];
    ABufferEmitVertex(prismoid[2]);
    EndPrimitive();

    // vec3 i,j,k; float r = 0.05*EARTH_RADIUS;
    // j = u; i = normal; k = cross(i, j); i *= r; k *= r;
    
    // // Compute face 1 of 2:
    // prismoid[0] = vec4(p1 + i + k, 0);
    // prismoid[1] = vec4(p1 + i - k, 0);
    // prismoid[2] = vec4(p1 - i - k, 0);
    // prismoid[3] = vec4(p1 - i + k, 0);

    // // Compute face 2 of 2:
    // prismoid[4] = vec4(p2 + i + k, 0);
    // prismoid[5] = vec4(p2 + i - k, 0);
    // prismoid[6] = vec4(p2 - i - k, 0);
    // prismoid[7] = vec4(p2 - i + k, 0);    

    // // Emit the six faces of the prismoid:
    // emitFace(0,1,3,2); emitFace(5,4,6,7);
    // emitFace(4,5,0,1); emitFace(3,2,7,6);
    // emitFace(0,3,4,7); emitFace(2,1,6,5);
}