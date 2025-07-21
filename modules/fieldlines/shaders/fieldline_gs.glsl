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

#include "PowerScaling/powerScaling_vs.hglsl"

layout(lines_adjacency) in;
in vec4 vs_color[];

layout(triangle_strip, max_vertices = 4) out;
out vec4 gs_color;
out vec4 gs_position;
out vec3 gs_normal;

uniform mat4 modelViewProjection;
uniform mat4 modelTransform;
uniform vec3 cameraViewDir;



// Calculate the correct powerscaled position and depth for the ABuffer
void ABufferEmitVertex(vec4 pos) {
  // calculate psc position
  vec4 tmp = pos;
  vec4 position = pscTransform(tmp, modelTransform);
  gs_position = tmp;

  // project the position to view space
  position =  modelViewProjection*position;
  gl_Position = z_normalization(position);
  EmitVertex();
}


// Original code from http://prideout.net/blog/?p=61
void main() {
  gs_color = vs_color[0];

  // Get the current and adjacent vertex positions and calculate help vectors u and v
  vec3 p0, p1, p2, p3;
  p0 = gl_in[0].gl_Position.xyz; p1 = gl_in[1].gl_Position.xyz;
  p2 = gl_in[2].gl_Position.xyz; p3 = gl_in[3].gl_Position.xyz;
  vec3 n0 = normalize(p1 - p0);
  vec3 n1 = normalize(p2 - p1);
  vec3 n2 = normalize(p3 - p2);
  vec3 u = normalize(n0 + n1);
  vec3 v = normalize(n1 + n2);

  float EARTH_RADIUS = 6371000.0;
  float width = 0.1 * EARTH_RADIUS;

  // Calculate normals for all 4 new vertices
  vec3 normals[4];
  normals[0] = normalize(cross(cameraViewDir,u));
  normals[1] = -normals[0];
  normals[3] = normalize(cross(cameraViewDir,v));
  normals[2] = -normals[3];

  // Calculate positions for the new vertices
  vec4 prismoid[4];
  prismoid[0] = vec4(p1 + normals[0] * width, 0.0);
  prismoid[1] = vec4(p1 + normals[1] * width, 0.0);
  prismoid[2] = vec4(p2 + normals[2] * width, 0.0);
  prismoid[3] = vec4(p2 + normals[3] * width, 0.0);

  // Send normals and verticies to fragment shader
  gs_normal = normals[0];
  ABufferEmitVertex(prismoid[0]);

  gs_normal = normals[1];
  ABufferEmitVertex(prismoid[1]);

  gs_normal = normals[3];
  ABufferEmitVertex(prismoid[3]);

  gs_normal = normals[2];
  ABufferEmitVertex(prismoid[2]);
  EndPrimitive();
}
