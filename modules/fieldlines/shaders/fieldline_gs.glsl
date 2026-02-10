/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include "powerscaling/powerscaling_vs.glsl"

layout(lines_adjacency) in;
in Data {
  vec4 color;
} in_data[];

layout(triangle_strip, max_vertices = 4) out;
out Data {
  vec4 color;
  vec4 position;
  vec3 normal;
} out_data;

uniform mat4 modelViewProjection;
uniform mat4 modelTransform;
uniform vec3 cameraViewDir;


// Calculate the correct powerscaled position and depth for the ABuffer
void ABufferEmitVertex(vec4 pos) {
  // calculate psc position
  vec4 tmp = pos;
  vec4 position = pscTransform(tmp, modelTransform);
  out_data.position = tmp;

  // project the position to view space
  position = modelViewProjection*position;
  gl_Position = z_normalization(position);
  EmitVertex();
}


// Original code from http://prideout.net/blog/?p=61
void main() {
  out_data.color = in_data[0].color;

  // Get the current and adjacent vertex positions and calculate help vectors u and v
  vec3 p0 = gl_in[0].gl_Position.xyz;
  vec3 p1 = gl_in[1].gl_Position.xyz;
  vec3 p2 = gl_in[2].gl_Position.xyz;
  vec3 p3 = gl_in[3].gl_Position.xyz;
  vec3 n0 = normalize(p1 - p0);
  vec3 n1 = normalize(p2 - p1);
  vec3 n2 = normalize(p3 - p2);
  vec3 u = normalize(n0 + n1);
  vec3 v = normalize(n1 + n2);

  const float EarthRadius = 6371000.0;
  float width = 0.1 * EarthRadius;

  // Calculate normals for all 4 new vertices
  vec3 normals0 = normalize(cross(cameraViewDir, u));
  vec3 normals1 = -normals0;
  vec3 normals3 = normalize(cross(cameraViewDir, v));
  vec3 normals2 = -normals3;

  // Calculate positions for the new vertices
  vec4 prismoid0 = vec4(p1 + normals0 * width, 0.0);
  vec4 prismoid1 = vec4(p1 + normals1 * width, 0.0);
  vec4 prismoid2 = vec4(p2 + normals2 * width, 0.0);
  vec4 prismoid3 = vec4(p2 + normals3 * width, 0.0);

  // Send normals and verticies to fragment shader
  out_data.normal = normals0;
  ABufferEmitVertex(prismoid0);

  out_data.normal = normals1;
  ABufferEmitVertex(prismoid1);

  out_data.normal = normals3;
  ABufferEmitVertex(prismoid3);

  out_data.normal = normals2;
  ABufferEmitVertex(prismoid2);
  EndPrimitive();
}
