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

#include "floatoperations.glsl"
#include "powerscaling/powerscalingmath.glsl"

layout(points) in;
in Data {
  vec3 color;
} in_data[];

layout(triangle_strip, max_vertices = 4) out;
out Data {
  vec4 position;
  vec2 psfCoords;
  flat vec3 color;
  flat float screenSpaceDepth;
} out_data;

uniform dvec3 eyePosition;
uniform dvec3 cameraUp;
uniform dmat4 viewProjectionMatrix;
uniform dmat4 modelMatrix;

const double Parsec = 3.08567756E16;


void main() {
  out_data.position = gl_in[0].gl_Position;
  out_data.color = in_data[0].color;

  dvec4 dpos = dvec4(out_data.position);
  dpos.xyz *= 8.0;
  dpos = modelMatrix * dpos;
  dpos /= Parsec;
  // It lies about 8 kpc from the center on the Orion Arm of the Milky Way
  dpos.x += 8000.0;

  dvec3 normal = normalize(eyePosition - dpos.xyz);
  dvec3 newRight = normalize(cross(cameraUp, normal));
  dvec3 newUp = cross(normal, newRight);
  dvec3 scaledRight = 32.0 * newRight;
  dvec3 scaledUp = 32.0 * newUp;

  vec4 bottomLeftVertex = z_normalization(
    vec4(viewProjectionMatrix * dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w))
  );

  out_data.screenSpaceDepth = bottomLeftVertex.w;

  vec4 topRightVertex = z_normalization(
    vec4(viewProjectionMatrix * dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w))
  );

  vec4 bottomRightVertex = z_normalization(
    vec4(viewProjectionMatrix * dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w))
  );
  vec4 topLeftVertex = z_normalization(
    vec4(viewProjectionMatrix * dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w))
  );

  // Build primitive
  gl_Position = topLeftVertex;
  out_data.psfCoords = vec2(-1.0, 1.0);
  EmitVertex();

  gl_Position = bottomLeftVertex;
  out_data.psfCoords = vec2(-1.0, -1.0);
  EmitVertex();

  gl_Position = topRightVertex;
  out_data.psfCoords = vec2(1.0, 1.0);
  EmitVertex();

  gl_Position = bottomRightVertex;
  out_data.psfCoords = vec2(1.0, -1.0);
  EmitVertex();

  EndPrimitive();
}
