/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include "PowerScaling/powerScalingMath.hglsl"

layout(points) in;
flat in vec3 normal[]; // This could be the globe normal instead, and be used as up-direction

layout(triangle_strip, max_vertices = 4) out;
out vec2 texCoord;
flat out float vs_screenSpaceDepth;
flat out vec3 vs_normal;

// General settings
uniform dmat4 modelTransform;
uniform dmat4 viewTransform;
uniform dmat4 projectionTransform;

// Camera information
// @TODO: Option to use camera position? (to work better in dome)
uniform vec3 up;
uniform vec3 right;

uniform float pointSize;

const vec2 corners[4] = vec2[4](
  vec2(0.0, 0.0),
  vec2(1.0, 0.0),
  vec2(1.0, 1.0),
  vec2(0.0, 1.0)
);

void main() {
  vec4 pos = gl_in[0].gl_Position;
  vs_normal = normal[0];
  dvec4 dpos = modelTransform * dvec4(dvec3(pos.xyz), 1.0);

  dvec4 scaledRight = pointSize * dvec4(right, 0.0) * 0.5;
  dvec4 scaledUp = pointSize * dvec4(up, 0.0) * 0.5;

  dmat4 cameraViewProjectionMatrix = projectionTransform * viewTransform;

  vec4 dposClip = vec4(cameraViewProjectionMatrix * dpos);
  vec4 scaledRightClip = vec4(cameraViewProjectionMatrix * scaledRight);
  vec4 scaledUpClip = vec4(cameraViewProjectionMatrix * scaledUp);

  // TODO: Option to put anchor point at bottom
  vec4 initialPosition = z_normalization(dposClip - scaledRightClip - scaledUpClip);
  vs_screenSpaceDepth = initialPosition.w;
  vec4 secondPosition = z_normalization(dposClip + scaledRightClip - scaledUpClip);
  vec4 crossCorner = z_normalization(dposClip + scaledUpClip + scaledRightClip);
  vec4 thirdPosition = z_normalization(dposClip + scaledUpClip - scaledRightClip);

  // Build primitive
  texCoord = corners[0];
  gl_Position = initialPosition;
  EmitVertex();

  texCoord = corners[1];
  gl_Position = secondPosition;
  EmitVertex();

  texCoord = corners[3];
  gl_Position = thirdPosition;
  EmitVertex();

  texCoord = corners[2];
  gl_Position = crossCorner;
  EmitVertex();

  EndPrimitive();
}
