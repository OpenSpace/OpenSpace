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

#include "powerscaling/powerscalingmath.glsl"

layout(points) in;
layout(line_strip, max_vertices = 4) out;

in Data {
  flat dvec4 dposWorld;
  flat vec3 upWorld;
} in_data[];

out Data {
  vec4 color;
  float depthClipSpace;
} out_data;

uniform dmat4 modelMatrix;
uniform dmat4 cameraViewProjectionMatrix;
uniform float scale;
uniform dvec3 cameraPosition;
uniform vec4 originLineColor;
uniform float lineLengthFactor;

const float M_PI = 3.14159265359;

void main() {
  dvec4 dposWorld = in_data[0].dposWorld;
  vec3 upWorld = normalize(in_data[0].upWorld);

  // Limit the max size of the points, as the angle in "FOV" that the point is allowed
  // to take up. Note that the max size is for the diameter, and we need the radius
  const float DesiredAngleRadians = radians(1.0);

  double distanceToCamera = length(dposWorld.xyz - cameraPosition);
  float currentAngle = atan(float(1.0 / distanceToCamera));

  // Calculate correction scale to achieve desired angle
  float correctionScale = DesiredAngleRadians / currentAngle;

  float lineLength = correctionScale * scale * lineLengthFactor;

  dvec4 originDir = - lineLength * dvec4(normalize(dvec3(dposWorld)), 0.0);
  dvec4 upDir = lineLength * dvec4(upWorld, 0.0);

  // Line 1: From current position to origin
  // Start at current position
  vec4 startClip = vec4(cameraViewProjectionMatrix * dposWorld);
  out_data.color = originLineColor;
  out_data.depthClipSpace = startClip.w;
  gl_Position = z_normalization(startClip);
  EmitVertex();

  // End at origin
  dvec4 endPos = dposWorld + originDir;
  vec4 originClip = vec4(cameraViewProjectionMatrix * endPos);
  out_data.color = originLineColor;
  out_data.depthClipSpace = startClip.w;
  gl_Position = z_normalization(vec4(originClip));
  EmitVertex();

  EndPrimitive();

  // @TODO: Leaving the up-direction for now. There is no data for this, so all the
  // stars will have the same up as of now

//  // Line 2: From current position in up direction
//  vec4 upLineColor = vec4(1.0, 1.0, 1.0, 1.0);
//
//  // Start at current position
//  out_data.color = upLineColor;
//  out_data.depthClipSpace = startClip.w;
//  gl_Position = z_normalization(vec4(startClip));
//  EmitVertex();
//
//  // End at current position + upWorld * scale
//  dvec4 upPos = dposWorld + upDir;
//  vec4 upClip = vec4(cameraViewProjectionMatrix * upPos);
//  out_data.color = upLineColor;
//  out_data.depthClipSpace = upClip.w;
//  gl_Position = z_normalization(upClip);
//  EmitVertex();

  EndPrimitive();
}
