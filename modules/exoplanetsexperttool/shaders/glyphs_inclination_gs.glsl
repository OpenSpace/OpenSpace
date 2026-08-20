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
layout(triangle_strip, max_vertices = 4) out;

in Data {
  flat float component;
  flat int glyphIndex;
  flat dvec4 dposWorld;
  flat vec3 inclinationVector;
  flat int hasInclinationFlag;
  flat vec4 color;
} in_data[];

out Data {
  flat float component;
  float depthClipSpace;
  vec4 positionViewSpace;
  flat int glyphIndex;
  vec2 texCoords;
  float sizeFactor; // The factor used for the radius of the ring
  vec4 color;
  flat int hasInclination;
} out_data;

uniform dmat4 modelMatrix;
uniform dmat4 cameraViewProjectionMatrix;
uniform float scale;
uniform bool onTop;

uniform dvec3 cameraPosition;

const vec2 Corners[4] = vec2[4](
  vec2(-1.0, -1.0),
  vec2(1.0, -1.0),
  vec2(-1.0, 1.0),
  vec2(1.0, 1.0)
);

void main() {
  out_data.component = in_data[0].component;
  out_data.glyphIndex = in_data[0].glyphIndex;
  out_data.color = in_data[0].color;
  out_data.hasInclination = in_data[0].hasInclinationFlag;

  dvec4 dpos = in_data[0].dposWorld;
  vec3 inclinationVector = normalize(in_data[0].inclinationVector);

  vec3 scaledRight = normalize(cross(inclinationVector, vec3(0.0, 1.0, 0.0)));
  vec3 scaledUp = normalize(cross(scaledRight, inclinationVector));

  // Limit the max size of the points, as the angle in "FOV" that the point is allowed
  // to take up. Note that the max size is for the diameter, and we need the radius
  const float DesiredAngleRadians = radians(0.3);

  double distanceToCamera = length(dpos.xyz - cameraPosition);
  float pointSize = length(scaledRight);
  float currentAngle = atan(float(pointSize / distanceToCamera));

  // Calculate correction scale to achieve desired angle
  float correctionScale = DesiredAngleRadians / currentAngle;

  scaledRight *= correctionScale * scale;
  scaledUp *= correctionScale * scale;

  // Apply component scaling lastly, to get comparable sizes
  float comp = in_data[0].component;

  out_data.sizeFactor = comp;

  vec4 scaledRightClip = out_data.sizeFactor *
    vec4(cameraViewProjectionMatrix * dvec4(scaledRight, 0.0));
  vec4 scaledUpClip = out_data.sizeFactor *
    vec4(cameraViewProjectionMatrix * dvec4(scaledUp, 0.0));

  dvec4 dposClip = cameraViewProjectionMatrix * dpos;

  // Lower left
  out_data.texCoords = Corners[0];
  vec4 lowerLeft = vec4(dposClip - scaledRightClip - scaledUpClip);
  gl_Position = z_normalization(lowerLeft);
  out_data.depthClipSpace = lowerLeft.w * (1 - int(onTop));
  EmitVertex();

  // Lower right
  out_data.texCoords = Corners[1];
  vec4 lowerRight = vec4(dposClip + scaledRightClip - scaledUpClip);
  gl_Position = z_normalization(lowerRight);
  out_data.depthClipSpace = lowerRight.w * (1 - int(onTop));
  EmitVertex();

  // Upper left
  out_data.texCoords = Corners[2];
  vec4 upperLeft = vec4(dposClip - scaledRightClip + scaledUpClip);
  gl_Position = z_normalization(upperLeft);
  out_data.depthClipSpace = upperLeft.w * (1 - int(onTop));
  EmitVertex();

  // Upper right
  out_data.texCoords = Corners[3];
  vec4 upperRight = vec4(dposClip + scaledRightClip + scaledUpClip);
  gl_Position = z_normalization(upperRight);
  out_data.depthClipSpace = upperRight.w * (1 - int(onTop));
  EmitVertex();

  EndPrimitive();
}
