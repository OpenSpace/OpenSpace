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
layout(triangle_strip, max_vertices = 8) out;

in Data {
  flat dvec4 dposWorld;
  flat vec3 rotationAxisWorld;
  flat int hasRotationAxis;
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
uniform vec4 rotationAxisColor;
uniform float observationLineLengthFactor;
uniform float rotationAxisLineLengthFactor;
uniform float observationLineWidth;
uniform float rotationAxisLineWidth;
uniform vec2 viewportSize;
uniform bool drawOriginLine;
uniform bool drawRotationAxis;

void emitThickLine(vec4 startClip, vec4 endClip, float width, vec4 color,
                   float depthClipSpace)
{
  vec4 start = z_normalization(startClip);
  vec4 end = z_normalization(endClip);
  vec2 direction = end.xy / end.w - start.xy / start.w;
  vec2 pixelDirection = direction * viewportSize;
  float projectedLength = length(pixelDirection);
  if (projectedLength <= 1e-7 || any(lessThanEqual(viewportSize, vec2(0.0)))) {
    return;
  }

  vec2 normal = normalize(vec2(-pixelDirection.y, pixelDirection.x));
  vec2 offset = normal * width / viewportSize;
  vec4 startPositive = vec4(start.xy + offset * start.w, start.zw);
  vec4 startNegative = vec4(start.xy - offset * start.w, start.zw);
  vec4 endPositive = vec4(end.xy + offset * end.w, end.zw);
  vec4 endNegative = vec4(end.xy - offset * end.w, end.zw);

  out_data.color = color;
  out_data.depthClipSpace = depthClipSpace;
  gl_Position = startPositive;
  EmitVertex();
  out_data.color = color;
  out_data.depthClipSpace = depthClipSpace;
  gl_Position = startNegative;
  EmitVertex();
  out_data.color = color;
  out_data.depthClipSpace = depthClipSpace;
  gl_Position = endPositive;
  EmitVertex();
  out_data.color = color;
  out_data.depthClipSpace = depthClipSpace;
  gl_Position = endNegative;
  EmitVertex();
  EndPrimitive();
}

void main() {
  dvec4 dposWorld = in_data[0].dposWorld;
  // Limit the max size of the points, as the angle in "FOV" that the point is allowed
  // to take up. Note that the max size is for the diameter, and we need the radius
  const float DesiredAngleRadians = radians(1.0);

  double distanceToCamera = length(dposWorld.xyz - cameraPosition);
  float currentAngle = atan(float(1.0 / distanceToCamera));

  // Calculate correction scale to achieve desired angle
  float correctionScale = DesiredAngleRadians / currentAngle;

  vec4 startClip = vec4(cameraViewProjectionMatrix * dposWorld);
  if (drawOriginLine) {
    float lineLength = correctionScale * scale * observationLineLengthFactor;
    dvec4 originDir = -lineLength * dvec4(normalize(dvec3(dposWorld)), 0.0);
    vec4 originClip = vec4(cameraViewProjectionMatrix * (dposWorld + originDir));
    emitThickLine(startClip, originClip, observationLineWidth, originLineColor,
      startClip.w);
  }

  if (drawRotationAxis && in_data[0].hasRotationAxis != 0) {
    float lineLength = correctionScale * scale * rotationAxisLineLengthFactor;
    vec3 axis = normalize(in_data[0].rotationAxisWorld);
    dvec4 axisOffset = 0.5 * lineLength * dvec4(axis, 0.0);
    vec4 axisStartClip = vec4(cameraViewProjectionMatrix * (dposWorld - axisOffset));
    vec4 axisEndClip = vec4(cameraViewProjectionMatrix * (dposWorld + axisOffset));
    emitThickLine(axisStartClip, axisEndClip, rotationAxisLineWidth,
      rotationAxisColor, startClip.w);
  }
}
