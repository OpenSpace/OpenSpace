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

const int MaxColors = 8;

in Data {
  flat float component;
  flat int nColors;
  flat vec4 colors[MaxColors];
  flat int glyphIndex;
  flat dvec4 dposWorld;
} in_data[];

out Data {
  flat float component;
  float depthClipSpace;
  vec4 positionViewSpace;
  flat int glyphIndex;
  flat int nColors;
  flat vec4 colors[MaxColors];
  vec2 texCoords;
  float sizeFactor; // The factor used for the radius of the ring
} out_data;

uniform dmat4 modelMatrix;
uniform dmat4 cameraViewProjectionMatrix;
uniform float scale;
uniform bool onTop;
uniform bool useFixedRingWidth;

uniform int renderOption;

// RenderOption: CameraViewDirection
uniform vec3 up;
uniform vec3 right;

// RenderOption: CameraPositionNormal
uniform dvec3 cameraPosition;
uniform vec3 cameraLookUp;

const int RenderOptionCameraViewDirection = 0;
const int RenderOptionCameraPositionNormal = 1;

const vec2 Corners[4] = vec2[4](
  vec2(-1.0, -1.0),
  vec2(1.0, -1.0),
  vec2(-1.0, 1.0),
  vec2(1.0, 1.0)
);

void main() {
  out_data.component = in_data[0].component;
  out_data.colors = in_data[0].colors;
  out_data.nColors = in_data[0].nColors;
  out_data.glyphIndex = in_data[0].glyphIndex;

  dvec4 dpos = in_data[0].dposWorld;

  vec3 scaledRight = vec3(1.0, 0.0, 0.0);
  vec3 scaledUp = vec3(0.0, 1.0, 0.0);

  if (renderOption == RenderOptionCameraViewDirection) {
    scaledRight = right;
    scaledUp = up;
  }
  else if (renderOption == RenderOptionCameraPositionNormal) {
    vec3 normal = vec3(normalize(cameraPosition - dpos.xyz));
    vec3 newRight = normalize(cross(cameraLookUp, normal));
    vec3 newUp = cross(normal, newRight);

    scaledRight = newRight;
    scaledUp = newUp;
  }

  // Limit the max size of the points, as the angle in "FOV" that the point is allowed
  // to take up. Note that the max size is for the diameter, and we need the radius
  const float DesiredAngleRadians = radians(0.4);

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
  if (!useFixedRingWidth) {
    // Same area:
//    sizeFactor = sqrt(2.0 * comp);

    // Ish 90% width of previous ring
    double sizeFactor = 1.0;
    for (int i = 1; i < int(comp); i++) {
      // This computation is not completely logical.
      // But it makes the result look ok. based on
      // trying to make each ring about 90% as wide
      // as the previous. The sqrt spaces them out quite nicely
      sizeFactor += double(sqrt(pow(0.87, comp)));
    }
    out_data.sizeFactor = float(sizeFactor);
  }

  vec4 scaledRightClip = out_data.sizeFactor *
    vec4(cameraViewProjectionMatrix * dvec4(scaledRight, 0.0));
  vec4 scaledUpClip = out_data.sizeFactor *
    vec4(cameraViewProjectionMatrix * dvec4(scaledUp, 0.0));

  dvec4 dposClip = cameraViewProjectionMatrix * dpos;
  vec4 lowerLeft = vec4(dposClip - scaledRightClip - scaledUpClip);
  out_data.depthClipSpace = lowerLeft.w * (1 - int(onTop));

  // Lower left
  out_data.texCoords = Corners[0];
  gl_Position = z_normalization(lowerLeft);
  EmitVertex();

  // Lower right
  out_data.texCoords = Corners[1];
  gl_Position = z_normalization(vec4(dposClip + scaledRightClip - scaledUpClip));
  EmitVertex();

  // Upper left
  out_data.texCoords = Corners[2];
  gl_Position = z_normalization(vec4(dposClip + scaledUpClip - scaledRightClip));
  EmitVertex();

  // Upper right
  out_data.texCoords = Corners[3];
  gl_Position = z_normalization(vec4(dposClip + scaledUpClip + scaledRightClip));
  EmitVertex();

  EndPrimitive();
}
