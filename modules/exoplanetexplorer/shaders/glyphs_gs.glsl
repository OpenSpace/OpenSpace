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
layout(triangle_strip, max_vertices = 26) out;

const int MaxColors = 4;

// Rings rendering
const int RingSegments = 12;
const float Pi = 3.141592657;
const float MinAnnularRadiusPixels = 160.0;
const float MinAnnularWidthPixels = 2.0;
const float GeometryTolerance = 1e-5;

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
  flat float ringWidth;
} out_data;

uniform dmat4 modelMatrix;
uniform dmat4 cameraViewProjectionMatrix;
uniform float scale;
uniform bool onTop;
uniform bool useFixedRingWidth;
uniform bool isRenderIndexStep = false;
uniform vec2 viewportSize;

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

void emitGlyphVertex(vec2 texCoords, vec4 positionClip, float ringWidth, float depth) {
  out_data.component = in_data[0].component;
  out_data.colors = in_data[0].colors;
  out_data.nColors = in_data[0].nColors;
  out_data.glyphIndex = in_data[0].glyphIndex;
  out_data.depthClipSpace = depth;
  out_data.texCoords = texCoords;
  out_data.ringWidth = ringWidth;
  gl_Position = z_normalization(positionClip);
  EmitVertex();
}

void main() {
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

  float sizeFactor = comp;
  float ringWidth = 0.95 * 1.0 / comp;
  if (!useFixedRingWidth) {
    float componentPower = pow(0.87, comp);
    double ringSpacing = double(sqrt(componentPower));
    double accumulatedSizeFactor = 1.0;
    for (int i = 1; i < int(comp); i++) {
      accumulatedSizeFactor += ringSpacing;
    }
    sizeFactor = float(accumulatedSizeFactor);
    ringWidth = componentPower / sizeFactor;
  }

  vec4 scaledRightClip = sizeFactor *
    vec4(cameraViewProjectionMatrix * dvec4(scaledRight, 0.0));
  vec4 scaledUpClip = sizeFactor *
    vec4(cameraViewProjectionMatrix * dvec4(scaledUp, 0.0));

  dvec4 dposClip = cameraViewProjectionMatrix * dpos;
  vec4 lowerLeft = vec4(dposClip - scaledRightClip - scaledUpClip);
  float depth = lowerLeft.w * (1 - int(onTop));

  float innerRadius = 1.0 - ringWidth;
  float outerRadius = 1.0 / cos(Pi / float(RingSegments)) + GeometryTolerance;
  double minimumW = dposClip.w - double(outerRadius) *
    (abs(double(scaledRightClip.w)) + abs(double(scaledUpClip.w)));

  vec2 centerNdc = vec2(dposClip.xy / dposClip.w);
  vec2 rightPixels = 0.5 * viewportSize *
    (scaledRightClip.xy - centerNdc * scaledRightClip.w) / float(dposClip.w);
  vec2 upPixels = 0.5 * viewportSize *
    (scaledUpClip.xy - centerNdc * scaledUpClip.w) / float(dposClip.w);
  float projectedRadius = min(length(rightPixels), length(upPixels));

  // Use annular geometry for large, narrow rings safely in front of the camera.
  // Keep the full quad for picking, selection overlays, and smaller or wider rings.
  // The annular rendering prevent rasterizing uneccessary fragments, like the empty
  // space inside the ring
  if (!onTop && !isRenderIndexStep && innerRadius > 0.5 && minimumW > 0.0 &&
      projectedRadius >= MinAnnularRadiusPixels &&
      projectedRadius * ringWidth >= MinAnnularWidthPixels)
  {
    innerRadius = max(0.0, innerRadius - GeometryTolerance);

    for (int segment = 0; segment <= RingSegments; segment++) {
      // Repeat the first vertex to close the strip without overlapping triangles
      float angle = 2.0 * Pi * float(segment % RingSegments) / float(RingSegments);
      vec2 direction = vec2(cos(angle), sin(angle));
      vec2 innerCoords = innerRadius * direction;
      vec2 outerCoords = outerRadius * direction;

      emitGlyphVertex(
        innerCoords,
        vec4(dposClip + double(innerCoords.x) * dvec4(scaledRightClip) +
          double(innerCoords.y) * dvec4(scaledUpClip)),
        ringWidth,
        depth
      );

      emitGlyphVertex(
        outerCoords,
        vec4(dposClip + double(outerCoords.x) * dvec4(scaledRightClip) +
          double(outerCoords.y) * dvec4(scaledUpClip)),
        ringWidth,
        depth
      );
    }
    EndPrimitive();
    return;
  }

  // Quad rendering

  // Lower left
  emitGlyphVertex(Corners[0], lowerLeft, ringWidth, depth);

  // Lower right
  emitGlyphVertex(
    Corners[1],
    vec4(dposClip + scaledRightClip - scaledUpClip),
    ringWidth,
    depth
  );

  // Upper left
  emitGlyphVertex(
    Corners[2],
    vec4(dposClip + scaledUpClip - scaledRightClip),
    ringWidth,
    depth
  );

  // Upper right
  emitGlyphVertex(
    Corners[3],
    vec4(dposClip + scaledUpClip + scaledRightClip),
    ringWidth,
    depth
  );

  EndPrimitive();
}
