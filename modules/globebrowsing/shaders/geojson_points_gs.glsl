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

#include "PowerScaling/powerScalingMath.hglsl"

layout(points) in;
flat in vec3 normal[]; // Point normals correspond to globe out direction, model space
flat in float dynamicHeight[];

layout(triangle_strip, max_vertices = 4) out;
out vec2 texCoord;
flat out float vs_screenSpaceDepth;
out vec4 vs_positionViewSpace;
flat out vec3 vs_normal;

// General settings
uniform dmat4 modelTransform;
uniform dmat4 viewTransform;
uniform dmat4 projectionTransform;

uniform float heightOffset;
uniform bool useHeightMapData;

// Camera information
uniform vec3 cameraUp;
uniform vec3 cameraRight;
uniform dvec3 cameraPosition; // world coordinates
uniform vec3 cameraLookUp;

// Render mode
uniform int renderMode;
// OBS! Keep in sync with option property options
const int RenderOptionCameraDir = 0;
const int RenderOptionCameraPos = 1;
const int RenderOptionGlobeNormal = 2;
const int RenderOptionGlobeSurface = 3;

uniform float pointSize;
uniform float textureWidthFactor;

// If false, use the center
uniform bool useBottomAnchorPoint = true;

const vec2 corners[4] = vec2[4](
  vec2(0.0, 0.0),
  vec2(1.0, 0.0),
  vec2(1.0, 1.0),
  vec2(0.0, 1.0)
);

void main() {
  vec4 pos = gl_in[0].gl_Position;
  vs_normal = normal[0];
  dvec4 dpos = dvec4(dvec3(pos.xyz), 1.0);

  // Offset position based on height information
  if (length(pos.xyz) > 0) {
      dvec3 outDirection = normalize(dvec3(dpos));
      float height = heightOffset;
      if (useHeightMapData) {
        height += dynamicHeight[0];
      }
      dpos += dvec4(outDirection * double(height), 0.0);
  }
  // World coordinates
  dpos = modelTransform * dpos;
  vec3 worldNormal = normalize(mat3(modelTransform) * vs_normal);

  // Set up and right directions based on render mode.
  // renderMode 0 is default
  vec3 right = cameraRight;
  vec3 up = cameraUp;
  vec3 cameraToPosDir = vec3(normalize(cameraPosition - dpos.xyz));

  // Update right and up based on render mode
  if (renderMode == RenderOptionCameraPos) {
    right = normalize(cross(cameraLookUp, cameraToPosDir));
    up = normalize(cross(cameraToPosDir, right));
  }
  else if (renderMode == RenderOptionGlobeNormal) {
    up = worldNormal;
    right = normalize(cross(up, cameraToPosDir));
  }
  else if (renderMode == RenderOptionGlobeSurface) {
    // Compute up to be orthogonal to globe normal and camera right direction
    up = normalize(cross(worldNormal, right));
    // Recompute right to be orthognal to globe normal
    right = cross(up, worldNormal);
  }

  dvec4 scaledRight = pointSize * dvec4(right, 0.0) * 0.5;
  dvec4 scaledUp = pointSize * dvec4(up, 0.0) * 0.5;

  dmat4 cameraViewProjectionMatrix = projectionTransform * viewTransform;

  vec4 dposClip = vec4(cameraViewProjectionMatrix * dpos);
  vec4 scaledRightClip = textureWidthFactor * vec4(cameraViewProjectionMatrix * scaledRight);
  vec4 scaledUpClip = vec4(cameraViewProjectionMatrix * scaledUp);

  // Place anchor point at the bottom
  vec4 bottomLeft = z_normalization(dposClip - scaledRightClip);
  vec4 bottomRight = z_normalization(dposClip + scaledRightClip);
  vec4 topRight = z_normalization(dposClip + 2 * scaledUpClip + scaledRightClip);
  vec4 topLeft = z_normalization(dposClip + 2 * scaledUpClip - scaledRightClip);

  if (!useBottomAnchorPoint) {
    // Place anchor point at the center
    bottomLeft = z_normalization(dposClip - scaledRightClip - scaledUpClip);
    bottomRight = z_normalization(dposClip + scaledRightClip - scaledUpClip);
    topRight = z_normalization(dposClip + scaledUpClip + scaledRightClip);
    topLeft = z_normalization(dposClip + scaledUpClip - scaledRightClip);
  }

  vs_screenSpaceDepth = bottomLeft.w;
  vs_positionViewSpace = vec4(viewTransform * dpos);

  // Build primitive
  texCoord = corners[0];
  gl_Position = bottomLeft;
  EmitVertex();

  texCoord = corners[1];
  gl_Position = bottomRight;
  EmitVertex();

  texCoord = corners[3];
  gl_Position = topLeft;
  EmitVertex();

  texCoord = corners[2];
  gl_Position = topRight;
  EmitVertex();

  EndPrimitive();
}
