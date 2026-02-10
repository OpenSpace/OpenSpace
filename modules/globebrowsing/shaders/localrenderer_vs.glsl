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
#include "tile.glsl"
#include "texturetilemapping.glsl"
#include "tileheight.glsl"
#include "tilevertexskirt.glsl"

#define nDepthMaps #{nDepthMaps}

layout(location = 1) in vec2 in_uv;

out Data {
  vec4 position;
  vec3 ellipsoidNormalCameraSpace;
  vec3 levelWeights;
  vec3 positionCameraSpace;
  vec3 posObjSpace;
  vec3 normalObjSpace;
  vec2 uv;
#if USE_ACCURATE_NORMALS
  vec3 ellipsoidTangentThetaCameraSpace;
  vec3 ellipsoidTangentPhiCameraSpace;
#endif // USE_ACCURATE_NORMALS
#if USE_ECLIPSE_SHADOWS
  vec3 positionWorldSpace;
#endif // USE_ECLIPSE_SHADOWS
#if SHADOW_MAPPING_ENABLED
  vec4 shadowCoords;
#endif // SHADOW_MAPPING_ENABLED
#if nDepthMaps > 0
  vec4 positionsLightspace[nDepthMaps];
#endif // nDepthMaps > 0
} out_data;

uniform dmat4 inverseViewTransform;
uniform dmat4 modelTransform;

#if SHADOW_MAPPING_ENABLED
  uniform dmat4 shadowMatrix;
#endif // SHADOW_MAPPING_ENABLED

uniform mat4 projectionTransform;
// Input points in camera space
uniform vec3 p00;
uniform vec3 p10;
uniform vec3 p01;
uniform vec3 p11;
uniform vec3 patchNormalCameraSpace;
uniform float chunkMinHeight;
uniform float distanceScaleFactor;
uniform int chunkLevel;

#if nDepthMaps > 0
  uniform dmat4 inv_vp;
  uniform dmat4 light_vps[nDepthMaps];
#endif // nDepthMaps > 0


vec3 bilinearInterpolation(vec2 uv) {
  vec3 p0 = mix(p00, p10, uv.x);
  vec3 p1 = mix(p01, p11, uv.x);
  return mix(p0, p1, uv.y);
}


void main() {
  // Position in cameraspace
  vec3 p = bilinearInterpolation(in_uv);

  // Calculate desired level based on distance to the vertex on the ellipsoid
  // Before any heightmapping is done
  float distToVertexOnEllipsoid = length(p + patchNormalCameraSpace * chunkMinHeight);

  // use level weight for height sampling, and output to fragment shader
  float projectedScaleFactor = distanceScaleFactor / distToVertexOnEllipsoid;
  float desiredLevel = log2(projectedScaleFactor);
  float levelInterp = chunkLevel - desiredLevel;
  out_data.levelWeights = vec3(
    clamp(1.0 - levelInterp, 0.0, 1.0),
    clamp(levelInterp, 0.0, 1.0) - clamp(levelInterp - 1.0, 0.0, 1.0),
    clamp(levelInterp - 1.0, 0.0, 1.0)
  );

  // Get the height value and apply skirts
  float height = tileHeightScaled(
    in_uv,
    out_data.levelWeights
  ) - tileVertexSkirtLength() * 100.0;

  // Translate the point along normal
  p += patchNormalCameraSpace * height;

#if USE_ACCURATE_NORMALS
  // Calculate tangents
  out_data.ellipsoidTangentThetaCameraSpace = normalize(p10 - p00);
  out_data.ellipsoidTangentPhiCameraSpace = normalize(p01 - p00);
#endif // USE_ACCURATE_NORMALS

  // Write output
  out_data.uv = in_uv;
  out_data.position = z_normalization(projectionTransform * vec4(p, 1.0));
  gl_Position = out_data.position;
  out_data.ellipsoidNormalCameraSpace = patchNormalCameraSpace;
  out_data.positionCameraSpace = p;
  out_data.posObjSpace = vec3(inverseViewTransform * dvec4(p, 1.0));

#if USE_ECLIPSE_SHADOWS
  out_data.positionWorldSpace = vec3(modelTransform * dvec4(p, 1.0));
#endif // USE_ECLIPSE_SHADOWS

#if SHADOW_MAPPING_ENABLED
  out_data.shadowCoords = vec4(shadowMatrix * dvec4(p, 1.0));
#endif // SHADOW_MAPPING_ENABLED

#if nDepthMaps > 0
  for (int idx = 0; idx < nDepthMaps; idx++) {
    out_data.positions_lightspace[idx] = vec4(light_vps[idx] * (inv_vp * dvec4(p, 1.0)));
  }
#endif // nDepthMaps > 0
}
