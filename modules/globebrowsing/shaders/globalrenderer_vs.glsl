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

layout(location = 1) in vec2 in_uv;

out vec4 fs_position;
out vec2 fs_uv;
out vec3 ellipsoidNormalCameraSpace;
out vec3 levelWeights;
out vec3 positionCameraSpace;
out vec3 posObjSpace;
out vec3 normalObjSpace;

#if USE_ACCURATE_NORMALS
  out vec3 ellipsoidTangentThetaCameraSpace;
  out vec3 ellipsoidTangentPhiCameraSpace;
#endif // USE_ACCURATE_NORMALS

uniform dmat4 modelTransform;

#if USE_ECLIPSE_SHADOWS
  out vec3 positionWorldSpace;
#endif // USE_ECLIPSE_SHADOWS

#if SHADOW_MAPPING_ENABLED
  out vec4 shadowCoords;
  uniform dmat4 shadowMatrix;
#endif // SHADOW_MAPPING_ENABLED

uniform mat4 modelViewProjectionTransform;
uniform mat4 modelViewTransform;
uniform vec3 radiiSquared;

uniform vec2 minLatLon;
uniform vec2 lonLatScalingFactor;
uniform vec3 cameraPosition;
uniform float chunkMinHeight;

uniform float distanceScaleFactor;
uniform int chunkLevel;

#define nDepthMaps #{nDepthMaps}
#if nDepthMaps > 0
  uniform dmat4 inv_vp;
  uniform dmat4 light_vps[nDepthMaps];
  uniform sampler2D light_depth_maps[nDepthMaps];
  out vec4 positions_lightspace[nDepthMaps];
#endif // nDepthMaps > 0

struct PositionNormalPair {
  vec3 position;
  vec3 normal;
};

PositionNormalPair globalInterpolation(vec2 uv) {
  vec2 lonlat = lonLatScalingFactor * uv + minLatLon;

  // geodetic surface normal
  float cosLat = cos(lonlat.y);
  vec3 normal = vec3(cosLat * cos(lonlat.x), cosLat * sin(lonlat.x), sin(lonlat.y));
  vec3 k = radiiSquared * normal;
  float gamma = sqrt(dot(k, normal));

  PositionNormalPair result;
  result.position = k / gamma;
  result.normal = normal;
  return result;
}

vec3 getLevelWeights(float distToVertexOnEllipsoid) {
  float projectedScaleFactor = distanceScaleFactor / distToVertexOnEllipsoid;
  float desiredLevel = log2(projectedScaleFactor);
  float levelInterp = chunkLevel - desiredLevel;

  return vec3(
    clamp(1.0 - levelInterp, 0.0, 1.0),
    clamp(levelInterp, 0.0, 1.0) - clamp(levelInterp - 1.0, 0.0, 1.0),
    clamp(levelInterp - 1.0, 0.0, 1.0)
  );
}


void main() {
  PositionNormalPair pair = globalInterpolation(in_uv);
  float distToVertexOnEllipsoid = length((pair.normal * chunkMinHeight + pair.position) - cameraPosition);

  // use level weight for height sampling, and output to fragment shader
  levelWeights = getLevelWeights(distToVertexOnEllipsoid);

  // Get the height value and apply skirts
  float height = getTileHeight(in_uv, levelWeights) - getTileVertexSkirtLength();

#if USE_ACCURATE_NORMALS
  // Calculate tangents
  // tileDelta is a step length (epsilon). Should be small enough for accuracy but not
  // Too small for precision. 1 / 512 is good.
  const float tileDelta = 1.0 / 512.0;
  PositionNormalPair pair10 = globalInterpolation(vec2(1.0, 0.0) * tileDelta + in_uv);
  PositionNormalPair pair01 = globalInterpolation(vec2(0.0, 1.0) * tileDelta + in_uv);
  vec3 ellipsoidTangentTheta = normalize(pair10.position - pair.position);
  vec3 ellipsoidTangentPhi = normalize(pair01.position - pair.position);
  ellipsoidTangentThetaCameraSpace = mat3(modelViewTransform) * ellipsoidTangentTheta;
  ellipsoidTangentPhiCameraSpace = mat3(modelViewTransform) * ellipsoidTangentPhi;
#endif // USE_ACCURATE_NORMALS

  // Add the height in the direction of the normal
  pair.position = pair.normal * height + pair.position;
  vec4 positionClippingSpace = modelViewProjectionTransform * vec4(pair.position, 1.0);

  // Write output
  fs_uv = in_uv;
  fs_position = z_normalization(positionClippingSpace);
  gl_Position = fs_position;
  ellipsoidNormalCameraSpace = mat3(modelViewTransform) * pair.normal;
  positionCameraSpace = vec3(modelViewTransform * vec4(pair.position, 1.0));
  posObjSpace = pair.position;
  normalObjSpace = pair.normal;

#if USE_ECLIPSE_SHADOWS
  positionWorldSpace = vec3(modelTransform * dvec4(pair.position, 1.0));
#endif // USE_ECLIPSE_SHADOWS

#if SHADOW_MAPPING_ENABLED
  shadowCoords = vec4(shadowMatrix * dvec4(pair.position, 1.0));
#endif // SHADOW_MAPPING_ENABLED

#if nDepthMaps > 0
  for (int idx = 0; idx < nDepthMaps; idx++) {
    positions_lightspace[idx] = vec4(light_vps[idx] * (inv_vp * dvec4(positionCameraSpace, 1.0)));
  }
#endif // nDepthMaps > 0
}
