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

#include "PowerScaling/powerScaling_vs.hglsl"
#include <${MODULE_GLOBEBROWSING}/shaders/tile.glsl>
#include <${MODULE_GLOBEBROWSING}/shaders/texturetilemapping.glsl>
#include <${MODULE_GLOBEBROWSING}/shaders/tileheight.glsl>
#include <${MODULE_GLOBEBROWSING}/shaders/tilevertexskirt.glsl>

layout(location = 1) in vec2 in_uv;

out vec4 fs_position;
out vec2 fs_uv;
out vec3 ellipsoidNormalCameraSpace;
out vec3 levelWeights;
out vec3 positionCameraSpace;

#if USE_ACCURATE_NORMALS
  out vec3 ellipsoidTangentThetaCameraSpace;
  out vec3 ellipsoidTangentPhiCameraSpace;
#endif // USE_ACCURATE_NORMALS

#if USE_ECLIPSE_SHADOWS
  out vec3 positionWorldSpace;
  uniform dmat4 modelTransform;
#endif // USE_ECLIPSE_SHADOWS

#if SHADOW_MAPPING_ENABLED
  // ShadowMatrix is the matrix defined by:
  // textureCoordsMatrix * projectionMatrix * combinedViewMatrix * modelMatrix
  // where textureCoordsMatrix is just a scale and bias computation: [-1,1] to [0,1]
  uniform dmat4 shadowMatrix;
  out vec4 shadowCoords;
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

#if USE_ECLIPSE_SHADOWS
  positionWorldSpace = vec3(modelTransform * dvec4(pair.position, 1.0));
#endif // USE_ECLIPSE_SHADOWS

#if SHADOW_MAPPING_ENABLED
  shadowCoords = vec4(shadowMatrix * dvec4(pair.position, 1.0));
#endif // SHADOW_MAPPING_ENABLED
}
