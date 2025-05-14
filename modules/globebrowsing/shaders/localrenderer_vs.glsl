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

out vec2 fs_uv;
out vec4 fs_position;
out vec3 ellipsoidNormalCameraSpace;
out vec3 levelWeights;
out vec3 positionCameraSpace;

#if USE_ACCURATE_NORMALS
  out vec3 ellipsoidTangentThetaCameraSpace;
  out vec3 ellipsoidTangentPhiCameraSpace;
#endif // USE_ACCURATE_NORMALS

#if USE_ECLIPSE_SHADOWS
out vec3 positionWorldSpace;
uniform dmat4 inverseViewTransform;
#endif // USE_ECLIPSE_SHADOWS

#if SHADOW_MAPPING_ENABLED
  // ShadowMatrix is the matrix defined by:
  // textureCoordsMatrix * projectionMatrix * combinedViewMatrix * modelMatrix
  // where textureCoordsMatrix is just a scale and bias computation: [-1,1] to [0,1]
  uniform dmat4 shadowMatrix;
  out vec4 shadowCoords;
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


vec3 bilinearInterpolation(vec2 uv) {
  vec3 p0 = mix(p00, p10, uv.x);
  vec3 p1 = mix(p01, p11, uv.x);
  return mix(p0, p1, uv.y);
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
  // Position in cameraspace
  vec3 p = bilinearInterpolation(in_uv);

  // Calculate desired level based on distance to the vertex on the ellipsoid
  // Before any heightmapping is done
  float distToVertexOnEllipsoid = length(p + patchNormalCameraSpace * chunkMinHeight);

  // use level weight for height sampling, and output to fragment shader
  levelWeights = getLevelWeights(distToVertexOnEllipsoid);

  // Get the height value and apply skirts
  float height = getTileHeightScaled(in_uv, levelWeights) - getTileVertexSkirtLength() * 100.0;

  // Translate the point along normal
  p += patchNormalCameraSpace * height;

  vec4 positionClippingSpace = projectionTransform * vec4(p, 1);

#if USE_ACCURATE_NORMALS
  // Calculate tangents
  ellipsoidTangentThetaCameraSpace = normalize(p10 - p00);
  ellipsoidTangentPhiCameraSpace = normalize(p01 - p00);
#endif // USE_ACCURATE_NORMALS

  // Write output
  fs_uv = in_uv;
  fs_position = z_normalization(positionClippingSpace);
  gl_Position = fs_position;
  ellipsoidNormalCameraSpace = patchNormalCameraSpace;
  positionCameraSpace = p;

#if USE_ECLIPSE_SHADOWS
  positionWorldSpace = vec3(inverseViewTransform * dvec4(p, 1.0));
#endif // USE_ECLIPSE_SHADOWS

#if SHADOW_MAPPING_ENABLED
  shadowCoords = vec4(shadowMatrix * dvec4(p, 1.0));
#endif // SHADOW_MAPPING_ENABLED
}
