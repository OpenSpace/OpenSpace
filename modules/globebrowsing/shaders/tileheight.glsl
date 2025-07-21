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

#ifndef TILE_HEIGHT_HGLSL
#define TILE_HEIGHT_HGLSL

#include "PowerScaling/powerScaling_vs.hglsl"
#include <${MODULE_GLOBEBROWSING}/shaders/tile.glsl>

#ifndef USE_HEIGHTMAP
#define USE_HEIGHTMAP #{useAccurateNormals}
#endif // USE_HEIGHTMAP

#ifndef USE_ACCURATE_NORMALS
#define USE_ACCURATE_NORMALS #{useAccurateNormals}
#endif // USE_ACCURATE_NORMALS

#if USE_HEIGHTMAP
uniform Layer HeightLayers[NUMLAYERS_HEIGHTMAP];
uniform float heightScale;
#endif // USE_HEIGHTMAP

#if USE_ACCURATE_NORMALS && USE_HEIGHTMAP
uniform float deltaTheta0;
uniform float deltaTheta1;
uniform float deltaPhi0;
uniform float deltaPhi1;
uniform float tileDelta;
#endif // USE_ACCURATE_NORMALS && USE_HEIGHTMAP

// levelWeights := Variable to determine which texture to sample from
// HeightLayers := Three textures to sample from
float getUntransformedTileHeight(vec2 uv, vec3 levelWeights) {
  float height = CHUNK_DEFAULT_HEIGHT;

#if USE_HEIGHTMAP
  // Calculate desired level based on distance to the vertex on the ellipsoid before any
  // heightmapping is done.
  height = calculateUntransformedHeight(uv, levelWeights, HeightLayers);
#endif // USE_HEIGHTMAP

    return height;
}

// levelWeights := Variable to determine which texture to sample from
// HeightLayers := Three textures to sample from
float getTileHeight(vec2 uv, vec3 levelWeights) {
  float height = CHUNK_DEFAULT_HEIGHT;

#if USE_HEIGHTMAP
  // Calculate desired level based on distance to the vertex on the ellipsoid before any
  // heightmapping is done
  height = calculateHeight(uv, levelWeights, HeightLayers);
#endif // USE_HEIGHTMAP

    return height;
}

float getTileHeightScaled(vec2 uv, vec3 levelWeights) {
  float height = getTileHeight(uv, levelWeights);

#if USE_HEIGHTMAP
    height *= heightScale;
#endif // USE_HEIGHTMAP

    return height;
}

vec3 getTileNormal(vec2 uv, vec3 levelWeights, vec3 ellipsoidNormalCameraSpace,
                   vec3 ellipsoidTangentThetaCameraSpace,
                   vec3 ellipsoidTangentPhiCameraSpace)
{
  vec3 normal = ellipsoidNormalCameraSpace;

#if USE_ACCURATE_NORMALS
  float deltaPhi = mix(deltaPhi0, deltaPhi1, uv.x);
  float deltaTheta = mix(deltaTheta0, deltaTheta1, uv.y);

  vec3 deltaPhiVec = ellipsoidTangentPhiCameraSpace * deltaPhi;
  vec3 deltaThetaVec = ellipsoidTangentThetaCameraSpace * deltaTheta;

  float height00 = getTileHeightScaled(uv, levelWeights);
  float height10 = getTileHeightScaled(uv + vec2(tileDelta, 0.0), levelWeights);
  float height01 = getTileHeightScaled(uv + vec2(0.0, tileDelta), levelWeights);

  vec3 diffTheta = deltaThetaVec + ellipsoidNormalCameraSpace * (height10 - height00);
  vec3 diffPhi = deltaPhiVec + ellipsoidNormalCameraSpace * (height01 - height00);

  normal = normalize(cross(diffTheta, diffPhi));
#endif // USE_ACCURATE_NORMALS
  return normal;
}

#endif // TILE_HEIGHT_HGLSL
