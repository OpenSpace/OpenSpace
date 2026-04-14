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

layout (shared) buffer atlasMapBlock_#{id} {
    uint atlasMap_#{id}[];
};

uniform float opacity_#{id};
uniform sampler1D transferFunction_#{id};
uniform sampler3D textureAtlas_#{id};
uniform int gridType_#{id};
uniform uint maxNumBricksPerAxis_#{id};
uniform uint paddedBrickDim_#{id};
uniform ivec3 nBricksInAtlas_#{id};
uniform ivec3 atlasSize_#{id};
uniform float stepSizeCoefficient_#{id} = 1.0;


void atlasMapDataFunction_#{id}(ivec3 brickCoords, inout uint atlasIntCoord,
                                inout uint level)
{
  int linearBrickCoord = intCoord(brickCoords, ivec3(maxNumBricksPerAxis_#{id}));
  uint mapData = atlasMap_#{id}[linearBrickCoord];
  level = mapData >> 28;
  atlasIntCoord = mapData & 0x0FFFFFFF;
}

vec3 atlasCoordsFunction_#{id}(vec3 position) {
  uint maxNumBricksPerAxis = maxNumBricksPerAxis_#{id};
  uint paddedBrickDim = paddedBrickDim_#{id};

  ivec3 brickCoords = ivec3(position * maxNumBricksPerAxis);
  uint atlasIntCoord;
  uint level;
  atlasMapDataFunction_#{id}(brickCoords, atlasIntCoord, level);

  float levelDim = float(maxNumBricksPerAxis) / exp2(level);
  vec3 inBrickCoords = mod(position * levelDim, 1.0);

  float scale = float(paddedBrickDim) - 2.0;
  vec3 paddedInBrickCoords = (1.0 + inBrickCoords * scale) / paddedBrickDim;

  ivec3 numBricksInAtlas = ivec3(vec3(atlasSize_#{id}) / paddedBrickDim);
  vec3 atlasOffset = vec3Coords(atlasIntCoord, numBricksInAtlas);
  return (atlasOffset + paddedInBrickCoords) / vec3(numBricksInAtlas);
}

float stepSize#{id}(vec3 samplePos, vec3 dir) {
  return 0.01;
  if (true /*opacity_#{id} >= OpacityThreshold*/) {
    return stepSizeCoefficient_#{id} /
      float(maxNumBricksPerAxis_#{id}) / float(paddedBrickDim_#{id});
  }
  else {
    // return a number that is garantueed to be bigger than the whole volume
    return 2.0;
  }
}

void sample#{id}(vec3 samplePos, vec3 dir, inout vec3 accumulatedColor,
                 inout vec3 accumulatedAlpha, inout float maxStepSize)
{
  if (true /*opacity_#{id} >= OpacityThreshold*/) {
    if (gridType_#{id} == 1) {
      samplePos = cartesianToSpherical(samplePos);
    }
    vec3 sampleCoords = atlasCoordsFunction_#{id}(samplePos);
    float intensity = texture(textureAtlas_#{id}, sampleCoords).x;
    maxStepSize = stepSizeCoefficient_#{id} /
        float(maxNumBricksPerAxis_#{id}) / float(paddedBrickDim_#{id});
    vec4 contribution = texture(transferFunction_#{id}, intensity);
    contribution.a = 1.0 - pow(1.0 - contribution.a, maxStepSize);

    vec3 oneMinusFrontAlpha = vec3(1.0) - accumulatedAlpha;
    accumulatedColor += oneMinusFrontAlpha * contribution.rgb * contribution.a;
    accumulatedAlpha += oneMinusFrontAlpha * vec3(contribution.a);
  }
  else {
    maxStepSize = 2.0;
  }
}
