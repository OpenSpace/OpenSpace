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
    int linearBrickCoord = multires_intCoord(
        brickCoords,
        ivec3(maxNumBricksPerAxis_#{id})
    );
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

    float levelDim = float(maxNumBricksPerAxis) / pow(2.0, level);
    vec3 inBrickCoords = mod(position * levelDim, 1.0);

    float scale = float(paddedBrickDim) - 2.0;
    vec3 paddedInBrickCoords = (1.0 + inBrickCoords * scale) / paddedBrickDim;

    ivec3 numBricksInAtlas = ivec3(vec3(atlasSize_#{id}) / paddedBrickDim);
    vec3 atlasOffset = multires_vec3Coords(atlasIntCoord, numBricksInAtlas);
    return (atlasOffset + paddedInBrickCoords) / vec3(numBricksInAtlas);
}

float stepSize#{id}(vec3 samplePos, vec3 dir) {
    return 0.01;
    if (true /*opacity_#{id} >= MULTIRES_OPACITY_THRESHOLD*/) {
        return stepSizeCoefficient_#{id}/float(maxNumBricksPerAxis_#{id})/float(paddedBrickDim_#{id});
    }
    else {
        // return a number that is garantueed to be bigger than the whole volume
        return 2.0;
    }
}

void sample#{id}(vec3 samplePos, vec3 dir, inout vec3 accumulatedColor,
                 inout vec3 accumulatedAlpha, inout float maxStepSize)
{
    //vec4 sample#{id}(vec3 samplePos, vec3 dir, vec4 foregroundColor, inout float maxStepSize) {
    //return vec4(1.0, 1.0, 1.0, 1.0);

    if (true /*opacity_#{id} >= MULTIRES_OPACITY_THRESHOLD*/) {
        if (gridType_#{id} == 1) {
            samplePos = multires_cartesianToSpherical(samplePos);
        }
        vec3 sampleCoords = atlasCoordsFunction_#{id}(samplePos);
        //return vec4(sampleCoords, 1.0);
        //sampleCoords = vec3(1.0,0.0, 0.0);
        float intensity = texture(textureAtlas_#{id}, sampleCoords).x;
        //intensity = sampleCoords;
        maxStepSize = stepSizeCoefficient_#{id}/float(maxNumBricksPerAxis_#{id})/float(paddedBrickDim_#{id});
        //return vec4(vec3(intensity), 1.0);
        vec4 contribution = texture(transferFunction_#{id}, intensity);
        contribution.a = 1.0 - pow(1.0 - contribution.a, maxStepSize);
        //contribution = vec4(sampleCoords, 1.0);
        //vec4 contribution = vec4(vec3(intensity), 1.0);
        //contribution.a *= 0.3;
        //contribution = vec4(1.0, 1.0, 1.0, intensity * 1000000.0);
        //contribution = vec4(1.0, 1.0, 1.0, 1.0);

        //contribution.a *= opacity_#{id};

        //maxStepSize = 0.01;

        vec3 oneMinusFrontAlpha = vec3(1.0) - accumulatedAlpha;
        accumulatedColor += oneMinusFrontAlpha * contribution.rgb * contribution.a;
        accumulatedAlpha += oneMinusFrontAlpha * vec3(contribution.a);
    }
    else {
        maxStepSize = 2.0;
    }
}
