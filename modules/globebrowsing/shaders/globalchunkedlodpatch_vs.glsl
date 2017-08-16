/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2017                                                             *
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
#include <${MODULE_GLOBEBROWSING}/shaders/ellipsoid.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/tile.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/texturetilemapping.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/tileheight.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/tilevertexskirt.hglsl>

layout(location = 1) in vec2 in_uv;

out vec2 fs_uv;
out vec4 fs_position;
out vec3 fs_normal;
out vec3 ellipsoidNormalCameraSpace;
out LevelWeights levelWeights;
out vec3 positionCameraSpace;

#if USE_ACCURATE_NORMALS
out vec3 ellipsoidTangentThetaCameraSpace;
out vec3 ellipsoidTangentPhiCameraSpace;
#endif //USE_ACCURATE_NORMALS

uniform mat4 modelViewProjectionTransform;
uniform mat4 modelViewTransform;
uniform vec3 radiiSquared;

uniform vec2 minLatLon;
uniform vec2 lonLatScalingFactor;
uniform vec3 cameraPosition;
uniform float chunkMinHeight;

uniform float distanceScaleFactor;
uniform int chunkLevel;


PositionNormalPair globalInterpolation(vec2 uv) {
    vec2 lonLatInput;
    lonLatInput.y = minLatLon.y + lonLatScalingFactor.y * uv.y; // Lat
    lonLatInput.x = minLatLon.x + lonLatScalingFactor.x * uv.x; // Lon
    PositionNormalPair positionPairModelSpace = geodetic2ToCartesian(
        lonLatInput.y,
        lonLatInput.x,
        radiiSquared
    );
    return positionPairModelSpace;
}

void main() {
    PositionNormalPair pair = globalInterpolation(in_uv);
    float distToVertexOnEllipsoid =
        length(pair.position + pair.normal * chunkMinHeight - cameraPosition);

    float levelInterpolationParameter =
        getLevelInterpolationParameter(
            chunkLevel,
            distanceScaleFactor,
            distToVertexOnEllipsoid);

    // use level weight for height sampling, and output to fragment shader
    levelWeights = getLevelWeights(levelInterpolationParameter);

    // Get the height value
    float height = getTileHeight(in_uv, levelWeights);

    // Apply skirts
    height -= getTileVertexSkirtLength();

#if USE_ACCURATE_NORMALS
    // Calculate tangents
    // tileDelta is a step length (epsilon). Should be small enough for accuracy but not
    // Too small for precision. 1 / 512 is good.
    const float tileDelta = 1.0 / 512.0;
    PositionNormalPair pair10 = globalInterpolation(
        in_uv + vec2(1.0, 0.0) * tileDelta
    );
    PositionNormalPair pair01 = globalInterpolation(
        in_uv + vec2(0.0, 1.0) * tileDelta
    );
    vec3 ellipsoidTangentTheta = normalize(pair10.position - pair.position);
    vec3 ellipsoidTangentPhi = normalize(pair01.position - pair.position);
    ellipsoidTangentThetaCameraSpace = mat3(modelViewTransform) * ellipsoidTangentTheta;
    ellipsoidTangentPhiCameraSpace = mat3(modelViewTransform) * ellipsoidTangentPhi;
#endif // USE_ACCURATE_NORMALS

    // Add the height in the direction of the normal
    pair.position += pair.normal * height;
    vec4 positionClippingSpace =
        modelViewProjectionTransform * vec4(pair.position, 1);

    // Write output
    fs_uv = in_uv;
    fs_position = z_normalization(positionClippingSpace);
    gl_Position = fs_position;
    ellipsoidNormalCameraSpace = mat3(modelViewTransform) * pair.normal;
    fs_normal = pair.normal;
    positionCameraSpace = vec3(modelViewTransform * vec4(pair.position, 1));
}
