/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#endif // USE_ACCURATE_NORMALS

#if USE_ECLIPSE_SHADOWS
out vec3 positionWorldSpace;
uniform dmat4 inverseViewTransform;
#endif

uniform mat4 projectionTransform;
// Input points in camera space
uniform vec3 p00;
uniform vec3 p10;
uniform vec3 p01;
uniform vec3 p11;
uniform vec3 patchNormalCameraSpace;
uniform vec3 patchNormalModelSpace;
uniform float chunkMinHeight;

uniform float distanceScaleFactor;
uniform int chunkLevel;

vec3 bilinearInterpolation(vec2 uv) {
    vec3 p0 = (1 - uv.x) * p00 + uv.x * p10;
    vec3 p1 = (1 - uv.x) * p01 + uv.x * p11;
    vec3 p = (1 - uv.y) * p0 + uv.y * p1;
    return p;
}

void main() {
    // Position in cameraspace
    vec3 p = bilinearInterpolation(in_uv);
    
    // Calculate desired level based on distance to the vertex on the ellipsoid
    // Before any heightmapping is done
    float distToVertexOnEllipsoid =
        length(p + patchNormalCameraSpace * chunkMinHeight);
    float levelInterpolationParameter =
        getLevelInterpolationParameter(
            chunkLevel,
            distanceScaleFactor,
            distToVertexOnEllipsoid);
    
    // use level weight for height sampling, and output to fragment shader
    levelWeights = getLevelWeights(levelInterpolationParameter);

    // Get the height value and apply skirts
    float height =
        getTileHeightScaled(in_uv, levelWeights) - getTileVertexSkirtLength();
    
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
    fs_normal = patchNormalModelSpace;
    positionCameraSpace = p;

    #if USE_ECLIPSE_SHADOWS
    positionWorldSpace = vec3(inverseViewTransform * dvec4(p, 1.0));
    #endif
}
