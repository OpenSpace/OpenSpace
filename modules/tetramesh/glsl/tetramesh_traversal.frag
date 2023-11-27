/*********************************************************************************
 *
 * Inviwo - Interactive Visualization Workshop
 *
 * Copyright (c) 2020-2023 Inviwo Foundation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 *********************************************************************************/

#include "utils/structs.glsl"
#include "utils/shading.glsl"
#include "utils/classification.glsl"

#if !defined(REF_SAMPLING_INTERVAL)
#define REF_SAMPLING_INTERVAL 150.0
#endif

#define ERT_THRESHOLD 0.99  // threshold for early ray termination

/**
 * Data structures for tetrahedra indexing and face enumeration based on
 *    M. Lage, T. Lewiner, H. Lopes, and L. Velho.
 *    CHF: A scalable topological data structure for tetrahedral meshes.
 *    In Brazilian Symposium on Computer Graphics and Image Processing
 *    (SIBGRAPI'05), pp. 349-356, 2005, doi: 10.1109/SIBGRAPI.2005.18
 */

uniform GeometryParameters geometry;
uniform CameraParameters camera;
uniform LightParameters lighting;

uniform sampler2D transferFunction;
uniform int shaderOutput = 0;
uniform int maxSteps = 1000;
uniform int numTetraSamples = 100;

// Use (scalar + tfValueOffset) * tfValueScaling to map scalar values from [min,max] to [0,1]
uniform float tfValueScaling = 1.0;
uniform float tfValueOffset = 0.0;

// The opacity scaling factor affects the extinction parameter used in the integration along the 
// ray. It can be used to obtain the same visual impression for differently scaled datasets since 
// the ray traversal is performed in data coordinates and not restricted to [0,1] as in Inviwo's
// regular volume raycasting.
uniform float opacityScaling = 1.0;

uniform ImageParameters backgroundParameters;
uniform sampler2D backgroundColor;
uniform sampler2D backgroundDepth;

uniform sampler2D tfGradients;

const float invalidDepth = 1.0e8;

struct VertexPosition {
    vec3 pos;
    float scalar;
};

layout(std430, binding=0) readonly buffer nodeBuffer {
    VertexPosition vertexPositions[];
};
layout(std430, binding=1) readonly buffer nodeIdsBuffer {
    ivec4 vertexIds[];
};
layout(std430, binding=2) readonly buffer opposingFaceIdsBuffer {
    ivec4 faceIds[];
};

in Fragment {
    smooth vec4 worldPosition;
    smooth vec3 position;
    flat vec4 color;
    flat int tetraFaceId;

    flat vec3 camPosData;
} in_frag;


const ivec3 triIndices[4] = ivec3[4](ivec3(1, 2, 3), ivec3(2, 0, 3), 
                                     ivec3(3, 0, 1), ivec3(0, 2, 1));

struct Tetra {
    mat4x3 v; // vertices
    vec4 s; // scalar values

    mat4x3 fA; // oriented face areas (in negative normal direction) as used in barycentricWeights(), 
               // their magnitude is equivalent to two times the face area.
    float jacobyDetInv; // 1 over determinant of the Jacobian, where det(Jacobian) = 6 vol(tetra)
};

mat4x3 getFaceAreas(in Tetra t);

Tetra getTetra(in int tetraId) {
    ivec4 vertices = vertexIds[tetraId];

    VertexPosition[4] p = VertexPosition[](vertexPositions[vertices[0]],
                                           vertexPositions[vertices[1]],
                                           vertexPositions[vertices[2]],
                                           vertexPositions[vertices[3]]);

    Tetra t;
    t.v = mat4x3(p[0].pos, p[1].pos, p[2].pos, p[3].pos);
    t.s = vec4(p[0].scalar, p[1].scalar, p[2].scalar, p[3].scalar);

    t.fA = getFaceAreas(t);

    // the determinant of the Jacobian of the tetrahedra is det = 6 V, where V is its volume
    t.jacobyDetInv = 1.0 / dot(cross(t.v[2] - t.v[0], t.v[3] - t.v[2]), t.v[1] - t.v[0]);

    return t;
}

// Compute the oriented face areas (in negative normal direction) as used in barycentric 
// interpolation. Their magnitude is equivalent to two times the face area.
//
// @param t   input tetraehdron
// @return oriented face areas fA
mat4x3 getFaceAreas(in Tetra t) {
    const vec3 v_01 = t.v[1] - t.v[0];
    const vec3 v_02 = t.v[2] - t.v[0];
    const vec3 v_03 = t.v[3] - t.v[0];
    const vec3 v_12 = t.v[2] - t.v[1];
    const vec3 v_13 = t.v[3] - t.v[1];

    return mat4x3(cross(v_13, v_12),
                  cross(v_02, v_03),
                  cross(v_03, v_01),
                  cross(v_01, v_02));
}

// Compute the face normals for tetrahedron \p t
//
// @param t   input tetraehdron with oriented face areas (in negative normal direction)
// @return face normals, that is normalized(fA[0]), ..., normalized(fA[3])
mat4x3 getFaceNormals(in Tetra t) {
    return mat4x3(-normalize(t.fA[0]), -normalize(t.fA[1]), -normalize(t.fA[2]), -normalize(t.fA[3]));
}

struct ExitFace {
    int faceId;
    float segmentLength;
};

// Determine the closest exit face within the tetrahedron \p tetra given a ray at \p startPosition 
// and direction \p rayDirection.
//
// @param tetra          current tetrahedron
// @param entryFaceId    local face ID of the face [0,3] through which the ray entered the tetrahedron
// @param startPosition  start position of the ray
// @param rayDirection   direction of the ray
// @return the closest face where the ray exits the tetrahedron
ExitFace findTetraExitFace(in Tetra tetra, in int entryFaceId, 
                           in vec3 startPosition, in vec3 rayDirection) {
    const mat4x3 faceNormal = getFaceNormals(tetra);
    // intersect ray at current position with all tetra faces
    const vec4 vdir = vec4(dot(faceNormal[0], rayDirection),
                           dot(faceNormal[1], rayDirection),
                           dot(faceNormal[2], rayDirection),
                           dot(faceNormal[3], rayDirection));
    vec4 vt = vec4(dot(tetra.v[1] - startPosition, faceNormal[0]),
                   dot(tetra.v[2] - startPosition, faceNormal[1]),
                   dot(tetra.v[3] - startPosition, faceNormal[2]),
                   dot(tetra.v[0] - startPosition, faceNormal[3])) / vdir;

    // only consider intersections on the inside of the current triangle faces, that is t > 0.
    // Also ignore intersections being parallel to a face
    vt = mix(vt, vec4(invalidDepth), lessThan(vdir, vec4(0.0)));

    // ignore self-intersection with current face ID, set distance to max
    vt[entryFaceId] = invalidDepth;

    // closest intersection
    // face ID of closest intersection
    const int face1 = vt.x < vt.y ? 0 : 1;
    const int face2 = vt.z < vt.w ? 2 : 3;
    const int face = vt[face1] < vt[face2] ? face1 : face2;        
    const float tmin = vt[face];

    return ExitFace(face, tmin);
}

// Interpolate scalars of tetrahedron \p tetra using barycentric coordinates for position \p p within
//
// @param p      position of the barycentric coords
// @param tetra  input tetrahedron
// @return interpolated scalar value
// 
// see https://www.iue.tuwien.ac.at/phd/nentchev/node30.html
// and https://www.iue.tuwien.ac.at/phd/nentchev/node31.html
float barycentricInterpolation(in vec3 p, in Tetra tetra) {
    const vec3 v_0p = p - tetra.v[0];
    const vec3 v_1p = p - tetra.v[1];

    // barycentric volumes, correct volumes obtained by scaling with 1/6
    float vol0 = dot(tetra.fA[0], v_1p);
    float vol1 = dot(tetra.fA[1], v_0p);
    float vol2 = dot(tetra.fA[2], v_0p);
    float vol3 = dot(tetra.fA[3], v_0p);

    return dot(vec4(vol0, vol1, vol2, vol3) * tetra.jacobyDetInv, tetra.s);
}

// Determine barycentric gradients at each vertex of \p tetra
//
// @param tetra  input tetrahedron with oriented face areas
// @return barycentric gradients (direction matches face normals)
mat4x3 getBarycentricGradients(in Tetra tetra) {
    return tetra.fA * tetra.jacobyDetInv;    
}

// Compute the constant gradient within tetrahedron \p tetra
//
// @param tetra  input tetrahedron withoriented face areas
// @return gradient of \p tetra
vec3 getTetraGradient(in Tetra tetra) {
    // accumulate the barycentric gradients (fA / det(Jacobian)) weighted by the 
    // corresponding scalar values
    return -normalize(tetra.fA * tetra.jacobyDetInv * tetra.s);
}

// Compute the absorption along distance \p tIncr according to the volume rendering equation. The 
// \p opacityScaling factor is used to scale the extinction to account for differently sized datasets.
float absorption(in float opacity, in float tIncr) {
    return 1.0 - pow(1.0 - opacity, tIncr * REF_SAMPLING_INTERVAL * opacityScaling);
}

float normalizeScalar(float scalar) {
    return (scalar + tfValueOffset) * tfValueScaling;
}

// Convert normalized screen coordinates [0,1] to data coords of the tetramesh geometry
float convertScreenPosToDataDepth(vec3 screenPos) {
    vec4 posData = geometry.worldToData * camera.clipToWorld 
        * vec4(screenPos * 2.0 - 1.0, 1.0);
    posData /= posData.w;
    return length(posData.xyz - in_frag.camPosData);
}

// Compute the non-linear depth of a data-space position in normalized device coordinates
float normalizedDeviceDepth(in vec3 posData, in mat4 dataToClip) {
    mat4 mvpTranspose = transpose(dataToClip);

    vec4 pos = vec4(posData, 1.0);
    float depth = dot(mvpTranspose[2], pos);
    float depthW = dot(mvpTranspose[3], pos);

    return ((depth / depthW) + 1.0) * 0.5;
}

void main() {
    // all computations except illumination (World space) take place in Data space
    const vec3 rayDirection = normalize(in_frag.position - in_frag.camPosData);
    const vec3 rayDirWorld = normalize(in_frag.worldPosition.xyz - camera.position);
    const float tEntry = length(in_frag.position - in_frag.camPosData);

    const float tetraSamplingDelta = 1.0 / float(numTetraSamples);

    float bgDepthScreen = invalidDepth;

#if defined(BACKGROUND_AVAILABLE)
    vec2 screenTexCoords = gl_FragCoord.xy * backgroundParameters.reciprocalDimensions;
    vec4 background = texture(backgroundColor, screenTexCoords); 
    // prepare background color for pre-multiplied alpha blending
    background.rgb *= background.a;

    bgDepthScreen = texture(backgroundDepth, screenTexCoords).r;
    // background depth in Data space
    const float tBackground = convertScreenPosToDataDepth(vec3(screenTexCoords, bgDepthScreen));
#endif // BACKGROUND_AVAILABLE

    int tetraFaceId = in_frag.tetraFaceId;
    vec3 pos = in_frag.position;

    int tetraId = tetraFaceId / 4;
    int localFaceId = tetraFaceId % 4;
    ivec4 vertices = vertexIds[tetraId];

    // determine scalar value at entry position
    Tetra tetra = getTetra(tetraId);
    float prevScalar = normalizeScalar(barycentricInterpolation(pos, tetra));

    vec4 dvrColor = vec4(0);
#if defined(BACKGROUND_AVAILABLE)
    // blend background if it lies in front of the start position of the ray
    dvrColor = tBackground < tEntry ? background : dvrColor;

    // suppress background blending tests if the background alpha is zero
    bool bgBlended = tBackground < tEntry || background.a == 0.0;
#endif // BACKGROUND_AVAILABLE

    float tTotal = tEntry;
    float tFirstHit = invalidDepth;
    int steps = 0;
    while (tetraFaceId > -1 && steps < maxSteps && dvrColor.a < ERT_THRESHOLD) {
        // find next tetra
        tetraId = tetraFaceId / 4;
        localFaceId = tetraFaceId % 4;
        vertices = vertexIds[tetraId];

        // query data of current tetrahedron
        tetra = getTetra(tetraId);
        ExitFace exitFace = findTetraExitFace(tetra, localFaceId, pos, rayDirection);

        vec3 endPos = pos + rayDirection * exitFace.segmentLength;

        const float scalar = normalizeScalar(barycentricInterpolation(endPos, tetra));
        const vec3 gradient = getTetraGradient(tetra);
        const vec3 gradientWorld = geometry.dataToWorldNormalMatrix * gradient;
        
        float tDelta = exitFace.segmentLength * tetraSamplingDelta;
        for (int i = 1; i <= numTetraSamples; ++i) {
            float s = mix(prevScalar, scalar, i * tetraSamplingDelta);

            float tCurrent = tTotal + tDelta * i;
#if defined(BACKGROUND_AVAILABLE)
            if (!bgBlended && tBackground > tCurrent - tDelta && tBackground <= tCurrent) {
                dvrColor += (1.0 - dvrColor.a) * background;
                bgBlended = true;
            }
#endif // BACKGROUND_AVAILABLE

            vec4 color = applyTF(transferFunction, s);
            if (color.a > 0) {
                tFirstHit = tFirstHit == invalidDepth ? tCurrent : tFirstHit;

#if defined(SHADING_ENABLED)
                // perform illumination in world coordinates
                vec3 worldPos = (geometry.dataToWorld * vec4(pos + rayDirection * tDelta * i, 1.0)).xyz;
                color.rgb = APPLY_LIGHTING(lighting, color.rgb, color.rgb, vec3(1.0),
                                           worldPos, gradientWorld, -rayDirWorld);
#endif // SHADING_ENABLED

                // volume integration along current segment
                color.a = absorption(color.a, tDelta);
                // front-to-back blending
                color.rgb *= color.a;
                dvrColor += (1.0 - dvrColor.a) * color;
            }
        }

#if defined(BACKGROUND_AVAILABLE)
        // need to check the entire interval again due to precision issues
        if (!bgBlended && tBackground > tTotal && tBackground <= tTotal + exitFace.segmentLength) {
            dvrColor += (1.0 - dvrColor.a) * background;
            bgBlended = true;
        }
#endif // BACKGROUND_AVAILABLE

        prevScalar = scalar;

        // update position
        pos = endPos;
        tTotal += exitFace.segmentLength;

        // determine the half face opposing the half face with the found intersection
        tetraFaceId = faceIds[tetraId][exitFace.faceId];
        ++steps;
    }

#if defined(BACKGROUND_AVAILABLE)
    // blend result with background
    if (tBackground > tTotal) {
        // tFirstHit = tFirstHit == invalidDepth ? tBackground : tFirstHit;
        dvrColor += (1.0 - dvrColor.a) * background;
    }
#endif // BACKGROUND_AVAILABLE

    float depth = tFirstHit == invalidDepth ? 1.0 : 
        normalizedDeviceDepth(in_frag.camPosData + rayDirection * tFirstHit, 
                              camera.worldToClip * geometry.dataToWorld);

    gl_FragDepth = min(depth, bgDepthScreen);

    FragData0 = dvrColor;
}
