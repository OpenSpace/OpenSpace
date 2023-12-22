/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

const float RefSsamplingInterval = 150.0;
const float ErtThreshold = 0.99;  // threshold for early ray termination
const float InvalidDepth = 1.0e8;

uniform int maxSteps_#{id} = 1000;
uniform int numTetraSamples_#{id} = 100;
uniform float opacityScaling_#{id} = 1.0;
uniform sampler2D transferFunction_#{id};

// Use (scalar + tfValueOffset_#{id}) * tfValueScaling_#{id} to map scalar values from 
// [min,max] to [0,1]
uniform float tfValueScaling_#{id} = 1.0;
uniform float tfValueOffset_#{id} = 0.0;

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

in Fragment_tetra {
    smooth vec4 worldPosition;
    smooth vec3 position;
    flat vec4 color;
    flat int tetraFaceId;

    flat vec3 camPosData;
} in_frag;

struct Tetra {
    // vertices
    mat4x3 v;
    // scalar values
    vec4 s; 
    // oriented face areas (in negative normal direction) as used in barycentricWeights(),
    // their magnitude is equivalent to two times the face area.
    mat4x3 fA;
    // 1 over determinant of the Jacobian, where det(Jacobian) = 6 vol(tetra)
    float jacobyDetInv; 
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

    // the determinant of the Jacobian of the tetrahedra is det = 6 V, where V is its
    // volume
    float s = dot(cross(t.v[2] - t.v[0], t.v[3] - t.v[2]), t.v[1] - t.v[0]);
    t.jacobyDetInv = 1.0 / s;

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
    return mat4x3(-normalize(t.fA[0]), -normalize(t.fA[1]), -normalize(t.fA[2]),
                  -normalize(t.fA[3]));
}

struct ExitFace {
    int faceId;
    float segmentLength;
};

// Determine the closest exit face within the tetrahedron \p tetra given a ray at
// \p startPosition and direction \p rayDirection.
//
// @param tetra          current tetrahedron
// @param entryFaceId    local face ID of the face [0,3] through which the ray entered the
// tetrahedron
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

    // only consider intersections on the inside of the current triangle faces, that is 
    // t > 0. Also ignore intersections being parallel to a face
    vt = mix(vt, vec4(InvalidDepth), lessThan(vdir, vec4(0.0)));

    // ignore self-intersection with current face ID, set distance to max
    vt[entryFaceId] = InvalidDepth;

    // closest intersection
    // face ID of closest intersection
    const int face1 = vt.x < vt.y ? 0 : 1;
    const int face2 = vt.z < vt.w ? 2 : 3;
    const int face = vt[face1] < vt[face2] ? face1 : face2;        
    const float tmin = vt[face];

    return ExitFace(face, tmin);
}

// Compute the absorption along distance \p tIncr according to the volume rendering
// equation. The \p opacityScaling_#{id} factor is used to scale the extinction to account
// for differently sized datasets.
float absorption(in float opacity, in float tIncr) {
    return 1.0 - pow(
        1.0 - opacity,
        tIncr * RefSsamplingInterval * opacityScaling_#{id}
    );
}

float normalizeScalar(float scalar) {
    return (scalar + tfValueOffset_#{id}) * tfValueScaling_#{id};
}


// Interpolate scalars of tetrahedron \p tetra using barycentric coordinates for position
// \p p within
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

void sample#{id}(vec3 samplePos, vec3 dir, inout vec3 accumulatedColor,
                 inout vec3 accumulatedAlpha, inout float stepSize)
{
    // all computations take place in Data space
    const vec3 rayDirection = dir; //normalize(in_frag.position - in_frag.camPosData);
    // const float tEntry = length(dir); //length(in_frag.position - in_frag.camPosData);

    const float tetraSamplingDelta = 1.0 / float(numTetraSamples_#{id});

    int tetraFaceId = in_frag.tetraFaceId;
    vec3 pos = samplePos; //in_frag.position;

    int tetraId = tetraFaceId / 4;
    int localFaceId = tetraFaceId % 4;
    ivec4 vertices = vertexIds[tetraId];

    // determine scalar value at entry position
    Tetra tetra = getTetra(tetraId);

    float prevScalar = normalizeScalar(barycentricInterpolation(pos, tetra));
    vec4 dvrColor = vec4(0);

    int steps = 0;
    while (tetraFaceId > -1 && steps < maxSteps_#{id} && dvrColor.a < ErtThreshold) {

        // find next tetra
        tetraId = tetraFaceId / 4;
        localFaceId = tetraFaceId % 4;
        vertices = vertexIds[tetraId];

        // query data of current tetrahedron
        tetra = getTetra(tetraId);
        ExitFace exitFace = findTetraExitFace(tetra, localFaceId, pos, rayDirection);

        vec3 endPos = pos + rayDirection * exitFace.segmentLength;

        const float scalar = normalizeScalar(barycentricInterpolation(endPos, tetra));
       
       // If using original coordinates this factor must be divided by
       // e.g., 1000 to achieve better rendering results, TODO: find
       // a scalar value that is not randomly chosen. Issue is that the
       // segmentLength gets scaled -> normalize it?
        float tDelta = exitFace.segmentLength * tetraSamplingDelta; 
        for (int i = 1; i <= numTetraSamples_#{id}; ++i) {
            float s = mix(prevScalar, scalar, i * tetraSamplingDelta);

            vec4 color = texture(transferFunction_#{id}, vec2(s, 0.5f));
            if (color.a > 0) {
                // volume integration along current segment
                color.a = absorption(color.a, tDelta);
                // front-to-back blending
                color.rgb *= color.a;
                dvrColor += (1.0 - dvrColor.a) * color;
            }
        }

        accumulatedColor = dvrColor.rgb;
        accumulatedAlpha = dvrColor.aaa;

        prevScalar = scalar;

        // update position
        pos = endPos;
        
        // determine the half face opposing the half face with the found intersection
        tetraFaceId = faceIds[tetraId][exitFace.faceId];
        ++steps;
    }
    // This is done to avoid calling sample#{id} multiple times
    stepSize = 0.0;
}

float stepSize#{id}(vec3 samplePos, vec3 dir) {
    return 1.0 / float(numTetraSamples_#{id});
}
