/*********************************************************************************
 *
 * Inviwo - Interactive Visualization Workshop
 *
 * Copyright (c) 2023 Inviwo Foundation
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
#include "utils/pickingutils.glsl"

layout(location = 5) in int in_tetraFaceId;

uniform GeometryParameters geometry;
uniform CameraParameters camera;

// #include "utils/structs.glsl"
// #include "utils/pickingutils.glsl"
#version 460

layout(location = 0) in vec3 in_position;
layout(location = 1) in int in_tetraFaceId;

//struct GeometryParameters {
////    mat4 dataToModel;
////    mat4 modelToData;
////    mat4 modelToWorld;
////    mat4 worldToModel;
////    mat3 modelToWorldNormalMatrix; // Equivalent to normalMatrix
////    mat3 dataToWorldNormalMatrix;  // Equivalent to normalMatrix
//};
uniform mat4 dataToWorld;
uniform mat4 worldToData;

//struct CameraParameters {
//   mat4 worldToView; // Equivalent to view
//   mat4 viewToWorld; // Equivalent to viewInverse
//   mat4 viewToClip; // Equivalent to projection
//   mat4 clipToView; // Equivalent to projectionInverse
uniform dmat4 worldToClip; // Equivalent to viewProjection
//   mat4 clipToWorld; // Equivalent to viewProjectionInverse
uniform vec3 position;
//   float nearPlane;  // zNear
//   float farPlane;   // zFar
//};

uint reverseByte(uint b) {
    b = (b & uint(0xF0)) >> 4 | (b & uint(0x0F)) << 4;
    b = (b & uint(0xCC)) >> 2 | (b & uint(0x33)) << 2;
    b = (b & uint(0xAA)) >> 1 | (b & uint(0x55)) << 1;
    return b;
}

vec3 pickingIndexToColor(uint id) {
    uint index = id;

    uint r = 0u;
    uint g = 0u;
    uint b = 0u;

    for (int i = 0; i < 8; ++i) {
        r |= ((index & uint(1 << (3 * i + 2))) >> (2 * i + 2));
        g |= ((index & uint(1 << (3 * i + 1))) >> (2 * i + 1));
        b |= ((index & uint(1 << (3 * i + 0))) >> (2 * i + 0));
    }

    return vec3(reverseByte(r), reverseByte(g), reverseByte(b)) / 255.0;
}
// Co

//uniform GeometryParameters geometry;
//uniform CameraParameters camera;

out Fragment {
    smooth vec4 worldPosition;
    smooth vec3 position;
    flat vec4 color;
    flat int tetraFaceId;

    flat vec3 camPosData;
} out_vert;

void main(void) {
    // gl_VertexID
    out_vert.color = vec4(pickingIndexToColor(in_tetraFaceId + 1), 1.0);
    out_vert.tetraFaceId = in_tetraFaceId;
    out_vert.camPosData = vec3(worldToData * vec4(position, 1.0));

    out_vert.position = in_position.xyz;
    out_vert.worldPosition = dataToWorld * vec4(in_position.xyz, 1.0);
//    gl_Position = worldToClip * out_vert.worldPosition;
    gl_Position = vec4(worldToClip * dvec4(in_position.xyz, 1.0));
}
