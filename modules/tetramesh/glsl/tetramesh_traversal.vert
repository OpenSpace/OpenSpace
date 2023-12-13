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
