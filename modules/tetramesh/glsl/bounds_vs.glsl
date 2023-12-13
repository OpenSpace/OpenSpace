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

#version __CONTEXT__

#include "PowerScaling/powerScaling_vs.hglsl"

layout(location = 0) in vec3 in_position;
layout(location = 1) in int in_tetraFaceId;

out vec4 positionLocalSpace;
out vec4 positionCameraSpace;

out Fragment_tetra {
    smooth vec4 worldPosition;
    smooth vec3 position;
    flat vec4 color;
    flat int tetraFaceId;

    flat vec3 camPosData;
} out_vert;

uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;


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

void main() {
  positionLocalSpace = vec4(in_position, 1.0);
  positionCameraSpace = modelViewTransform * positionLocalSpace;

  out_vert.tetraFaceId = in_tetraFaceId;
  out_vert.color = vec4(pickingIndexToColor(in_tetraFaceId + 1), 1.0);



  vec4 positionClipSpace = projectionTransform * positionCameraSpace;
  vec4 positionScreenSpace = z_normalization(positionClipSpace);

  gl_Position = positionScreenSpace;
}
