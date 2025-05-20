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

#version __CONTEXT__
#include "PowerScaling/powerScaling_vs.hglsl"

layout(location = 0) in vec2 in_position;

out vec3 texCoord;
out vec4 positionCameraSpace;

uniform vec3 normal;
uniform float offset;
uniform mat3 basis;

uniform mat4 modelViewProjection;
uniform mat4 modelViewTransform;
uniform mat4 modelTransform;

void main() {
    // The quad is covers -1.0 to 1.0 while the volume is defined for -0.5 to 0.5
    vec2 pos = in_position * 0.5; // map quad to the range -0.5 to 0.5
    // Scale plane so that it will cover entire volume on the diagonal
    float quadScale = 1.42;
    pos *= quadScale;
    vec3 sliceCenter = normal * offset;
    // Convert 2D quad coordinates into 3D slice position
    vec3 localPos = sliceCenter + basis * vec3(pos, 0.0);
    // Remap texture coordinates to the range 0 to 1
    texCoord = localPos + 0.5;

    positionCameraSpace = modelViewTransform * modelTransform * vec4(localPos, 1.0);

    vec4 positionClipSpace = modelViewProjection * modelTransform * vec4(localPos, 1.0);
    vec4 positionScreenSpace = z_normalization(positionClipSpace);

    gl_Position =  positionScreenSpace;
}