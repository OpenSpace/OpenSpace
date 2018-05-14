/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

// Vertex attributes
layout(location = 0) in vec4 in_position;
//layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

// Uniforms
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;

uniform vec4 cameraCenter;
uniform vec4 cameraAxis;
uniform vec4 cameraHorizontal;
uniform vec4 cameraVector;

uniform vec4 cameraCenter2;
uniform vec4 cameraAxis2;
uniform vec4 cameraHorizontal2;
uniform vec4 cameraVector2;

uniform vec3 cameraDirectionWorldSpace;

uniform float _magnification;

// Outputs
out vec2 vs_st;
out vec2 vs_st2;
out vec3 vs_normalViewSpace;
out vec4 vs_positionScreenSpace;
out vec4 vs_positionCameraSpace;
out vec4 vs_color;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {

    vec4 pointToCamera = in_position - cameraCenter;
    vec4 pointToCamera2 = in_position - cameraCenter2;

    // CAHV texture calculation
    // x = (P - C) * H / (P - C) * A
    // y = (P - C) * V / (P - C) * A
    double floor = dot(pointToCamera, cameraAxis);

    double xRoof = dot(pointToCamera, cameraHorizontal);

    double yRoof = dot(pointToCamera, cameraVector);

    double x = xRoof / floor;
    double y = yRoof / floor;

    int colsize = 1024;
    vec2 textureSize = vec2(colsize,colsize);

    vec2 uv = vec2((1.0 / textureSize.x) * x, 1.0 - (1.0 / textureSize.y) * y);

    double floor2 = dot(pointToCamera2, cameraAxis2);

    double xRoof2 = dot(pointToCamera2, cameraHorizontal2);

    double yRoof2 = dot(pointToCamera2, cameraVector2);

    double x2 = xRoof2 / floor2;
    double y2 = yRoof2 / floor2;

    vec2 textureSize2 = vec2(1354.0, 1209.0);

    vec2 uv2 = vec2((1.0 / textureSize2.x) * x2, 1.0 - (1.0 / textureSize2.y) * y2);

    vec4 position = in_position;
    position.z = position.z - 0.1;
    position.xyz *= pow(10, _magnification);
    vs_positionCameraSpace = modelViewTransform * position;
    vec4 positionClipSpace = projectionTransform * vs_positionCameraSpace;

    // Write output
    vs_st = uv;
    vs_st2 = uv2;
    vs_positionScreenSpace = z_normalization(positionClipSpace);
    gl_Position = vs_positionScreenSpace;

    // The normal transform should be the transposed inverse of the model transform?
    vs_normalViewSpace = normalize(mat3(modelViewTransform) * in_normal);
}
