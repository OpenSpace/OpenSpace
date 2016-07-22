/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

// Uniforms
uniform mat4 modelViewTransform;
uniform mat4 viewTransform;
uniform mat4 projectionTransform;

uniform vec3 cameraDirectionWorldSpace;

uniform float _magnification;

// Outputs
out vec2 vs_st;
out vec3 vs_normalViewSpace;
out vec3 vs_cameraDirectionViewSpace;
out vec4 vs_position;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {
    vec4 position = in_position;
    position.xyz *= pow(10, _magnification);
    vec4 positionViewSpace = modelViewTransform * position;
    vec4 positionClipSpace = projectionTransform * positionViewSpace;

    // Write output
    vs_st = in_st;
    vs_position = z_normalization(positionClipSpace);
    gl_Position = vs_position;
    // The normal transform should be the transposed inverse of the model transform?
    vs_normalViewSpace = normalize(mat3(modelViewTransform) * in_normal);
    vs_cameraDirectionViewSpace = normalize(mat3(viewTransform) * cameraDirectionWorldSpace);
}