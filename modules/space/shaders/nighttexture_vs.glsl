/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

out vec2 vs_st;
out vec4 vs_normal;
out vec4 vs_position;
out vec4 vs_gPosition;
out vec3 vs_gNormal;

uniform mat4 ModelTransform;
uniform dmat4 modelViewTransform;
uniform mat4 modelViewProjectionTransform;

uniform sampler2D heightTex;
uniform bool _hasHeightMap;
uniform float _heightExaggeration;

void main() {
    // set variables
    vs_st = in_st;
        
    // this is wrong for the normal. The normal transform is the transposed inverse of the model transform
    vs_normal = normalize(ModelTransform * vec4(in_normal,0));
    // vs_normal = vec4(in_normal, 0.0);

    // G-Buffer
    vs_gNormal = in_normal;

    vec4 position = vec4(in_position.xyz * pow(10, in_position.w), 1.0);
    
    if (_hasHeightMap) {
        float height = texture(heightTex, in_st).r;
        vec3 displacementDirection = abs(normalize(in_normal.xyz));
        float displacementFactor = height * _heightExaggeration;
        position.xyz = position.xyz + displacementDirection * displacementFactor;
    }
    
    // G-Buffer
    vs_gPosition = vec4(modelViewTransform * position); // Must be in SGCT eye space
    
    position = modelViewProjectionTransform * position;
    vs_position = z_normalization(position);

    gl_Position =  vs_position;
}
