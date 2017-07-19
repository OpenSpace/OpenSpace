/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2017                                                             *
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

in vec4 in_point_position;

out vec4 vs_positionScreenSpace;
out vec4 vs_pointColor;

uniform vec3 color;
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform int pointSteps;


void main() {
    const vec4 positionCameraSpace = modelViewTransform * in_point_position;
    const vec4 positionClipSpace = projectionTransform * positionCameraSpace;
    vs_positionScreenSpace = z_normalization(positionClipSpace);

    gl_Position = vs_positionScreenSpace;

    if (mod(gl_VertexID, pointSteps) == 0) {
        vs_pointColor.rgb = color;
        gl_PointSize = 5.0f;
    }
    else {
        vs_pointColor.rgb = (color + vec3(0.6, 0.6, 0.6)) / 2.0;
        gl_PointSize = 2.f;
    }

    // I don't like this random variable k defined in powerScalingMath.hglsl.
    // Will ignore it and use 10 in protest of psc dependencies. /KB
    // float maximumDistance = pow(k, 10);
    float maximumDistance = pow(10, 10);
    float distanceToCamera = length(positionCameraSpace.xyz);

    vs_pointColor.a = maximumDistance / (distanceToCamera / 100.0);
}