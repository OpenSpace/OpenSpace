/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

layout(location = 0) in vec3 in_arrowVertex;
layout(location = 1) in vec3 in_position;
layout(location = 2) in vec3 in_direction;
layout(location = 3) in float in_magnitude;

uniform mat4 modelViewProjection;
uniform float arrowScale;

flat out vec3 v_dir;
out float vs_positionDepth;
flat out float mag;

mat3 makeRotation(vec3 dir) {
    vec3 x = normalize(dir);
    vec3 up = abs(x.z) < 0.999 ? vec3(0,0,1) : vec3(0,1,0);
    vec3 y = normalize(cross(up, x));
    vec3 z = cross(x, y);
    return mat3(x, y, z);
}

float exponentialScale(float sliderValue, float minExp, float maxExp)
{
    // Clamp input just in case
    if (sliderValue < 1) sliderValue = 1;
    if (sliderValue > 100) sliderValue = 100;

    // Normalize slider to 0–1
    float t = (sliderValue - 1) / 99.0;

    // Interpolate exponent
    float exponent = minExp + t * (maxExp - minExp);

    // Base-10 exponential
    return pow(10.0, exponent);
}

void main() {
    mat3 R = makeRotation(in_direction);

    vec3 scaledPosition = in_arrowVertex * (in_magnitude * exponentialScale(arrowScale, 2.5, 23.0));
    vec3 worldPosition = in_position + R * scaledPosition;

    vec4 vsPositionClipSpace = modelViewProjection * vec4(worldPosition, 1.0);
    vs_positionDepth = vsPositionClipSpace.w;

    gl_Position = z_normalization(vsPositionClipSpace);
    v_dir = in_direction;
    mag = in_magnitude;
}