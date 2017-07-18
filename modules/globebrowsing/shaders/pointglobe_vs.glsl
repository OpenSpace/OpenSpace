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

layout(location = 0) in vec2 in_position;

uniform float lightIntensityClamped;
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform mat4 directionToSunViewSpace;

out vec4 vs_positionClipSpace;
out vec4 vs_positionCameraSpace;
out vec2 vs_positionModelSpace;

void main() {
    vs_positionModelSpace = in_position;

    float totalIntensity = lightIntensityClamped;
    
    vec4 positionCameraSpace = modelViewTransform * vec4(in_position * totalIntensity, 0, 1);
    
    // Position
    vec4 positionClipSpace = projectionTransform * positionCameraSpace;
    
    vs_positionClipSpace = z_normalization(positionClipSpace);
    
    gl_Position = z_normalization(positionClipSpace);
}
