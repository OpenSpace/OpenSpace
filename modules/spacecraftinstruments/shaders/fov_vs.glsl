/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

layout(location = 0) in vec3 in_point_position;
layout (location = 1) in int colorInformation;

out vec4 vs_color;
out vec4 vs_positionScreenSpace;

uniform mat4 modelViewProjectionTransform;

uniform vec3 defaultColorStart;
uniform vec3 defaultColorEnd;
uniform vec3 activeColor;
uniform vec3 targetInFieldOfViewColor;
uniform vec3 intersectionStartColor;
uniform vec3 intersectionEndColor;
uniform vec3 squareColor;
uniform float interpolation;

// This needs to be synced with the RenderableFov header
const int VertexColorTypeDefaultStart = 0;
const int VertexColorTypeDefaultEnd = 1;
const int VertexColorTypeInFieldOfView = 2;
const int VertexColorTypeActive = 3;
const int VertexColorTypeIntersectionStart = 4;
const int VertexColorTypeIntersectionEnd = 5;
const int VertexColorTypeSquare = 6;


void main() {
    vec4 position = vec4(in_point_position, 1);
    vec4 positionClipSpace = modelViewProjectionTransform * position;

    vs_positionScreenSpace = z_normalization(positionClipSpace);
    gl_Position = vs_positionScreenSpace;

    vec3 color;
    switch (colorInformation) { 
        case VertexColorTypeDefaultStart:
            color = defaultColorStart;
            break;
        case VertexColorTypeDefaultEnd:
            color = defaultColorEnd;
            break;
        case VertexColorTypeInFieldOfView:
            color = activeColor * interpolation + targetInFieldOfViewColor * (1 - interpolation);
            break;
        case VertexColorTypeActive:
            color = activeColor;
            break;
        case VertexColorTypeIntersectionStart:
            color = intersectionStartColor;
            break;
        case VertexColorTypeIntersectionEnd:
            color = activeColor * interpolation + intersectionEndColor * (1 - interpolation);
            break;
        case VertexColorTypeSquare:
            color = activeColor * interpolation + squareColor * (1 - interpolation);
            break;
        default:
            color = vec3(1.0, 0.0, 1.0);
    }
    vs_color = vec4(color, 1.0);
}
