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

layout(location = 0) in vec3 in_point_position;
layout (location = 1) in int colorInformation;

out vec4 vs_color;
out float vs_depth;

uniform mat4 modelViewProjectionTransform;
uniform vec3 colorStart;
uniform vec3 colorEnd;
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
  vec4 positionClipSpace = modelViewProjectionTransform * vec4(in_point_position, 1.0);

  vec4 pos = z_normalization(positionClipSpace);
  vs_depth = pos.w;
  gl_Position = pos;

  vec3 color;
  switch (colorInformation) {
    case VertexColorTypeDefaultStart:
      vs_color = vec4(colorStart, 1.0);
      break;
    case VertexColorTypeDefaultEnd:
      vs_color = vec4(colorEnd, 1.0);
      break;
    case VertexColorTypeInFieldOfView:
      vs_color = vec4(
        activeColor * interpolation + targetInFieldOfViewColor * (1.0 - interpolation),
        1.0
      );
      break;
    case VertexColorTypeActive:
      vs_color = vec4(activeColor, 1.0);
      break;
    case VertexColorTypeIntersectionStart:
      vs_color = vec4(intersectionStartColor, 1.0);
      break;
    case VertexColorTypeIntersectionEnd:
      vs_color = vec4(
        activeColor * interpolation + intersectionEndColor * (1.0 - interpolation),
        1.0
      );
      break;
    case VertexColorTypeSquare:
      vs_color = vec4(
        activeColor * interpolation + squareColor * (1.0 - interpolation),
        1.0
      );
      break;
    default:
      vs_color = vec4(1.0, 0.0, 1.0, 1.0);
  }
}
