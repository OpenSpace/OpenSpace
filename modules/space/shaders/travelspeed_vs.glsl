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

layout(location = 0) in vec3 in_position;

out Data {
  vec4 positionViewSpace;
  vec4 finalColor;
  float depth;
} out_data;

uniform vec3 lineColor;
uniform float opacity;
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;


void main() {
  out_data.positionViewSpace = vec4(modelViewTransform * dvec4(in_position, 1.0));
  vec4 positionScreenSpace = projectionTransform * out_data.positionViewSpace;
  out_data.depth = positionScreenSpace.w;
  gl_Position = positionScreenSpace;

  // Makes it liniarly fade betweet vertex 0 and 1
  if (gl_VertexID == 0) {
    out_data.finalColor = vec4(0.0, 0.0, 0.0, 0.0);
  }
  // Makes sure the line between index 1 and 2 is uniformly colored
  else if (gl_VertexID == 1) {
    out_data.finalColor = vec4(lineColor, opacity);
  }
  else if (gl_VertexID == 2) {
    out_data.finalColor = vec4(lineColor, opacity);
  }
  // should never hit else
  else {
    out_data.finalColor = vec4(1.0, 1.0, 0.0, 1.0);
  }

  gl_Position.z = 0.0;
}
