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

#include "powerscaling/powerscaling_vs.glsl"

layout(location = 0) in vec3 in_point_position;

out Data {
  vec4 gPosition;
  noperspective vec2 mathLine;
  float positionDepth;
  float fade;
} out_data;

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform int idOffset;
uniform int nVertices;
uniform bool useLineFade;
uniform float lineLength;
uniform float lineFadeAmount;
uniform int vertexSortingMethod;
uniform int pointSize;
uniform int stride;

uniform vec4 viewport;

uniform bool useSplitRenderMode;
uniform int numberOfUniqueVertices;
uniform int floatingOffset;

// Fragile! Keep in sync with RenderableTrail::render
const int VertexSortingNewestFirst = 0;
const int VertexSortingOldestFirst = 1;
const int VertexSortingNoSorting = 2;


void main() {
  int modId = gl_VertexID;

  if ((vertexSortingMethod != VertexSortingNoSorting) && useLineFade) {
    float id = 0;

    if (useSplitRenderMode) {
        // Calculates id for when using split render mode (renderableTrailTrajectory)
        id = float(floatingOffset + modId) / float(max(1, numberOfUniqueVertices - 1));
    }
    else {
        // Account for a potential rolling buffer
        modId = gl_VertexID - idOffset;
        if (modId < 0) {
          modId += nVertices;
        }

        // Convert the index to a [0,1] range
        id = float(modId) / float(nVertices);
    }

    if (vertexSortingMethod == VertexSortingNewestFirst) {
      id = 1.0 - id;
    }

    float b0 = lineLength;
    float b1 = lineFadeAmount;

    float fadeValue = 0.0;
    if (id <= b0) {
      fadeValue = 0.0;
    }
    else if (id > b0 && id < b1) {
      fadeValue = (id - b0) / (b1 - b0);
    }
    else {
      fadeValue = 1.0;
    }

    out_data.fade = clamp(fadeValue, 0.0, 1.0);
  }
  else {
    out_data.fade = 1.0;
  }

  out_data.gPosition = vec4(modelViewTransform * dvec4(in_point_position, 1));
  vec4 positionClipSpace = projectionTransform * out_data.gPosition;
  out_data.positionDepth = positionClipSpace.w;

  float pts = float(pointSize);
  gl_PointSize = (stride == 1 || int(modId) % stride == 0) ? pts : pts / 2.0;
  gl_Position = z_normalization(positionClipSpace);

  vec4 positionNdc = positionClipSpace / positionClipSpace.w;
  out_data.mathLine = 0.5 * (positionNdc.xy + vec2(1.0)) * viewport.zw;
}
