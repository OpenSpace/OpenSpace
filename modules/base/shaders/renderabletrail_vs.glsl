/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

out float vs_positionDepth;
out vec4 vs_gPosition;
out float fade;
noperspective out vec2 mathLine;

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform int idOffset;
uniform int nVertices;
uniform bool useLineFade;
uniform float lineFade;
uniform int vertexSortingMethod;
uniform int pointSize;
uniform int stride;

uniform vec4 viewport;

// Fragile! Keep in sync with RenderableTrail::render
#define VERTEX_SORTING_NEWESTFIRST 0
#define VERTEX_SORTING_OLDESTFIRST 1
#define VERTEX_SORTING_NOSORTING 2


void main() {
  int modId = gl_VertexID;

  if ((vertexSortingMethod != VERTEX_SORTING_NOSORTING) && useLineFade) {
    // Account for a potential rolling buffer
    modId = gl_VertexID - idOffset;
    if (modId < 0) {
      modId += nVertices;
    }

    // Convert the index to a [0,1] ranger
    float id = float(modId) / float(nVertices);

    if (vertexSortingMethod == VERTEX_SORTING_NEWESTFIRST) {
      id = 1.0 - id;
    }

    fade = clamp(id * lineFade, 0.0, 1.0); 
  }
  else {
    fade = 1.0;
  }

  vs_gPosition = vec4(modelViewTransform * dvec4(in_point_position, 1));
  vec4 vs_positionClipSpace = projectionTransform * vs_gPosition;
  vec4 vs_positionNDC = vs_positionClipSpace / vs_positionClipSpace.w;
  vs_positionDepth = vs_positionClipSpace.w;
  
  gl_PointSize = (stride == 1 || int(modId) % stride == 0) ? 
                  float(pointSize) : float(pointSize) / 2;
  gl_Position  = z_normalization(vs_positionClipSpace);

  mathLine = 0.5 * (vs_positionNDC.xy + vec2(1.0)) * viewport.zw;
}
