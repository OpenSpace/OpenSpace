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

#ifndef TILE_VERTEX_SKIRT_HGLSL
#define TILE_VERTEX_SKIRT_HGLSL

#include "PowerScaling/powerScaling_vs.hglsl"

uniform int xSegments;
uniform float skirtLength;

bool tileVertexIsSkirtVertex() {
  int vertexIDx = gl_VertexID % (xSegments + 3);
  int vertexIDy = gl_VertexID / (xSegments + 3);
  return vertexIDx == 0 || vertexIDy == 0 ||
         vertexIDx == (xSegments + 2) || vertexIDy == (xSegments + 2);
}

float getTileVertexSkirtLength() {
  return tileVertexIsSkirtVertex() ? skirtLength : 0.0;
}

#endif // TILE_VERTEX_SKIRT_HGLSL
