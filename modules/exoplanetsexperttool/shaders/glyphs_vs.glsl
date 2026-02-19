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

const int MaxColors = 8;

layout(location = 0) in vec3 in_position;
layout(location = 1) in float in_component;
layout(location = 2) in int in_glyphIndex;
layout(location = 3) in int in_nColors;

// Always pass 8 colors
layout(location = 4) in vec4 in_colors[MaxColors];

out Data {
  flat float component;
  flat int nColors;
  flat vec4 colors[MaxColors];
  flat int glyphIndex;
  flat dvec4 dposWorld;
} out_data;

uniform dmat4 modelMatrix;

void main() {
  out_data.component = in_component;
  out_data.nColors = in_nColors;
  out_data.colors = in_colors;
  out_data.dposWorld = modelMatrix * dvec4(in_position, 1.0);
  out_data.glyphIndex = in_glyphIndex;
}
