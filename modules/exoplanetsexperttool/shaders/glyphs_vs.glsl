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

const int MaxColors = 8;

layout(location = 0) in vec3 in_position;
layout(location = 1) in float in_component;
layout(location = 2) in int in_glyphIndex;
layout(location = 3) in int in_nColors;

// Always pass 8 colors
layout(location = 4) in vec4 in_colors[MaxColors];

flat out float vs_component;
flat out int vs_nColors;
flat out vec4 vs_colors[MaxColors];
out dvec4 vs_dposWorld;
flat out int vs_glyphIndex;

uniform dmat4 modelMatrix;

void main() {
    dvec4 position = dvec4(in_position, 1.0);

    vs_component = in_component;
    vs_nColors = in_nColors;
    vs_colors = in_colors;
    vs_dposWorld = modelMatrix * position;
    vs_glyphIndex = in_glyphIndex;
}
