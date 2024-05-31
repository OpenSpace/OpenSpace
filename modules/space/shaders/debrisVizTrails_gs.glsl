/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

layout(lines) in;
flat in float currentRevolutionFraction[];
flat in float vertexRevolutionFraction[];
flat in vec4 viewSpacePositions[];

layout(line_strip, max_vertices = 2) out;
out float viewSpaceDepth;
out float periodFraction;
out float offsetPeriods;
out vec4 viewSpacePosition;

uniform float trailFadeExponent;
uniform float colorFadeCutoffValue;

void main() {
  // cFrac is how far along the trail orbit the head of the trail is.
  // v0Frac and v1Frac are how far the two vertices that creates the current line strip
  // are along the trail orbit. The variables span between 0 and 1, where 0 is the
  // beginning of the trail and 1 is the end of the trail (a full orbit).
  float cFrac = currentRevolutionFraction[0];
  float v0Frac = vertexRevolutionFraction[0];
  float v1Frac = vertexRevolutionFraction[1];

  // Distance between current revolution fraction and revolution fraction for
  // vertex0 and vertex1
  float vd0 = cFrac - v0Frac;
  if (vd0 < 0.0) {
    vd0 += 1.0;
  }

  float vd1 = cFrac - v1Frac;
  if (vd1 < 0.0) {
    vd1 += 1.0;
  }
  // ==========================

  // Calculates if vertex0 is close enough to NOT be discarded in fragment shader
  float invert = pow(1.0 - vd1, trailFadeExponent);  
  float fade = clamp(invert, 0.0, 1.0);

  // Only emit vertices for line segments where both vertices should be rendered
  if ((fade > colorFadeCutoffValue) || (vd0 < vd1)) {
    gl_Position = gl_in[0].gl_Position;
    viewSpaceDepth = gl_Position.w;
    periodFraction = cFrac;
    offsetPeriods = v0Frac;
    viewSpacePosition = viewSpacePositions[0];
    EmitVertex();

    gl_Position = gl_in[1].gl_Position;
    viewSpaceDepth = gl_Position.w;
    periodFraction = cFrac;
    offsetPeriods = v1Frac;
    viewSpacePosition = viewSpacePositions[1];
    EmitVertex();
  }

  EndPrimitive();
}
