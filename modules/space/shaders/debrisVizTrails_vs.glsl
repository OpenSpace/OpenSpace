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

#include "PowerScaling/powerScalingMath.hglsl"

layout (location = 0) in vec4 vertexData; // 1: x, 2: y, 3: z, 4: timeOffset,
layout (location = 1) in vec2 orbitData; // 1: epoch, 2: period

out vec4 viewSpacePosition;
out float viewSpaceDepth;
out float periodFraction;
out float offsetPeriods;

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform double inGameTime;


void main() {
  // The way the position and trail fade is calculated is:
  // By using inGameTime, epoch and period of this orbit, we get how many revolutions it
  // has done since epoch. The fract of that, is how far into a revolution it has traveled
  // since epoch. Similarly we do the same but for this vertex, but calculating
  // offsetPeriods. In the fragment shader the difference between periodFraction_f and
  // offsetPeriods is calculated to know how much to fade that specific fragment.

  // If orbit_data is doubles, cast to float first
  float epoch = orbitData.x;
  float period = orbitData.y;

  // calculate nr of periods, get fractional part to know where the vertex closest to the
  // debris part is right now
  double nrOfRevolutions = (inGameTime - epoch) / period;
  double frac = double(int(nrOfRevolutions));
  double periodFractiond = nrOfRevolutions - frac;
  if (periodFractiond < 0.0) {
    periodFractiond += 1.0;
  }
  periodFraction = float(periodFractiond);

  // same procedure for the current vertex
  offsetPeriods = vertexData.w / float(period);

  viewSpacePosition = vec4(modelViewTransform * dvec4(vertexData.xyz, 1));
  vec4 vs_position = z_normalization(projectionTransform * viewSpacePosition);
  gl_Position = vs_position;
  viewSpaceDepth = vs_position.w;
}
