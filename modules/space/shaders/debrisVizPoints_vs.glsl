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

layout (location = 0) in vec4 vertexData; // 1: x, 2: y, 3: z, 4: timeOffset,
layout (location = 1) in vec2 orbitData; // 1: epoch, 2: period

uniform double inGameTime;

flat out float currentRevolutionFraction;
flat out float vertexRevolutionFraction;

void main() {
  float epoch = orbitData.x;
  float period = orbitData.y;

  // calculate nr of periods, get fractional part to know where the vertex closest to the
  // debris part is right now
  double numOfRevolutions = (inGameTime - epoch) / period;
  currentRevolutionFraction = float(numOfRevolutions - double(int(numOfRevolutions)));
  if (currentRevolutionFraction < 0.0) {
    currentRevolutionFraction += 1.0;
  }

  // Same procedure for the current vertex
  vertexRevolutionFraction = vertexData.w / period;

  gl_Position = vec4(vertexData.xyz, 1.0);
}
