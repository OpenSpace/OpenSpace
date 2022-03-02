/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

in vec2 vs_st;
in vec4 vs_position;

uniform float lineWidth;
uniform vec2 dimensions;
uniform bool showCrosshair;
uniform bool showRectangle;
uniform vec4 lineColor;

// A factor which states how much thicker vertical lines are rendered than horizontal
// This compensates for the optical illusion that vertical lines appear thinner
const float VerticalThickness = 1.15;

float createLine(float lineCenter, float lineWidth, float coord) {
  // Calculate edges of line
  float startEdge = lineCenter - (lineWidth * 0.5);
  float endEdge = lineCenter + (lineWidth * 0.5);

  return step(startEdge, coord) - step(endEdge, coord);
}

float createRectangle(float linewidthY, float ratio, vec2 coord) {
  // Calculate the widths and centers for the lines
  float linewidthX = linewidthY * ratio * VerticalThickness;
  float linecenterX = linewidthX * 0.5;
  float linecenterY = linewidthY * 0.5;

  // Create the four lines for the rectangle
  float l = createLine(linecenterX, linewidthX, coord.x);
  float r = createLine(1.0 - linecenterX, linewidthX, coord.x);
  float b = createLine(linecenterY, linewidthY, coord.y);
  float t = createLine(1.0 - linecenterY, linewidthY, coord.y);

  return l + r + b + t;
}

float createCrosshair(in float linewidth, in float ratio, in vec2 coord) {
  const float Center = 0.5;
  float crosshairVertical = createLine(Center, linewidth * ratio * VerticalThickness, coord.x);
  float crosshairHorizontal = createLine(Center, linewidth, coord.y);

  return crosshairHorizontal + crosshairVertical;
}

#include "fragment.glsl"

Fragment getFragment() {
    float ratio = dimensions.y / dimensions.x;
    float crosshair = 0.0;
    float rectangle = 0.0;

    if (showCrosshair) {
      crosshair = createCrosshair(lineWidth, ratio, vs_st);
      float border = 1.0 - createRectangle(lineWidth * 5.0, ratio, vs_st);
      crosshair *= border;
    }

    if (showRectangle) {
      rectangle = createRectangle(lineWidth, ratio, vs_st);
    }

    float result = clamp(crosshair + rectangle, 0.0, 1.0);

    Fragment frag;
    frag.color = lineColor;
    frag.color.a *= result;

    return frag;
}
