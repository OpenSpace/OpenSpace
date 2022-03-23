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


in vec4 vs_gPosition;
in vec3 vs_gNormal;
in float vs_screenSpaceDepth;
in vec2 vs_st;
in vec4 vs_position;

uniform float crossHairSize;
uniform bool showRectangle;
uniform float lineWidth;
uniform vec2 dimensions;
uniform vec4 lineColor;
uniform float fov;

uniform bool additiveBlending;
uniform float opacity = 1.0;
uniform vec3 multiplyColor;

// A factor which states how much thicker vertical lines are rendered than horizontal
// This compensates for the optical illusion that vertical lines appear thinner
const float VerticalThickness = 1.1;

float createLine(float lineCenter, float lineWidth, float coord) {
  // Calculate edges of line
  float startEdge = lineCenter - (lineWidth * 0.5);
  float endEdge = lineCenter + (lineWidth * 0.5);

  return step(startEdge, coord) - step(endEdge, coord);
}

float createFilledRectangle(float width, float height, vec2 coord) {
  return createLine(0.5, width, coord.x) * createLine(0.5, height, coord.y);
}

float createCrosshair(in float linewidth, in float ratio, in vec2 coord) {
  const float Center = 0.5;
  float crosshairVertical = createLine(Center, linewidth * VerticalThickness, coord.x);
  float crosshairHorizontal = createLine(Center, linewidth, coord.y);

  return crosshairHorizontal + crosshairVertical;
}

#include "fragment.glsl"

Fragment getFragment() {
    float ratio = dimensions.y / dimensions.x;
    float rectangle = 0.0;
    float maxWwtFov = 70;

    float crosshair = createCrosshair(lineWidth, ratio, vs_st);
    float crossHairHeight = crossHairSize/maxWwtFov;
    float crossHairWidth = crossHairHeight * ratio;
    float crossHairBox = createFilledRectangle(crossHairHeight, crossHairWidth, vs_st);
    crosshair *= crossHairBox;

    if (showRectangle) {
      float height = fov/maxWwtFov;
      float width = height * ratio;
      float outerEdge = createFilledRectangle(height, width, vs_st);
      float lineWidthX = lineWidth * 2 * VerticalThickness;
      float lineWidthY = lineWidth * 2;
      float innerEdge = createFilledRectangle(height-lineWidthX, width-lineWidthY, vs_st);
      rectangle = outerEdge - innerEdge;
    }

    float result = clamp(crosshair + rectangle, 0.0, 1.0);

    Fragment frag;
    frag.color = lineColor;
    frag.color.a *= result;

    frag.color.rgb *= multiplyColor;

    frag.color.a *= opacity;
    if (frag.color.a == 0.0) {
        discard;
    }

    frag.depth = vs_screenSpaceDepth;

    if (additiveBlending) {
        frag.blend = BLEND_MODE_ADDITIVE;
    }

    // G-Buffer
    frag.gPosition = vs_gPosition;
    frag.gNormal = vec4(vs_gNormal, 1.0);

    return frag;
}
