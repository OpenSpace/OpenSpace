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

#include "fragment.glsl"

in Data {
  vec4 gPosition;
  vec3 gNormal;
  vec2 texCoord;
  float screenSpaceDepth;
} in_data;

uniform float crossHairSize;
uniform bool showRectangle;
uniform float lineWidth;
uniform float ratio;
uniform vec4 lineColor;
uniform float fov;
uniform float borderRadius;

uniform bool additiveBlending;
uniform float opacity;
uniform vec3 multiplyColor;

// A factor which states how much thicker vertical lines are rendered than horizontal
// This compensates for the optical illusion that vertical lines appear thinner
const float VerticalThickness = 1.1;


float createLine(float lineCenter, float lineWidth, float coord) {
  float startEdge = lineCenter - (lineWidth * 0.5);
  float endEdge = lineCenter + (lineWidth * 0.5);
  return step(startEdge, coord) - step(endEdge, coord);
}

float createFilledRectangle(float width, float height, vec2 coord) {
  return createLine(0.5, width, coord.x) * createLine(0.5, height, coord.y);
}

float createCrosshair(float linewidth, float ratio, vec2 coord) {
  const float Center = 0.5;
  float crosshairVertical = createLine(Center, linewidth * VerticalThickness, coord.x);
  float crosshairHorizontal = createLine(Center, linewidth, coord.y);
  return crosshairHorizontal + crosshairVertical;
}

// Creates a rounded rectangle where radius is [0, 1] from completely square
// to completely round
float roundedRectangle(vec2 coord, vec2 size, float radius) {
  vec2 q = abs(coord) - size + vec2(radius);
  return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - radius;
}

Fragment getFragment() {
  float rectangle = 0.0;
  const float MaxWwtFov = 70;

  float crosshair = createCrosshair(lineWidth, ratio, in_data.texCoord);
  float crossHairHeight = crossHairSize / MaxWwtFov;
  float crossHairWidth = crossHairHeight * ratio;
  float crossHairBox = createFilledRectangle(
    crossHairWidth,
    crossHairHeight,
    in_data.texCoord
  );
  crosshair *= crossHairBox;

  if (showRectangle) {
    float lineWidthY = lineWidth * 2.0;
    float lineWidthX = lineWidth * 2.0 * VerticalThickness;
    float height = (fov * 0.5) / MaxWwtFov - lineWidth;
    float width = (height * ratio) - lineWidthY;
    vec2 size = vec2(width, height);

    // The radius of the corners (in pixels) clockwise starting in the top left
    float radius = clamp(
      borderRadius * 0.5 * min(height, width),
      0.0,
      0.5 * min(height, width)
    );

    // Calculate distance to edge
    float distance = roundedRectangle(
      in_data.texCoord.st - vec2(0.5),
      size * 0.5,
      radius
    );

    // How soft the edges should be (in pixels)
    // Higher values could be used to simulate a drop shadow
    float edgeSoftness = 2.0;
    // Smooth the result (free antialiasing)
    float smoothedAlpha = 1.0 - smoothstep(0.0, edgeSoftness, distance);

    // Border
    float borderThickness = lineWidth * 0.5;
    const float BorderSoftness = 0.0;
    float borderAlpha =
      1.0 - smoothstep(borderThickness - BorderSoftness, borderThickness, abs(distance));

    // Colors
    const float BorderColor = 1.0;
    const float BgColor = 0.0;

    rectangle = mix(BgColor, mix(BgColor, BorderColor, borderAlpha), smoothedAlpha);
  }

  float result = clamp(crosshair + rectangle, 0.0, 1.0);

  float alpha = lineColor.a * result * opacity;
  if (alpha == 0.0) {
    discard;
  }

  Fragment frag;
  frag.color = vec4(lineColor.rgb * multiplyColor, alpha);
  frag.depth = in_data.screenSpaceDepth;
  if (additiveBlending) {
    frag.blend = BlendModeAdditive;
  }
  frag.gPosition = in_data.gPosition;
  frag.gNormal = vec4(in_data.gNormal, 1.0);
  return frag;
}
