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

const int MaxColors = 8;

in Data {
  flat float component;
  float depthClipSpace;
  vec4 positionViewSpace;
  flat int glyphIndex;
  flat int nColors;
  flat vec4 colors[MaxColors];
  vec2 texCoord; // [-1, 1]
  float sizeFactor; // The factor used for the radius of the ring
} in_data;

uniform float opacity;
uniform bool onTop;
uniform bool useFixedRingWidth;

uniform int maxIndex;
uniform int currentIndex;
uniform bool isRenderIndexStep = false;

const float M_PI = 3.141592657;

Fragment getFragment() {
  float radius = length(in_data.texCoord);

  float x = in_data.texCoord.x;
  float y = in_data.texCoord.y;

  // Render selection icon
  if (onTop && radius > 1.0 && (abs(x - y) < 0.2 || abs(-1.0 * x - y) < 0.2)) {
    Fragment frag;
    frag.color = vec4(1.0);
    frag.depth = in_data.depthClipSpace;
    frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);
    return frag;
  }

  if (onTop || radius > 1.0) {
    discard;
  }

  float maxRadius = 1.0;

  // Same width
  float width = 0.95 * 1.0 / in_data.component; // Make it a little smaller than full width

  if (!useFixedRingWidth) {
//    // Same area: Compute width from radius relationship instead
//    float underRoot = 1.0 - 1.0 / in_data.component;
//    // Prevent problems with potentially taking square of negative value
//    if (underRoot < 0.0) {
//      underRoot = 0.0;
//    }
//    width = 1.0 - sqrt(underRoot); // r_n minus r_(n-1) divided by r_n

    // Ish 90% width of previous ring
    width = pow(0.87, in_data.component) / in_data.sizeFactor;
  }

  float minRadius = 1.0 - width;

  float coord = (radius - minRadius) / (maxRadius - minRadius);
  if (coord < 0.0 || coord > 1.0) {
    discard;
  }

  // Find what color corresponds to the given angle
  float angleSlice = 2.0 * M_PI / in_data.nColors;
  vec2 up = vec2(0.0, 1.0);
  float angle = acos(dot(up, normalize(in_data.texCoord)));

  // left half of circle quadrant
  if (x < 0) {
    angle = M_PI + (M_PI - angle);
  }

  int colorIndex = int(floor(angle / angleSlice));

  vec4 color = in_data.colors[colorIndex];
  color.a *= opacity;

  // If the glyph is not visible due to too transparent, don't include this pixel
  if (color.a <  0.05) {
    discard;
  }

  Fragment frag;

  // Render glyph index if we are at that rendering step
  if (isRenderIndexStep) {
    color.rgb = vec3(float(in_data.glyphIndex) / float(maxIndex));
    color.a = 1.0;
    // Set to render the value as is, without any color adjustments
    frag.disableLDR2HDR = true;
  }

  bool isCurrentHoveredGlyph = in_data.glyphIndex == currentIndex && !isRenderIndexStep;
  float borderWidth = isCurrentHoveredGlyph ? 0.20: 0.13;
  if (coord > 1.0 - borderWidth || coord < 1.0 - 1.0 + borderWidth) {
    if (isCurrentHoveredGlyph) {
      color.rgb *= 2.5; // brighter border
    }
    else {
      color.rgb = vec3(0.0, 0.0, 0.0); //black border
    }
  }

  if (isCurrentHoveredGlyph) {
    // Brighten glyph
    color.rgb *= 1.5;
  }

  frag.color = color;
  frag.depth = in_data.depthClipSpace;
  frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);

  return frag;
}
