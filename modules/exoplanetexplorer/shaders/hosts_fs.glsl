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

const int MaxColors = 4;

in Data {
  flat float component;
  float depthClipSpace;
  vec4 positionViewSpace;
  flat int glyphIndex;
  flat vec4 color;
  vec2 texCoords; // [-1, 1]
} in_data;

uniform float opacity;
uniform bool onTop;

uniform int maxIndex;
uniform int currentIndex;
uniform bool isRenderIndexStep = false;
uniform bool isHighlightMode = false;
uniform float darkenFactor;
uniform bool shouldBlur;

const float M_PI = 3.141592657;

Fragment getFragment() {
  Fragment frag;
  frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);
  frag.depth = in_data.depthClipSpace;

  vec4 color = in_data.color;

  float radius = length(in_data.texCoords);
  float x = in_data.texCoords.x;
  float y = in_data.texCoords.y;

  float component = in_data.component;

  // Render selection icon
  if (onTop && radius > 1.0 && (abs(x - y) < 0.2 || abs(-1.0 * x - y) < 0.2)) {
    frag.color = vec4(1.0);
    return frag;
  }

  if (onTop || radius > 1.0) {
    discard;
  }

  // Gaussian/star-like alpha fade from center to edge of the point sprite
  if (shouldBlur) {
    const float sigma = 0.45;
    float gaussianFade = exp(-(radius * radius) / (2.0 * sigma * sigma));
    float coreGlow = exp(-(radius * radius) / (2.0 * (sigma * 0.28) * (sigma * 0.28)));

    color.rgb *= gaussianFade;         // smooth outer fade
    color.rgb += vec3(0.2) * coreGlow; // brighter center point
    color.a *= gaussianFade;
  }

  color.a *= opacity;

  // If the glyph is not visible due to too transparent, don't include this pixel
  if (color.a < 0.05) {
    discard;
  }

  // Render glyph index if we are at that rendering step
  if (isRenderIndexStep) {
    float index = float(in_data.glyphIndex) / float(maxIndex);
    color = vec4(vec3(index), 1.0);
    // Set to render the value as is, without any color adjustments
    frag.disableLDR2HDR = true;
  }
  else {
    bool isCurrentHoveredGlyph = in_data.glyphIndex == currentIndex;

    // Ring border
    float borderWidth = isCurrentHoveredGlyph ? 0.2: 0.13;
    if (radius > 1.0 - borderWidth) {
      color.rgb = isCurrentHoveredGlyph ? color.rgb : vec3(0.0);
    }

    // Brighten hovered glyph
    if (isCurrentHoveredGlyph) {
      color.rgb *= 2.5;
    }

    // When in higlight mode, also dim all other glyphs
    if (isHighlightMode && !isCurrentHoveredGlyph) {
      color.rgb *= darkenFactor;
    }
  }

  frag.color = color;

  return frag;
}
