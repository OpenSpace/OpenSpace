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

#include "fragment.glsl"

flat in float gs_colorParameter;
flat in float vs_screenSpaceDepth;
flat in vec4 vs_positionViewSpace;
in vec2 texCoord;
flat in int layer;

uniform float opacity;
uniform vec3 color;

uniform vec4 nanColor = vec4(0.5);
uniform bool useNanColor = true;

uniform vec4 aboveRangeColor;
uniform bool useAboveRangeColor;

uniform vec4 belowRangeColor;
uniform bool useBelowRangeColor;

uniform bool hasSpriteTexture;
uniform sampler2DArray spriteTexture;

uniform bool useColorMap;
uniform sampler1D colorMapTexture;
uniform float cmapRangeMin;
uniform float cmapRangeMax;
uniform bool hideOutsideRange;
uniform bool enableOutline;
uniform vec3 outlineColor;
uniform float outlineWeight;

uniform float fadeInValue;

vec4 sampleColorMap(float dataValue) {
    if (useNanColor && isnan(dataValue)) {
        return nanColor;
    }

    bool isOutside = dataValue < cmapRangeMin || dataValue > cmapRangeMax;
    if (isnan(dataValue) || (hideOutsideRange && isOutside)) {
        discard;
    }

    if (useBelowRangeColor && dataValue < cmapRangeMin) {
        return belowRangeColor;
    }

    if (useAboveRangeColor && dataValue > cmapRangeMax) {
        return aboveRangeColor;
    }

    float t = (dataValue - cmapRangeMin) / (cmapRangeMax - cmapRangeMin);
    t = clamp(t, 0.0, 1.0);
    return texture(colorMapTexture, t);
}

Fragment getFragment() {
  if (fadeInValue == 0.0 || opacity == 0.0) {
    discard;
  }

  // Moving the origin to the center and calculating the length
  float lengthFromCenter = length((texCoord - vec2(0.5)) * 2.0);
  if (!hasSpriteTexture && (lengthFromCenter > 1.0)) {
    discard;
  }

  vec4 fullColor = vec4(color, 1.0);
  if (useColorMap) {
    fullColor = sampleColorMap(gs_colorParameter);
  }

  if (hasSpriteTexture) {
    fullColor *= texture(spriteTexture, vec3(texCoord, layer));
  }
  else if (enableOutline && (lengthFromCenter > (1.0 - outlineWeight))) {
    fullColor.rgb = outlineColor;
  }

  fullColor.a *= opacity * fadeInValue;
  if (fullColor.a < 0.01) {
    discard;
  }

  Fragment frag;
  frag.color = fullColor;
  frag.depth = vs_screenSpaceDepth;
  frag.gPosition = vs_positionViewSpace;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);

  return frag;
}
