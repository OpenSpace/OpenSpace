/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
in vec2 texCoord;

uniform float alphaValue;
uniform vec3 color;

uniform vec4 nanColor = vec4(0.5);
uniform bool useNanColor = true;

uniform sampler2D spriteTexture;

uniform bool useColorMap;
uniform sampler1D colorMapTexture;
uniform float cmapRangeMin;
uniform float cmapRangeMax;
uniform bool hideOutsideRange;

uniform float fadeInValue;

vec4 sampleColorMap(float dataValue) {
    if (useNanColor && isnan(dataValue)) {
        return nanColor;
    }

    bool isOutside = dataValue < cmapRangeMin || dataValue > cmapRangeMax;
    if (hideOutsideRange && isOutside) {
        discard;
    }

    float t = (dataValue - cmapRangeMin) / (cmapRangeMax - cmapRangeMin);
    t = clamp(t, 0.0, 1.0);
    return texture(colorMapTexture, t);
}

Fragment getFragment() {
  if (fadeInValue == 0.0 || alphaValue == 0.0) {
    discard;
  }

  vec4 textureColor = texture(spriteTexture, texCoord);
  if (textureColor.a < 0.01) {
    discard;
  }

  vec4 fullColor = textureColor;

  if (useColorMap) {
    fullColor *= sampleColorMap(gs_colorParameter);
  }
  else {
    fullColor.rgb *= color;
  }

  fullColor.a *= alphaValue * fadeInValue;
  if (fullColor.a < 0.01) {
    discard;
  }

  Fragment frag;
  frag.color = fullColor;
  frag.depth = vs_screenSpaceDepth;
  // Setting the position of the billboards to not interact with the ATM
  frag.gPosition = vec4(-1e32, -1e32, -1e32, 1.0);
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);

  return frag;
}
