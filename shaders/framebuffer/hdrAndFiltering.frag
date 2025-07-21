/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "hdr.glsl"

#define HSV_COLOR 0u
#define HSL_COLOR 1u

in vec2 texCoord;
layout (location = 0) out vec4 finalColor;

uniform float hdrExposure;
uniform float blackoutFactor;
uniform float gamma;
uniform float Hue;
uniform float Saturation;
uniform float Value;
uniform float Lightness;
uniform vec4 Viewport;
uniform vec2 Resolution;

uniform sampler2D hdrFeedingTexture;

void main() {
  // Modify the texCoord based on the Viewport and Resolution. This modification is
  // necessary in case of side-by-side stereo as we only want to access the part of the
  // feeding texture that we are currently responsible for.  Otherwise we would map the
  // entire feeding texture into our half of the result texture, leading to a doubling
  // of the "missing" half.  If you don't believe me, load a configuration file with the
  // side_by_side stereo mode enabled, disable FXAA, and remove this modification.
  // The same calculation is done in the FXAA shader
  vec2 st = texCoord;
  st.x = st.x / (Resolution.x / Viewport[2]) + (Viewport[0] / Resolution.x);
  st.y = st.y / (Resolution.y / Viewport[3]) + (Viewport[1] / Resolution.y);

  vec4 color = texture(hdrFeedingTexture, st);
  color.rgb *= blackoutFactor;

  // Applies TMO
  vec3 tColor = toneMappingOperator(color.rgb, hdrExposure);

  // Color control
  vec3 hsvColor = rgb2hsv(tColor);
  hsvColor.x = (hsvColor.x + Hue);
  if (hsvColor.x > 360.0) {
    hsvColor -= 360.0;
  }
  hsvColor.y = clamp(hsvColor.y * Saturation, 0.0, 1.0);
  hsvColor.z = clamp(hsvColor.z * Value, 0.0, 1.0);

  // Gamma Correction
  finalColor = vec4(gammaCorrection(hsv2rgb(hsvColor), gamma), color.a);
}
