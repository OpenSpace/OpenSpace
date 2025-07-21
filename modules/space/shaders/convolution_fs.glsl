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

in vec2 vs_uv;

out vec4 renderTableColor;

uniform int psfTextureSize;
uniform int convolvedfTextureSize;
uniform sampler2D psfTexture;
uniform sampler2D shapeTexture;


void main() {
  float fullColor = 0.0;

  float maxConvSize = float(psfTextureSize);
  float convStep = 1.0 / maxConvSize;
  float textureStep = 1.0 / float(convolvedfTextureSize);
  for (float i = 0.0, ii = 0.0; i < maxConvSize; i += convStep, ii += textureStep) {
    for (float j = 0.0, jj = 0.0; j < maxConvSize; j += convStep, jj += textureStep) {
      vec2 uv = vs_uv;
      uv.x = clamp(i < 0.5 ? vs_uv.x - ii : vs_uv.x + ii, 0.0, 1.0);
      uv.y = clamp(j < 0.5 ? vs_uv.y - jj : vs_uv.y + jj, 0.0, 1.0);
      fullColor += texture(shapeTexture, uv).x * texture(psfTexture, vec2(i, j)).x;
    }
  }

  renderTableColor = vec4(fullColor / 40.0);
}
