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

in vec2 texCoord;
layout (location = 0) out vec4 finalColor;

uniform sampler2D downscaledRenderedVolume;
uniform sampler2D downscaledRenderedVolumeDepth;
uniform vec4 viewport;
uniform vec2 resolution;

void main() {
  // Modify the texCoord based on the Viewport and Resolution. This modification is
  // necessary in case of side-by-side stereo as we only want to access the part of the
  // feeding texture that we are currently responsible for.  Otherwise we would map the
  // entire feeding texture into our half of the result texture, leading to a doubling of
  // the "missing" half.  If you don't believe me, load a configuration file with the
  // side_by_side stereo mode enabled, disable FXAA, and remove this modification.
  // The same calculation is done in the HDR resolving shader
  vec2 st = texCoord;
  st.x = st.x / (resolution.x / viewport[2]) + (viewport[0] / resolution.x);
  st.y = st.y / (resolution.y / viewport[3]) + (viewport[1] / resolution.y);

  finalColor = texture(downscaledRenderedVolume, st);
  gl_FragDepth = texture(downscaledRenderedVolumeDepth, st).r;
}
