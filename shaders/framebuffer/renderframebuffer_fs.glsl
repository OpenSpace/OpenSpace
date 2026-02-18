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

#include "floatoperations.glsl"
#include <#{fragmentPath}>

layout(location = 0) out vec4 out_color;
layout(location = 1) out vec4 out_gPosition;
layout(location = 2) out vec4 out_gNormal;

#define exposure #{rendererData.hdrExposure}
#define disableHDRPipeline #{rendererData.disableHDR}
const float DeltaError = 0.013;
const float MaxValueColorBuffer = 1E10;


void main() {
  Fragment f = getFragment();

  // Color is already in HDR space
  if (f.disableLDR2HDR || (disableHDRPipeline == 1)) {
    out_color = f.color;
  }
  else {
    out_color = vec4(
      log2(vec3(1.0) - (f.color.rgb - vec3(DeltaError))) / -exposure,
      f.color.a
    );
  }

  out_color.x = isnan(out_color.x) ? MaxValueColorBuffer : out_color.x;
  out_color.y = isnan(out_color.y) ? MaxValueColorBuffer : out_color.y;
  out_color.z = isnan(out_color.z) ? MaxValueColorBuffer : out_color.z;

  out_gPosition = f.gPosition;
  out_gNormal = f.gNormal;

  gl_FragDepth = f.disableDepthNormalization ? f.depth : normalizeFloat(f.depth);
}
