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

#include "floatoperations.glsl"
#include <#{fragmentPath}>

#define exposure #{rendererData.hdrExposure}
#define disableHDRPipeline #{rendererData.disableHDR}
#define DeltaError 0.013f
#define MaxValueColorBuffer 1E10

layout(location = 0) out vec4 _out_color_;
layout(location = 1) out vec4 gPosition;
layout(location = 2) out vec4 gNormal;

void main() {
  Fragment f = getFragment();

  // Color is already in HDR space
  if (f.disableLDR2HDR || (disableHDRPipeline == 1)) {
    _out_color_ = f.color;
  }
  else {
    _out_color_ = vec4((log2(vec3(1.0) - (f.color.rgb - vec3(DeltaError)))/(-exposure)), f.color.a);
  }

  _out_color_.x = isnan(_out_color_.x) ? MaxValueColorBuffer : _out_color_.x;
  _out_color_.y = isnan(_out_color_.y) ? MaxValueColorBuffer : _out_color_.y;
  _out_color_.z = isnan(_out_color_.z) ? MaxValueColorBuffer : _out_color_.z;

  gPosition = f.gPosition;
  gNormal = f.gNormal;

  gl_FragDepth = normalizeFloat(f.depth);
}
