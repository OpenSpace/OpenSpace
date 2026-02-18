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
  float positionDepth;
  float fade;
  noperspective vec2 mathLine;
} in_data;

uniform vec3 color;
uniform int renderPhase;
uniform float opacity = 1.0;
uniform float lineWidth;
uniform vec4 viewport;

// Fragile! Keep in sync with RenderableTrail::render::RenderPhase
const int RenderPhaseLines = 0;
const int RenderPhasePoints = 1;

const float Delta = 0.25;


Fragment getFragment() {
  Fragment frag;
  frag.color = vec4(color * in_data.fade, in_data.fade * opacity);
  frag.depth = in_data.positionDepth;
  frag.blend = BlendModeAdditive;

  if (renderPhase == RenderPhasePoints) {
    // Use the length of the vector (dot(circCoord, circCoord)) as factor in the
    // smoothstep to gradually decrease the alpha on the edges of the point
    vec2 circCoord = 2.0 * gl_PointCoord - 1.0;
    float circleClipping = smoothstep(1.0, 1.0 - Delta, dot(circCoord, circCoord));
    frag.color.a *= circleClipping;
  }
  else {
    // We can't expect a viewport of the form (0, 0, res.x, res.y) used to convert the
    // window coordinates from gl_FragCoord into [0, 1] coordinates, so we need to use
    // this more complicated method that is also used in the FXAA and HDR rendering steps
    vec2 xy = vec2(gl_FragCoord.xy);
    xy -= viewport.xy;

    double distanceCenter = length(in_data.mathLine - xy);
    double dLW = double(lineWidth);
    const float BlendFactor = 20.0;

    if (distanceCenter > dLW) {
      frag.color.a = 0.0;
    }
    else {
      frag.color.a *= pow(float((dLW - distanceCenter) / dLW), BlendFactor);
    }
  }

  frag.gPosition = in_data.gPosition;
  frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);
  return frag;
}
