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

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

in vec3 vs_normal;
in vec2 vs_st;
in float vs_depth;

uniform sampler2D baseTexture;
uniform sampler2D projectionTexture;
uniform bool meridianShift;
uniform float ambientBrightness;
uniform float projectionFading;
uniform bool hasBaseMap;
uniform vec4 objpos;
uniform vec3 sun_pos;


Fragment getFragment() {
  vec2 st = vs_st;
  if (meridianShift) {
    st.s += 0.5;
  }

  vec3 n = normalize(vs_normal);
  vec3 l_dir = normalize(sun_pos - objpos.xyz);
  float intensity = min(max(5.0 * dot(n, l_dir), ambientBrightness), 1.0);

  vec4 textureColor = vec4(0.2, 0.2, 0.2, 1.0);
  if (hasBaseMap) {
    textureColor = texture(baseTexture, st);
  }
  vec4 projectionColor = texture(projectionTexture, vs_st);
  if (projectionColor.a != 0.0) {
    textureColor.rgb = mix(
      textureColor.rgb,
      projectionColor.rgb,
      min(projectionFading, projectionColor.a)
    );
  }

  Fragment frag;
  frag.color = max(intensity * textureColor, vec4(0.0, 0.0, 0.0, 1.0));
  frag.depth = vs_depth;

  return frag;
}
