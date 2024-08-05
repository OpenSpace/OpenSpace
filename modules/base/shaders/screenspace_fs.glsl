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
#include "hdr.glsl"
#include "PowerScaling/powerScaling_fs.hglsl"

in vec2 vs_st;
in float vs_depth;

uniform sampler2D tex;
uniform vec3 color = vec3(1.0);
uniform float opacity = 1.0;
uniform float blackoutFactor = 1.0;
uniform vec4 backgroundColor = vec4(0.0);
uniform float hue;
uniform float value;
uniform float saturation;
uniform float gamma = 1.0;
uniform vec2 borderWidth = vec2(0.1);
uniform vec3 borderColor = vec3(0.0);


Fragment getFragment() {
  Fragment frag;

  vec4 texColor = texture(tex, vs_st) * vec4(color, opacity);

  frag.color = texColor.a * texColor + (1.0 - texColor.a) * backgroundColor;

  // Set border color
  if (vs_st.x < borderWidth.x || vs_st.x > 1 - borderWidth.x ||
      vs_st.y < borderWidth.y || vs_st.y > 1 - borderWidth.y)
  {
    frag.color = vec4(borderColor, opacity);
  }

  if (frag.color.a == 0.0) {
    discard;
  }

  frag.depth = vs_depth;

  vec3 hsvColor = rgb2hsv(frag.color.rgb);
  hsvColor.x = (hsvColor.x + hue);
  if (hsvColor.x > 360.0) {
    hsvColor -= 360.0;
  }
  hsvColor.y = clamp(hsvColor.y * saturation, 0.0, 1.0);
  hsvColor.z = clamp(hsvColor.z * value, 0.0, 1.0);

  frag.color.rgb = gammaCorrection(hsv2rgb(hsvColor), gamma) * blackoutFactor;
  return frag;
}
