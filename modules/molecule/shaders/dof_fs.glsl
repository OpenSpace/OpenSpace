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

#version __CONTEXT__

#pragma optionNV(unroll all)

// From http://blog.tuxedolabs.com/2018/05/04/bokeh-depth-of-field-in-single-pass.html

uniform sampler2D u_tex_half_res; // Half res color (rgb) and coc (a)
uniform sampler2D u_tex_color;    // Image to be processed
uniform sampler2D u_tex_depth;    // Linear depth

uniform vec2  u_texel_size;    // The size of a pixel: vec2(1.0/width, 1.0/height)
uniform float u_focus_depth;
uniform float u_focus_scale;
uniform float u_time;

const float GOLDEN_ANGLE = 2.39996323;
const float MAX_BLUR_SIZE = 15.0;
const float RAD_SCALE = 1.0; // Smaller = nicer blur, larger = faster
const float PI = 3.1415926535;

#define APPROX

float rand(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233))) * 43758.5453);
}

vec4 rand4( vec2 n ) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233))) * vec4(43758.5453, 28001.8384, 50849.4141, 12996.89));
}

float srand(vec2 n) {
  return rand(n) * 2.0 - 1.0;
}

vec4 srand4(vec2 n) {
  return rand4(n) * 2.0 - 1.0;
}

float get_blur_size(float depth, float focusPoint, float focusScale) {
  float coc = clamp((1.0 / focusPoint - 1.0 / depth)*focusScale, -1.0, 1.0);
  return abs(coc) * MAX_BLUR_SIZE;
}

vec3 depth_of_field(vec2 tex_coord, float focus_point, float focus_scale) {
  float center_depth = texture(u_tex_depth, tex_coord).r;
  vec3 center_color  = texture(u_tex_color, tex_coord).rgb;
  float center_coc = get_blur_size(center_depth, focus_point, focus_scale);
  vec4 color_coc_sum = vec4(center_color, center_coc);

  //vec4 noise = srand4(tex_coord + vec2(u_time, u_time) + 0.6959174) * 3.141259265 * 2.0;

  float contrib_sum = 1.0;
  float radius = RAD_SCALE;
  float ang = 0.0; // noise.z * PI;

  for (; radius < MAX_BLUR_SIZE; ang += GOLDEN_ANGLE) {
    vec2 tc = tex_coord + vec2(cos(ang), sin(ang)) * u_texel_size * radius;
    float sample_depth = texture(u_tex_depth, tc).r;
    vec3 sample_color = texture(u_tex_color, tc).rgb;
    float sample_coc = get_blur_size(sample_depth, focus_point, focus_scale);
    if (sample_depth > center_depth) {
      sample_coc = min(sample_coc, center_coc * 2.0);
    }

    vec4 sample_color_coc = vec4(sample_color, sample_coc);

    color_coc_sum += mix(
      color_coc_sum / contrib_sum,
      sample_color_coc,
      smoothstep(radius - 0.5, radius + 0.5, sample_color_coc.a)
    );
    contrib_sum += 1.0;
    radius += RAD_SCALE/radius;

#ifdef APPROX
    if (color_coc_sum.a / contrib_sum > 1.0) {
      break;
    }
    //if (radius > MAX_BLUR_SIZE * 0.15) break;
#endif
  }

#ifdef APPROX
  const float HALF_RES_RAD_SCALE = 2.0;
  for (; radius < MAX_BLUR_SIZE; ang += GOLDEN_ANGLE) {
    vec2 tc = tex_coord + vec2(cos(ang), sin(ang)) * u_texel_size * radius;
    vec4 sample_color_coc = texture(u_tex_half_res, tc) * vec4(1, 1, 1, MAX_BLUR_SIZE);

    float sample_depth = texture(u_tex_depth, tc).r;
    if (sample_depth > center_depth) {
      sample_color_coc.a = min(sample_color_coc.a, center_coc * 2.0);
    }

    color_coc_sum += mix(
      color_coc_sum / contrib_sum,
      sample_color_coc,
      smoothstep(radius-0.5, radius+0.5, sample_color_coc.a)
    );
    contrib_sum += 1.0;
    radius += HALF_RES_RAD_SCALE/radius;
  }
#endif
  return color_coc_sum.rgb /= contrib_sum;
}

in vec2 tc;
out vec4 out_frag;

void main() {
  vec3 dof = depth_of_field(tc, u_focus_depth, u_focus_scale);

  // To hide banding artifacts
  vec4 noise = srand4(tc + u_time + 0.6959174) / 15.0;
  dof += noise.xyz;
  out_frag = vec4(dof, 1.0);
}
