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

#extension GL_ARB_explicit_attrib_location : enable
#pragma optionNV(unroll all)

// This is ported straight from the implementation given by playdeadgames (MIT License)
// https://github.com/playdeadgames/temporal/blob/master/Assets/Shaders/TemporalReprojection.shader


#ifndef UNJITTER_COLORSAMPLES
#define UNJITTER_COLORSAMPLES 1
#endif
#ifndef UNJITTER_NEIGHBORHOOD
#define UNJITTER_NEIGHBORHOOD 0
#endif
#ifndef UNJITTER_REPROJECTION
#define UNJITTER_REPROJECTION 0
#endif
#ifndef UNJITTER_PREV_SAMPLES
#define UNJITTER_PREV_SAMPLES 0
#endif

#ifndef USE_YCOCG
#define USE_YCOCG 1
#endif
#ifndef USE_CLIPPING
#define USE_CLIPPING 1
#endif
#ifndef USE_DILATION
#define USE_DILATION 1
#endif

#define USE_MOTION_BLUR #{UseMotionBlur}

#ifndef USE_MOTION_BLUR_NEIGHBORMAX
#define USE_MOTION_BLUR_NEIGHBORMAX 1
#endif
#ifndef USE_OPTIMIZATIONS
#define USE_OPTIMIZATIONS 1
#endif

#ifndef MINMAX_3X3
#define MINMAX_3X3 0
#endif
#ifndef MINMAX_3X3_ROUNDED
#define MINMAX_3X3_ROUNDED 1
#endif
#ifndef MINMAX_4TAP_VARYING
#define MINMAX_4TAP_VARYING 0
#endif

uniform sampler2D u_tex_main;
uniform sampler2D u_tex_prev;
uniform sampler2D u_tex_linear_depth;
uniform sampler2D u_tex_vel;
uniform sampler2D u_tex_vel_neighbormax;

uniform vec4 u_texel_size;
uniform vec4 u_jitter_uv;

uniform float u_time;
uniform float u_feedback_min = 0.88;
uniform float u_feedback_max = 0.97;
uniform float u_motion_scale = 0.1;

const float FLT_EPS = 0.00000001f;

// https://software.intel.com/en-us/node/503873
vec3 rgb_to_ycocg(vec3 c) {
  // Y = R/4 + G/2 + B/4
  // Co = R/2 - B/2
  // Cg = -R/4 + G/2 - B/4
  return vec3(
    c.x / 4.0 + c.y / 2.0 + c.z / 4.0,
    c.x / 2.0 - c.z / 2.0,
    -c.x / 4.0 + c.y / 2.0 - c.z / 4.0
  );
}

// https://software.intel.com/en-us/node/503873
vec3 ycocg_to_rgb(vec3 c) {
  // R = Y + Co - Cg
  // G = Y + Cg
  // B = Y - Co - Cg
  return vec3(
    c.x + c.y - c.z,
    c.x + c.z,
    c.x - c.y - c.z
  );
}

float luminance(vec3 c) {
  const vec3 w = vec3(0.2125, 0.7154, 0.0721);
  return dot(c, w);
}

float PDnrand(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233)))* 43758.5453);
}
vec2 PDnrand2(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233)))* vec2(43758.5453, 28001.8384));
}
vec3 PDnrand3(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233)))* vec3(43758.5453, 28001.8384, 50849.4141 ));
}
vec4 PDnrand4(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233)))* vec4(43758.5453, 28001.8384, 50849.4141, 12996.89));
}

float PDsrand(vec2 n) {
  return PDnrand(n) * 2.0 - 1.0;
}
vec2 PDsrand2(vec2 n) {
  return PDnrand2(n) * 2.0 - 1.0;
}
vec3 PDsrand3(vec2 n) {
  return PDnrand3(n) * 2.0 - 1.0;
}
vec4 PDsrand4(vec2 n) {
  return PDnrand4(n) * 2.0 - 1.0;
}

float depth_sample_linear(vec2 uv) {
  return texture(u_tex_linear_depth, uv).x;
}

vec2 sample_velocity(vec2 uv) {
  return texture(u_tex_vel, uv).xy;
}

vec3 find_closest_fragment_3x3(vec2 uv) {
  vec2 dd = abs(u_texel_size.xy);
  vec2 du = vec2(dd.x, 0.0);
  vec2 dv = vec2(0.0, dd.y);

  vec3 dtl = vec3(-1, -1, texture(u_tex_linear_depth, uv - dv - du).x);
  vec3 dtc = vec3( 0, -1, texture(u_tex_linear_depth, uv - dv).x);
  vec3 dtr = vec3( 1, -1, texture(u_tex_linear_depth, uv - dv + du).x);

  vec3 dml = vec3(-1, 0, texture(u_tex_linear_depth, uv - du).x);
  vec3 dmc = vec3( 0, 0, texture(u_tex_linear_depth, uv).x);
  vec3 dmr = vec3( 1, 0, texture(u_tex_linear_depth, uv + du).x);

  vec3 dbl = vec3(-1, 1, texture(u_tex_linear_depth, uv + dv - du).x);
  vec3 dbc = vec3( 0, 1, texture(u_tex_linear_depth, uv + dv).x);
  vec3 dbr = vec3( 1, 1, texture(u_tex_linear_depth, uv + dv + du).x);

  vec3 dmin = dtl;
  if (dmin.z > dtc.z) {
    dmin = dtc;
  }
  if (dmin.z > dtr.z) {
    dmin = dtr;
  }

  if (dmin.z > dml.z) {
    dmin = dml;
  }
  if (dmin.z > dmc.z) {
    dmin = dmc;
  }
  if (dmin.z > dmr.z) {
    dmin = dmr;
  }

  if (dmin.z > dbl.z) {
    dmin = dbl;
  }
  if (dmin.z > dbc.z) {
    dmin = dbc;
  }
  if (dmin.z > dbr.z) {
    dmin = dbr;
  }

  return vec3(uv + dd.xy * dmin.xy, dmin.z);
}

vec4 sample_color(sampler2D tex, vec2 uv) {
#if USE_YCOCG
  vec4 c = texture(tex, uv);
  return vec4(rgb_to_ycocg(c.rgb), c.a);
#else
  return texture(tex, uv);
#endif
}

vec4 resolve_color(vec4 c) {
#if USE_YCOCG
  return vec4(ycocg_to_rgb(c.rgb).rgb, c.a);
#else
  return c;
#endif
}

vec4 clip_aabb(vec3 aabb_min, vec3 aabb_max, vec4 p, vec4 q) {
#if USE_OPTIMIZATIONS
  // note: only clips towards aabb center (but fast!)
  vec3 p_clip = 0.5 * (aabb_max + aabb_min);
  vec3 e_clip = 0.5 * (aabb_max - aabb_min) + FLT_EPS;

  vec4 v_clip = q - vec4(p_clip, p.w);
  vec3 v_unit = v_clip.xyz / e_clip;
  vec3 a_unit = abs(v_unit);
  float ma_unit = max(a_unit.x, max(a_unit.y, a_unit.z));

  if (ma_unit > 1.0) {
    return vec4(p_clip, p.w) + v_clip / ma_unit;
  }
  else {
    // point inside aabb
    return q;
  }
#else
  vec4 r = q - p;
  vec3 rmax = aabb_max - p.xyz;
  vec3 rmin = aabb_min - p.xyz;

  const float eps = FLT_EPS;

  if (r.x > rmax.x + eps) {
    r *= (rmax.x / r.x);
  }
  if (r.y > rmax.y + eps) {
    r *= (rmax.y / r.y);
  }
  if (r.z > rmax.z + eps) {
    r *= (rmax.z / r.z);
  }

  if (r.x < rmin.x - eps) {
    r *= (rmin.x / r.x);
  }
  if (r.y < rmin.y - eps) {
    r *= (rmin.y / r.y);
  }
  if (r.z < rmin.z - eps) {
    r *= (rmin.z / r.z);
  }

	return p + r;
#endif
}

vec2 sample_velocity_dilated(sampler2D tex, vec2 uv, int support) {
  vec2 du = vec2(u_texel_size.x, 0.0);
  vec2 dv = vec2(0.0, u_texel_size.y);
  vec2 mv = vec2(0.0);
  float rmv = 0.0;

  int end = support + 1;
  for (int i = -support; i != end; i++) {
    for (int j = -support; j != end; j++) {
      vec2 v = texture(tex, uv + i * dv + j * du).xy;
      float rv = dot(v, v);
      if (rv > rmv) {
        mv = v;
        rmv = rv;
      }
    }
  }

  return mv;
}

vec4 sample_color_motion(sampler2D tex, vec2 uv, vec2 ss_vel) {
  const int taps = 3;// on either side!
  vec2 v = 0.5 * ss_vel;

  float srand = PDsrand(uv + vec2(u_time, u_time));
  vec2 vtap = v / taps;
  vec2 pos0 = uv + vtap * (0.5 * srand);
  vec4 accu = sample_color(tex, uv);
  float wsum = 1.0;

  for (int i = -taps; i <= taps; i++) {
    vec2 uv = pos0 + i * vtap;
    //float w = 1.0;// box
    float w = taps - abs(i) + 1;// triangle
    //float w = 1.0 / (1 + abs(i));// pointy triangle
    accu += w * sample_color(tex, pos0 + i * vtap);
    wsum += w;
  }

  return accu / wsum;
}

// McGuire2012Blur
float cone(float d, float t) {
  return clamp(1.0 - d/t, 0.0, 1.0);
}

float cylinder(float d, float t) {
  return 1.0 - smoothstep(0.95*t, 1.05*t, d);
}

float soft_depth_compare(float z_a, float z_b) {
  const float SOFT_Z_EXTENT = -0.05;
  return clamp(1.0 - (z_a - z_b) / SOFT_Z_EXTENT, 0.0, 1.0);
}

vec2 round_to_nearest(vec2 uv) {
  return ivec2(uv * u_texel_size.zw) * u_texel_size.xy;
}

vec4 reconstruct_color(sampler2D color_tex, vec2 uv0, vec2 ss_vel_max) {
  const int taps = 3;// on either side!

  if (length(ss_vel_max) < FLT_EPS) {
    return sample_color(color_tex, uv0);
  }

  float srand = PDsrand(uv0 + vec2(u_time, u_time));
  vec2 vtap = 0.5 * ss_vel_max / taps;
  vec2 p0 = uv0 + vtap * (0.5 * srand);
  float d0 = depth_sample_linear(p0);
  float v0 = length(sample_velocity(p0));
  float w = 1.0;
  float wsum = w;
  vec4 accu = w * sample_color(color_tex, p0);

  for (int i = 1; i <= taps; i++) {
    vec2 uv = p0 - i * vtap; // Round to nearest?
    float d = depth_sample_linear(uv);
    float v = length(sample_velocity(uv));
    float f = soft_depth_compare(d0, d);
    float b = soft_depth_compare(d, d0);
    float duv = length(uv0 - uv);

    w = f * cone(duv, v) + b * cone(duv, v0) + cylinder(duv, v) * cylinder(duv, v0) * 2.0;
    wsum += w;
    accu += w * sample_color(color_tex, uv);
  }

  for (int i = 1; i <= taps; i++) {
    vec2 uv = p0 + i * vtap; // Round to nearest?
    float d = depth_sample_linear(uv);
    float v = length(sample_velocity(uv));
    float f = soft_depth_compare(d0, d);
    float b = soft_depth_compare(d, d0);
    float duv = length(uv0 - uv);

    w = f * cone(duv, v) + b * cone(duv, v0) + cylinder(duv, v) * cylinder(duv, v0) * 2.0;
    wsum += w;
    accu += w * sample_color(color_tex, uv);
  }

  return accu / wsum;
}

vec4 temporal_reprojection(vec2 ss_txc, vec2 ss_vel, float vs_dist) {
  // read texels
#if UNJITTER_COLORSAMPLES
  vec4 texel0 = sample_color(u_tex_main, ss_txc - u_jitter_uv.xy);
#else
  vec4 texel0 = sample_color(u_tex_main, ss_txc);
#endif

#if UNJITTER_PREV_SAMPLES
  vec4 texel1 = sample_color(u_tex_prev, ss_txc - u_jitter_uv.zw - ss_vel);
#else
  vec4 texel1 = sample_color(u_tex_prev, ss_txc - ss_vel);
#endif

  // calc min-max of current neighbourhood
#if UNJITTER_NEIGHBORHOOD
  vec2 uv = ss_txc - u_jitter_uv.xy;
#else
  vec2 uv = ss_txc;
#endif

#if MINMAX_3X3 || MINMAX_3X3_ROUNDED
  vec2 du = vec2(u_texel_size.x, 0.0);
  vec2 dv = vec2(0.0, u_texel_size.y);

  vec4 ctl = sample_color(u_tex_main, uv - dv - du);
  vec4 ctc = sample_color(u_tex_main, uv - dv);
  vec4 ctr = sample_color(u_tex_main, uv - dv + du);
  vec4 cml = sample_color(u_tex_main, uv - du);
  vec4 cmc = sample_color(u_tex_main, uv);
  vec4 cmr = sample_color(u_tex_main, uv + du);
  vec4 cbl = sample_color(u_tex_main, uv + dv - du);
  vec4 cbc = sample_color(u_tex_main, uv + dv);
  vec4 cbr = sample_color(u_tex_main, uv + dv + du);

  vec4 cmin = min(ctl, min(ctc, min(ctr, min(cml, min(cmc, min(cmr, min(cbl, min(cbc, cbr))))))));
  vec4 cmax = max(ctl, max(ctc, max(ctr, max(cml, max(cmc, max(cmr, max(cbl, max(cbc, cbr))))))));

#if MINMAX_3X3_ROUNDED || USE_YCOCG || USE_CLIPPING
  vec4 cavg = (ctl + ctc + ctr + cml + cmc + cmr + cbl + cbc + cbr) / 9.0;
#endif

#if MINMAX_3X3_ROUNDED
  vec4 cmin5 = min(ctc, min(cml, min(cmc, min(cmr, cbc))));
  vec4 cmax5 = max(ctc, max(cml, max(cmc, max(cmr, cbc))));
  vec4 cavg5 = (ctc + cml + cmc + cmr + cbc) / 5.0;
  cmin = 0.5 * (cmin + cmin5);
  cmax = 0.5 * (cmax + cmax5);
  cavg = 0.5 * (cavg + cavg5);
#endif

#elif MINMAX_4TAP_VARYING// this is the method used in v2 (PDTemporalReprojection2)
  const float _SubpixelThreshold = 0.5;
  const float _GatherBase = 0.5;
  const float _GatherSubpixelMotion = 0.1666;

  vec2 texel_vel = ss_vel * u_texel_size.xy;
  float texel_vel_mag = length(texel_vel) * vs_dist;
  float k_subpixel_motion = clamp(_SubpixelThreshold / (FLT_EPS + texel_vel_mag), 0.0, 1.0);
  float k_min_max_support = _GatherBase + _GatherSubpixelMotion * k_subpixel_motion;

  vec2 ss_offset01 = k_min_max_support * vec2(-u_texel_size.x, u_texel_size.y);
  vec2 ss_offset11 = k_min_max_support * vec2(u_texel_size.x, u_texel_size.y);
  vec4 c00 = sample_color(u_tex_main, uv - ss_offset11);
  vec4 c10 = sample_color(u_tex_main, uv - ss_offset01);
  vec4 c01 = sample_color(u_tex_main, uv + ss_offset01);
  vec4 c11 = sample_color(u_tex_main, uv + ss_offset11);

  vec4 cmin = min(c00, min(c10, min(c01, c11)));
  vec4 cmax = max(c00, max(c10, max(c01, c11)));

#if USE_YCOCG || USE_CLIPPING
  vec4 cavg = (c00 + c10 + c01 + c11) / 4.0;
#endif
#endif

  // shrink chroma min-max
#if USE_YCOCG
  vec2 chroma_extent = vec2(0.25 * 0.5 * (cmax.r - cmin.r));
  vec2 chroma_center = texel0.gb;
  cmin.yz = chroma_center - chroma_extent;
  cmax.yz = chroma_center + chroma_extent;
  cavg.yz = chroma_center;
#endif

  // clamp to neighbourhood of current sample
#if USE_CLIPPING
  texel1 = clip_aabb(cmin.xyz, cmax.xyz, clamp(cavg, cmin, cmax), texel1);
#else
  texel1 = clamp(texel1, cmin, cmax);
#endif

  // feedback weight from unbiased luminance diff (t.lottes)
#if USE_YCOCG
  float lum0 = texel0.r;
  float lum1 = texel1.r;
#else
  float lum0 = luminance(texel0.rgb);
  float lum1 = luminance(texel1.rgb);
#endif
  float unbiased_diff = abs(lum0 - lum1) / max(lum0, max(lum1, 0.2));
  float unbiased_weight = 1.0 - unbiased_diff;
  float unbiased_weight_sqr = unbiased_weight * unbiased_weight;
  float k_feedback = mix(u_feedback_min, u_feedback_max, unbiased_weight_sqr);

  // output
  return mix(texel0, texel1, k_feedback);
}

in vec2 tc;
layout(location = 0) out vec4 out_buff;
layout(location = 1) out vec4 out_frag;

void main() {
#if UNJITTER_REPROJECTION
  vec2 uv = tc - u_jitter_uv.xy;
#else
  vec2 uv = tc;
#endif

#if USE_DILATION
  //--- 3x3 norm (sucks)
  //vec2 ss_vel = sample_velocity_dilated(_VelocityBuffer, uv, 1);
  //float vs_dist = depth_sample_linear(uv);

  //--- 5 tap nearest (decent)
  //vec3 c_frag = find_closest_fragment_5tap(uv);
  //vec2 ss_vel = tex2D(_VelocityBuffer, c_frag.xy).xy;
  //float vs_dist = depth_resolve_linear(c_frag.z);

  //--- 3x3 nearest (good)
  vec3 c_frag = find_closest_fragment_3x3(uv);
  vec2 ss_vel = texture(u_tex_vel, c_frag.xy).xy;
  float vs_dist = c_frag.z;
#else
  vec2 ss_vel = texture(u_tex_vel, uv).xy;
  float vs_dist = depth_sample_linear(uv);
#endif

  // temporal resolve
  vec4 color_temporal = temporal_reprojection(uv, ss_vel, vs_dist);

  // prepare outputs
  vec4 to_buffer = resolve_color(color_temporal);

#if USE_MOTION_BLUR
#if USE_MOTION_BLUR_NEIGHBORMAX
  ss_vel = u_motion_scale * texture(u_tex_vel_neighbormax, uv).xy;
#else
  ss_vel = u_motion_scale * ss_vel;
#endif

  float vel_mag = length(ss_vel * u_texel_size.zw);
  const float vel_trust_full = 2.0;  // 2.0
  const float vel_trust_none = 15.0; // 15.0
  const float vel_trust_span = vel_trust_none - vel_trust_full;
  float trust = 1.0 - clamp(vel_mag - vel_trust_full, 0.0, vel_trust_span) / vel_trust_span;

#if UNJITTER_COLORSAMPLES
  //vec4 color_motion = reconstruct_color(u_tex_main, uv, ss_vel);
  vec4 color_motion = sample_color_motion(u_tex_main, uv, ss_vel);
#else
  //vec4 color_motion = reconstruct_color(u_tex_main, uv - u_jitter_uv.xy, ss_vel);
  vec4 color_motion = sample_color_motion(u_tex_main, uv - u_jitter_uv.xy, ss_vel);
#endif

  vec4 to_screen = resolve_color(mix(color_motion, color_temporal, trust));
#else
  vec4 to_screen = resolve_color(color_temporal);
#endif

  //// NOTE: velocity debug
  //to_screen.g += 1000.0 * length(ss_vel);
  //to_screen = vec4(1000.0 * abs(ss_vel), 0.0, 0.0);
  //to_screen = vec4(ss_vel*1000,0,1);

  out_buff = to_buffer;
  out_frag = to_screen;

  //out_frag = texture(u_tex_vel_neighbormax, uv);
  //out_frag = texture(u_tex_prev, uv);
}
