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

#ifndef AO_RANDOM_TEX_SIZE
#define AO_RANDOM_TEX_SIZE 4
#endif

#define AO_PERSPECTIVE #{AoPerspective}

#ifndef AO_NUM_SAMPLES
#define AO_NUM_SAMPLES 32
#endif

struct HBAOData {
  float radius_to_screen;
  float neg_inv_r2;
  float n_dot_v_bias;
  float normal_bias;

  float ao_multiplier;
  float pow_exponent;
  vec2 inv_full_res;

  vec4 proj_info;

  vec4 sample_pattern[32];
};

layout(std140) uniform u_control_buffer {
  HBAOData control;
};

uniform sampler2D u_tex_linear_depth;
uniform sampler2D u_tex_normal;
uniform sampler2D u_tex_random;

in vec2 tc;
out vec4 out_frag;


float rand(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233))) * 43758.5453);
}
vec2 rand2(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233))) * vec2(43758.5453, 28001.8384));
}
vec3 rand3(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233))) * vec3(43758.5453, 28001.8384, 50849.4141));
}
vec4 rand4(vec2 n) {
  return fract(sin(dot(n.xy, vec2(12.9898, 78.233))) * vec4(43758.5453, 28001.8384, 50849.4141, 12996.89));
}

float srand(vec2 n) {
  return rand(n) * 2.0 - 1.0;
}
vec2 srand2(vec2 n) {
  return rand2(n) * 2.0 - 1.0;
}
vec3 srand3(vec2 n) {
  return rand3(n) * 2.0 - 1.0;
}
vec4 srand4(vec2 n) {
  return rand4(n) * 2.0 - 1.0;
}

vec3 uv_to_view(vec2 uv, float eye_z) {
#if AO_PERSPECTIVE
  return vec3((uv * control.proj_info.xy + control.proj_info.zw) * eye_z, eye_z);
#else
  return vec3((uv * control.proj_info.xy + control.proj_info.zw), eye_z);
#endif
}

vec3 fetch_view_pos(vec2 uv, float lod) {
  float view_depth = textureLod(u_tex_linear_depth, uv, lod).x;
  return uv_to_view(uv, view_depth);
}

vec3 decode_normal(vec2 enc) {
  vec2 fenc = enc * 4 - 2;
  float f = dot(fenc, fenc);
  float g = sqrt(1 - f / 4.0);
  vec3 n;
  n.xy = fenc * g;
  n.z = 1 - f / 2.0;
  return n;
}

vec3 fetch_view_normal(vec2 uv) {
  vec2 enc = texelFetch(u_tex_normal, ivec2(gl_FragCoord.xy), 0).xy;
  //vec2 enc = textureLod(u_tex_normal, uv, 0).xy;
  vec3 n = decode_normal(enc);
  return n * vec3(1, 1, -1);
}

//----------------------------------------------------------------------------------
float falloff(float dist2) {
  // 1 scalar mad instruction
  return dist2 * control.neg_inv_r2 + 1.0;
}

//----------------------------------------------------------------------------------
// P = view-space position at the kernel center
// N = view-space normal at the kernel center
// S = view-space position of the current sample
//----------------------------------------------------------------------------------
float compute_pixel_obscurance(vec3 P, vec3 N, vec3 S) {
  vec3 V = S - P;
  float VdotV = dot(V, V);
  float NdotV = dot(N, V) * 1.0 / sqrt(VdotV);

  float falloff_mult = max(0.0, falloff(VdotV));
  return max(0.0, NdotV - control.n_dot_v_bias) * falloff_mult;
}

//----------------------------------------------------------------------------------
vec2 rotate_sample(vec2 smpl, vec2 cos_sin) {
  return vec2(
    smpl.x * cos_sin.x - smpl.y * cos_sin.y,
    smpl.x * cos_sin.y + smpl.y * cos_sin.x
  );
}

//----------------------------------------------------------------------------------
vec4 get_jitter(vec2 uv) {
  // (cos(Alpha),sin(Alpha),rand1,rand2)
  vec2 coord = gl_FragCoord.xy / AO_RANDOM_TEX_SIZE;
  vec4 jitter = textureLod(u_tex_random, coord, 0);
  return jitter;
}

//----------------------------------------------------------------------------------
float compute_ao(vec2 full_res_uv, float radius_pixels, vec4 jitter, vec3 view_position,
                 vec3 view_normal)
{
  const float global_mip_offset = -4.3; // -4.3 is recomended in the intel ASSAO implementation
  float mip_offset = log2(radius_pixels) + global_mip_offset;

  float weight_sum = 0.0;
  float ao = 0.0;

  //vec2 noise = srand2(full_res_uv + vec2(control.time) + 0.2765672);
  //vec2 cos_sin = vec2(cos(noise.x * 3.1415926535), sin(noise.x * 3.1415026535));

  vec3 normal = mix(vec3(0,0,1), view_normal, control.normal_bias);

  for (int i = 0; i < AO_NUM_SAMPLES; i++) {
    vec4 smpl = control.sample_pattern[i];
    vec2 uv = rotate_sample(smpl.xy, jitter.xy) * jitter.z;
    float weight_scale = smpl.z;
    float mip_level = mip_offset + smpl.w;

    vec2 snapped_uv = round(radius_pixels * uv) * control.inv_full_res + full_res_uv;
    vec3 view_sample = fetch_view_pos(snapped_uv, mip_level);
    ao += compute_pixel_obscurance(view_position, view_normal, view_sample) * weight_scale;
    weight_sum += 1.0;
  }
  ao *= control.ao_multiplier / weight_sum;

  return clamp(1.0 - ao, 0.0, 1.0);
}

//----------------------------------------------------------------------------------
void main() {
  vec2 uv = tc;
  vec3 view_position = fetch_view_pos(uv, 0);
  vec3 view_normal = fetch_view_normal(uv);

  // Compute projection of disk of radius control.R into screen space
#if AO_PERSPECTIVE
  float radius_pixels = control.radius_to_screen / view_position.z;
#else
  float radius_pixels = control.radius_to_screen;
#endif
  // Get jitter vector for the current full-res pixel
  vec4 jitter = get_jitter(uv);
  float ao = compute_ao(uv, radius_pixels, jitter, view_position, view_normal);

  out_frag = vec4(vec3(pow(ao, control.pow_exponent)), 1);
}
