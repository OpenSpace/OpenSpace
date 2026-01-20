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

#include <core/md_str.h>

namespace mol::internal {

static constexpr str_t vShaderSrcFsQuad = STR(
R"(
#version 150 core

out vec2 tc;

uniform vec2 u_tc_scl = vec2(1,1);

void main() {
  uint idx = uint(gl_VertexID) % 3U;
  gl_Position = vec4(
    (float( idx     &1U)) * 4.0 - 1.0,
    (float((idx>>1U)&1U)) * 4.0 - 1.0,
    0,
    1.0
  );
  tc = (gl_Position.xy * 0.5 + 0.5) * u_tc_scl;
}
)");

// From here
// https://github.com/kosua20/Rendu/blob/master/resources/common/shaders/screens/fxaa.frag
static constexpr str_t fShaderSrcFxaa = STR(
R"(
#version 330 core
in vec2 tc;

uniform sampler2D tex;
uniform vec2 inverseScreenSize;

// Settings for FXAA.
#define EDGE_THRESHOLD_MIN 0.0312
#define EDGE_THRESHOLD_MAX 0.125
#define QUALITY(q) ((q) < 5 ? 1.0 : ((q) > 5 ? ((q) < 10 ? 2.0 : ((q) < 11 ? 4.0 : 8.0)) : 1.5))
#define ITERATIONS 12
#define SUBPIXEL_QUALITY 0.75

// Relative luminance
float rgb2luma(vec3 rgb){
  return sqrt(dot(rgb, vec3(0.299, 0.587, 0.114)));
}
// Output: the fragment color
out vec4 fragColor;

/** Performs FXAA post-process anti-aliasing as described in the Nvidia FXAA white paper and the associated shader code.
*/
void main() {
  vec4 colorCenter = texture(tex, tc);
    
  // Luma at the current fragment
  float lumaCenter = rgb2luma(colorCenter.rgb);
    
  // Luma at the four direct neighbours of the current fragment.
  float lumaDown = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2( 0,-1)).rgb);
  float lumaUp = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2( 0, 1)).rgb);
  float lumaLeft = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2(-1, 0)).rgb);
  float lumaRight = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2( 1, 0)).rgb);
    
  // Find the maximum and minimum luma around the current fragment.
  float lumaMin = min(lumaCenter, min(min(lumaDown, lumaUp), min(lumaLeft, lumaRight)));
  float lumaMax = max(lumaCenter, max(max(lumaDown, lumaUp), max(lumaLeft, lumaRight)));
    
  // Compute the delta.
  float lumaRange = lumaMax - lumaMin;
    
  // If the luma variation is lower that a threshold (or if we are in a really dark area),
  // we are not on an edge, don't perform any AA.
  if (lumaRange < max(EDGE_THRESHOLD_MIN, lumaMax * EDGE_THRESHOLD_MAX)) {
    fragColor = colorCenter;
    return;
  }
    
  // Query the 4 remaining corners lumas.
  float lumaDownLeft = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2(-1,-1)).rgb);
  float lumaUpRight = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2( 1, 1)).rgb);
  float lumaUpLeft = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2(-1, 1)).rgb);
  float lumaDownRight = rgb2luma(textureLodOffset(tex, tc, 0.0, ivec2( 1,-1)).rgb);
    
  // Combine the four edges lumas (using intermediary variables for future computations
  // with the same values).
  float lumaDownUp = lumaDown + lumaUp;
  float lumaLeftRight = lumaLeft + lumaRight;
    
  // Same for corners
  float lumaLeftCorners = lumaDownLeft + lumaUpLeft;
  float lumaDownCorners = lumaDownLeft + lumaDownRight;
  float lumaRightCorners = lumaDownRight + lumaUpRight;
  float lumaUpCorners = lumaUpRight + lumaUpLeft;
    
  // Compute an estimation of the gradient along the horizontal and vertical axis.
  float edgeHorizontal =
    abs(-2.0 * lumaLeft + lumaLeftCorners) + abs(-2.0 * lumaCenter + lumaDownUp ) * 2.0 +
    abs(-2.0 * lumaRight + lumaRightCorners);
  float edgeVertical =
    abs(-2.0 * lumaUp + lumaUpCorners) + abs(-2.0 * lumaCenter + lumaLeftRight) * 2.0 +
    abs(-2.0 * lumaDown + lumaDownCorners);
    
  // Is the local edge horizontal or vertical ?
  bool isHorizontal = (edgeHorizontal >= edgeVertical);
    
  // Choose the step size (one pixel) accordingly.
  float stepLength = isHorizontal ? inverseScreenSize.y : inverseScreenSize.x;
    
  // Select the two neighboring texels lumas in the opposite direction to the local edge.
  float luma1 = isHorizontal ? lumaDown : lumaLeft;
  float luma2 = isHorizontal ? lumaUp : lumaRight;
  // Compute gradients in this direction.
  float gradient1 = luma1 - lumaCenter;
  float gradient2 = luma2 - lumaCenter;
    
  // Which direction is the steepest ?
  bool is1Steepest = abs(gradient1) >= abs(gradient2);
    
  // Gradient in the corresponding direction, normalized.
  float gradientScaled = 0.25*max(abs(gradient1),abs(gradient2));
    
  // Average luma in the correct direction.
  float lumaLocalAverage = 0.0;
  if (is1Steepest) {
    // Switch the direction
    stepLength = - stepLength;
    lumaLocalAverage = 0.5*(luma1 + lumaCenter);
  }
  else {
    lumaLocalAverage = 0.5*(luma2 + lumaCenter);
  }
    
  // Shift UV in the correct direction by half a pixel.
  vec2 currentUv = tc;
  if (isHorizontal) {
    currentUv.y += stepLength * 0.5;
  }
  else {
    currentUv.x += stepLength * 0.5;
  }
    
  // Compute offset (for each iteration step) in the right direction.
  vec2 offset =
    isHorizontal ?
    vec2(inverseScreenSize.x, 0.0) :
    vec2(0.0, inverseScreenSize.y);
  // Compute UVs to explore on each side of the edge, orthogonally. The QUALITY allows us
  // to step faster.
  vec2 uv1 = currentUv - offset * QUALITY(0);
  vec2 uv2 = currentUv + offset * QUALITY(0);
    
  // Read the lumas at both current extremities of the exploration segment, and compute
  // the delta wrt to the local average luma.
  float lumaEnd1 = rgb2luma(textureLod(tex, uv1, 0.0).rgb);
  float lumaEnd2 = rgb2luma(textureLod(tex, uv2, 0.0).rgb);
  lumaEnd1 -= lumaLocalAverage;
  lumaEnd2 -= lumaLocalAverage;
    
  // If the luma deltas at the current extremities is larger than the local gradient, we
  // have reached the side of the edge.
  bool reached1 = abs(lumaEnd1) >= gradientScaled;
  bool reached2 = abs(lumaEnd2) >= gradientScaled;
  bool reachedBoth = reached1 && reached2;
    
  // If the side is not reached, we continue to explore in this direction.
  if (!reached1) {
    uv1 -= offset * QUALITY(1);
  }
  if (!reached2) {
    uv2 += offset * QUALITY(1);
  }
    
  // If both sides have not been reached, continue to explore.
  if (!reachedBoth) {
    for (int i = 2; i < ITERATIONS; i++) {
      // If needed, read luma in 1st direction, compute delta.
      if (!reached1) {
        lumaEnd1 = rgb2luma(textureLod(tex, uv1, 0.0).rgb);
        lumaEnd1 = lumaEnd1 - lumaLocalAverage;
      }
      // If needed, read luma in opposite direction, compute delta.
      if (!reached2) {
        lumaEnd2 = rgb2luma(textureLod(tex, uv2, 0.0).rgb);
        lumaEnd2 = lumaEnd2 - lumaLocalAverage;
      }
      // If the luma deltas at the current extremities is larger than the local gradient,
      // we have reached the side of the edge.
      reached1 = abs(lumaEnd1) >= gradientScaled;
      reached2 = abs(lumaEnd2) >= gradientScaled;
      reachedBoth = reached1 && reached2;
            
      // If the side is not reached, we continue to explore in this direction, with a
      // variable quality.
      if (!reached1) {
        uv1 -= offset * QUALITY(i);
      }
      if (!reached2) {
        uv2 += offset * QUALITY(i);
      }
            
      // If both sides have been reached, stop the exploration.
      if (reachedBoth) {
        break;
      }
    }
  }
    
  // Compute the distances to each side edge of the edge (!).
  float distance1 = isHorizontal ? (tc.x - uv1.x) : (tc.y - uv1.y);
  float distance2 = isHorizontal ? (uv2.x - tc.x) : (uv2.y - tc.y);
    
  // In which direction is the side of the edge closer ?
  bool isDirection1 = distance1 < distance2;
  float distanceFinal = min(distance1, distance2);
    
  // Thickness of the edge.
  float edgeThickness = (distance1 + distance2);
    
  // Is the luma at center smaller than the local average ?
  bool isLumaCenterSmaller = lumaCenter < lumaLocalAverage;
    
  // If the luma at center is smaller than at its neighbour, the delta luma at each end
  // should be positive (same variation).
  bool correctVariation1 = (lumaEnd1 < 0.0) != isLumaCenterSmaller;
  bool correctVariation2 = (lumaEnd2 < 0.0) != isLumaCenterSmaller;
    
  // Only keep the result in the direction of the closer side of the edge.
  bool correctVariation = isDirection1 ? correctVariation1 : correctVariation2;
   
  // UV offset: read in the direction of the closest side of the edge.
  float pixelOffset = - distanceFinal / edgeThickness + 0.5;
    
  // If the luma variation is incorrect, do not offset.
  float finalOffset = correctVariation ? pixelOffset : 0.0;
    
  // Sub-pixel shifting
  // Full weighted average of the luma over the 3x3 neighborhood.
  float lumaAverage = (1.0 / 12.0) *
    (2.0 * (lumaDownUp + lumaLeftRight) + lumaLeftCorners + lumaRightCorners);
  // Ratio of the delta between the global average and the center luma, over the luma
  // range in the 3x3 neighborhood.
  float subPixelOffset1 = clamp(abs(lumaAverage - lumaCenter) / lumaRange, 0.0, 1.0);
  float subPixelOffset2 =
    (-2.0 * subPixelOffset1 + 3.0) * subPixelOffset1 * subPixelOffset1;
  // Compute a sub-pixel offset based on this delta.
  float subPixelOffsetFinal = subPixelOffset2 * subPixelOffset2 * SUBPIXEL_QUALITY;
    
  // Pick the biggest of the two offsets.
  finalOffset = max(finalOffset,subPixelOffsetFinal);
    
  // Compute the final UV coordinates.
  vec2 finalUv = tc;
  if (isHorizontal) {
    finalUv.y += finalOffset * stepLength;
  }
  else {
    finalUv.x += finalOffset * stepLength;
  }
    
  // Read the color at the new UV coordinates, and use it.
  fragColor = textureLod(tex, finalUv, 0.0);
}
)");

static constexpr str_t fShaderSrcLinearizeDepth = STR(
R"(
#version 150 core

#ifndef PERSPECTIVE
#define PERSPECTIVE 1
#endif

// z_n * z_f,  z_n - z_f,  z_f, *not used*
uniform vec4 u_clip_info;
uniform sampler2D u_tex_depth;

float denormalizeFloat(float inpt) {
  if (inpt < 0.0) {
    return inpt + 1.0;
  }
  else {
    return pow(10, 30) * inpt;
  }
}

float ReconstructCSZ(float d, vec4 clip_info) {
#if PERSPECTIVE
  return (clip_info[0] / (d * clip_info[1] + clip_info[2]));
#else
  return (clip_info[1] + clip_info[2] - d*clip_info[1]);
#endif
}

out vec4 out_frag;

void main() {
  float d = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy), 0).x;
  out_frag = vec4(ReconstructCSZ(d, u_clip_info), 0, 0, 0);
}
)");

static constexpr str_t fShaderSrcSsao = STR(
R"(
#version 150 core

#pragma optionNV(unroll all)

#ifndef AO_RANDOM_TEX_SIZE
#define AO_RANDOM_TEX_SIZE 4
#endif

#ifndef AO_PERSPECTIVE
#define AO_PERSPECTIVE 1
#endif

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
vec2 rotate_sample(vec2 sample, vec2 cos_sin) {
  return vec2(
    sample.x * cos_sin.x - sample.y * cos_sin.y,
    sample.x * cos_sin.y + sample.y * cos_sin.x
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
    vec4 sample = control.sample_pattern[i];
    vec2 uv = rotate_sample(sample.xy, jitter.xy) * jitter.z;
    float weight_scale = sample.z;
    float mip_level = mip_offset + sample.w;
        
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
)");

static constexpr str_t fShaderSrcSsaoBlur = STR(
R"(
#version 150 core

#pragma optionNV(unroll all)

const float KERNEL_RADIUS = 3;
  
uniform float u_sharpness;
uniform vec2  u_inv_res_dir; // either set x to 1/width or y to 1/height
uniform sampler2D u_tex_ao;
uniform sampler2D u_tex_linear_depth;

in vec2 tc;
out vec4 out_frag;

float blur_function(vec2 uv, float r, float center_c, float center_d, inout float w_total)
{
  float c = texture(u_tex_ao, uv).x;
  float d = texture(u_tex_linear_depth, uv).x;

  const float sigma = KERNEL_RADIUS * 0.5;
  const float falloff = 1.0 / (2.0 * sigma * sigma);

  float ddiff = (d - center_d) * u_sharpness;
  float w = exp2(-r * r * falloff - ddiff * ddiff);
  w_total += w;

  return c * w;
}

void main() {
  float center_c = texture(u_tex_ao, tc).x;
  float center_d = texture(u_tex_linear_depth, tc).x;

  float c_total = center_c;
  float w_total = 1.0;

  for (float r = 1; r <= KERNEL_RADIUS; ++r) {
    vec2 uv = tc + u_inv_res_dir * r;
    c_total += blur_function(uv, r, center_c, center_d, w_total);  
  }

  for (float r = 1; r <= KERNEL_RADIUS; ++r) {
    vec2 uv = tc - u_inv_res_dir * r;
    c_total += blur_function(uv, r, center_c, center_d, w_total);  
  }

  out_frag = vec4(vec3(c_total/w_total), 1);
}
)");

constexpr str_t fShaderSrcDeferredShading = STR(
R"(
#version 150 core

uniform sampler2D u_texture_depth;
uniform sampler2D u_texture_color;
uniform sampler2D u_texture_normal;

uniform mat4 u_inv_proj_mat;
uniform vec3 u_light_dir;
uniform vec3 u_light_col;
uniform float u_time;

in vec2 tc;
out vec4 out_frag;

// TODO: Use linear depth instead and use uniform vec4 for unpacking to view coords.

vec4 depth_to_view_coord(vec2 tex_coord, float depth) {
  vec4 clip_coord = vec4(vec3(tex_coord, depth) * 2.0 - 1.0, 1.0);
  vec4 view_coord = u_inv_proj_mat * clip_coord;
  return view_coord / view_coord.w;
}

float fresnel(float H_dot_V) {
  const float n1 = 1.0;
  const float n2 = 1.5;
  const float R0 = pow((n1 - n2) / (n1 + n2), 2);
  return R0 + (1.0 - R0) * pow(1.0 - H_dot_V, 5);
}

// https://aras-p.info/texts/CompactNormalStorage.html
vec3 decode_normal(vec2 enc) {
  vec2 fenc = enc * 4 - 2;
  float f = dot(fenc, fenc);
  float g = sqrt(1 - f / 4.0);
  vec3 n;
  n.xy = fenc * g;
  n.z = 1 - f / 2.0;
  return n;
}

vec4 rand4(vec2 n) {
  return fract(
    sin(dot(n.xy, vec2(12.9898, 78.233))) * vec4(43758.5453, 28001.8384, 50849.4141, 12996.89)
  );
}

vec4 srand4(vec2 n) {
  return rand4(n) * 2.0 - 1.0;
}

const float spec_exp = 100.0;
const vec3 env_radiance = vec3(5.0);
vec3 dir_radiance = u_light_col;
vec3 L = u_light_dir;

vec3 lambert(in vec3 radiance) {
  const float ONE_OVER_PI = 1.0 / 3.1415926535;
  return radiance * ONE_OVER_PI;
}

vec3 shade(vec3 color, vec3 V, vec3 N) {
  vec3 H = normalize(L + V);
  float H_dot_V = clamp(dot(H, V), 0.0, 1.0);
  float N_dot_H = clamp(dot(N, H), 0.0, 1.0);
  float N_dot_L = clamp(dot(N, L), 0.0, 1.0);
  float fr = fresnel(H_dot_V);

  vec3 diffuse = color.rgb * lambert(env_radiance + N_dot_L * dir_radiance);
  vec3 specular = fr * (env_radiance + dir_radiance) * pow(N_dot_H, spec_exp);

  return diffuse + specular;
}

void main() {
  float depth = texelFetch(u_texture_depth, ivec2(gl_FragCoord.xy), 0).x;
  if (depth == 1.0) {
    out_frag = vec4(0);
    return;
  }
  vec4 color = texelFetch(u_texture_color, ivec2(gl_FragCoord.xy), 0);
  vec3 normal = decode_normal(texelFetch(u_texture_normal, ivec2(gl_FragCoord.xy), 0).xy);
  vec4 view_coord = depth_to_view_coord(tc, depth);

  // Add noise to reduce banding
  //vec4 noise4 = srand4(tc + u_time + 0.6959174) / 10.0;
  //color += color*noise4;

  vec3 N = normal;
  vec3 V = -normalize(view_coord.xyz);
  vec3 result = shade(color.rgb, V, N);

  out_frag = vec4(result, color.a);
}
)");


// ------ TONEMAP ------

constexpr str_t fShaderSrcTonemapPassthrough = STR(
R"(
#version 150 core
uniform sampler2D u_texture;
out vec4 out_frag;

void main() {
  vec4 color = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
  out_frag = color;
}
)");

constexpr str_t fShaderSrcTonemapExposureGamma = STR(
R"(
#version 150 core

uniform sampler2D u_texture;
uniform float u_exposure = 1.0;
uniform float u_gamma = 2.2;

out vec4 out_frag;

void main() {
  vec4 color = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
  const float exposure_bias = 0.5;
  color.rgb = exposure_bias * u_exposure * pow(color.rgb, 1.0 / vec3(u_gamma));
  out_frag = color;
}
)");

constexpr str_t fShaderSrcTonemapFilmic = STR(
R"(
#version 150 core

uniform sampler2D u_texture;
uniform float u_exposure = 1.0;
uniform float u_gamma = 2.2;

out vec4 out_frag;

// Source from here
// http://filmicworlds.com/blog/filmic-tonemapping-operators/

vec3 uncharted_tonemap(vec3 x) {
  const float A = 0.15;
  const float B = 0.50;
  const float C = 0.10;
  const float D = 0.20;
  const float E = 0.02;
  const float F = 0.30;
  return ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F;
}

vec3 uncharted(vec3 c) {
  const float W = 11.2;
  c *= u_exposure;

  const float exposure_bias = 0.5;
  vec3 curr = uncharted_tonemap(exposure_bias * c);

  vec3 white_scale = vec3(1.0) / uncharted_tonemap(vec3(W));
  vec3 color = curr * white_scale;

  return pow(color, 1.0 / vec3(u_gamma));
}

void main() {
  vec4 color = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
  color.rgb = uncharted(color.rgb);
  out_frag = color;
}
)");

constexpr str_t fShaderSrcTonemapAces = STR(
R"(
#version 150 core

uniform sampler2D u_texture;
uniform float u_exposure = 1.0;
uniform float u_gamma = 2.2;

out vec4 out_frag;

// Sources:
// https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
// https://github.com/TheRealMJP/BakingLab/blob/master/BakingLab/ACES.hlsl

// sRGB => XYZ => D65_2_D60 => AP1 => RRT_SAT
mat3 ACESInputMat = mat3(
  vec3(0.59719, 0.35458, 0.04823),
  vec3(0.07600, 0.90834, 0.01566),
  vec3(0.02840, 0.13383, 0.83777)
);

// ODT_SAT => XYZ => D60_2_D65 => sRGB
mat3 ACESOutputMat = mat3(
  vec3(1.60475, -0.53108, -0.07367),
  vec3(-0.10208, 1.10813, -0.00605),
  vec3(-0.00327, -0.07276, 1.07602)
);

vec3 RRTAndODTFit(vec3 v) {
  vec3 a = v * (v + 0.0245786f) - 0.000090537f;
  vec3 b = v * (0.983729f * v + 0.4329510f) + 0.238081f;
  return a / b;
}

vec3 ACESFitted(vec3 color) {
  color = color * ACESInputMat;
  color = RRTAndODTFit(color);
  color = color * ACESOutputMat;
  return clamp(color, 0.0, 1.0);
}

vec3 ACES(in vec3 c) {
  float a = 2.51f;
  float b = 0.03f;
  float y = 2.43f;
  float d = 0.59f;
  float e = 0.14f;
  return clamp((c * (a * c + b)) / (c * (y * c + d) + e), 0.0, 1.0);
}

void main() {
  vec4 in_frag = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
  const float exposure_bias = 0.5;
  vec3 color = in_frag.rgb * exposure_bias * u_exposure;
  color = ACESFitted(color);
  color = pow(color, vec3(1.0 / vec3(u_gamma)));
  out_frag = vec4(color, in_frag.a);
}
)");

// ------ DOF ------

constexpr str_t fShaderSrcDofHalfresPrepass = STR(
R"(
#version 150 core

uniform sampler2D u_tex_depth; // Linear depth
uniform sampler2D u_tex_color;

uniform float u_focus_point;
uniform float u_focus_scale;

float getBlurSize(float d, float fp, float fs) {
  float coc = clamp((1.0 / fp - 1.0 / d) * fs, -1.0, 1.0);
  return abs(coc);
}

in vec2 tc;
out vec4 out_frag;

void main() {
  float depth = textureLod(u_tex_depth, tc, 1).r;
  vec3 color = textureLod(u_tex_color, tc, 1).rgb;
  float coc = getBlurSize(depth, u_focus_point, u_focus_scale);

  out_frag = vec4(color, coc);
}
)");

constexpr str_t fShaderSrcDof = STR(
R"(
#version 150 core
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
)");

// ------ SCREEN SPACE VELOCITY ------

constexpr str_t fShaderSrcVelBlit = STR(
R"(
#version 150 core

uniform sampler2D u_tex_depth;
uniform mat4 u_curr_clip_to_prev_clip_mat;
uniform vec4 u_jitter_uv;

in vec2 tc;
out vec4 out_ss_vel;

void main() {
  float d = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy), 0).x;
  if (d == 1.0f) discard;
  //d = 1.0f;

  vec2 p_uv = tc;
  vec3 p_vs = vec3(tc, d);
  vec4 p_cs = vec4(p_vs * 2.0 - 1.0, 1.0); // [0, 1] -> [-1, 1]
    
  vec4 q_cs = u_curr_clip_to_prev_clip_mat * p_cs;
  vec2 q_uv = (q_cs.xy / q_cs.w) * 0.5 + 0.5; // [-1, 1] -> [0, 1]

  vec2 ss_vel = (p_uv - q_uv) + (u_jitter_uv.xy - u_jitter_uv.zw);

  out_ss_vel = vec4(ss_vel, 0, 0);
}
)");

constexpr str_t fShaderSrcVelTilemax = STR(
R"(
#version 150 core

#pragma optionNV(unroll all)

uniform sampler2D u_tex_vel;
uniform vec2 u_tex_vel_texel_size;

#ifndef TILE_SIZE
#define TILE_SIZE 20
#endif

in vec2 tc;
out vec4 out_max_tile_vel;

void main() {
  vec2 step = u_tex_vel_texel_size;
  vec2 base = tc + (0.5 - 0.5 * TILE_SIZE) * step;

  vec2 mv = vec2(0.0);
  float mv2 = 0.0;

  for (int i = 0; i < TILE_SIZE; i++) {
    for (int j = 0; j < TILE_SIZE; j++) {
      vec2 v = texture(u_tex_vel, base + vec2(i, j) * step).xy;
      float v2 = dot(v,v);
      if (v2 > mv2) {
        mv = v;
        mv2 = v2;
      }
    } 
  }

  out_max_tile_vel = vec4(min(mv, length(u_tex_vel_texel_size) * TILE_SIZE), 0, 0);
}
)");

constexpr str_t fShaderSrcVelNeighbormax = STR(
R"(
#version 150 core

#pragma optionNV(unroll all)

#define EXTENT 2

uniform sampler2D u_tex_vel;
uniform vec2 u_tex_vel_texel_size;

in vec2 tc;
out vec4 out_max_neighbor_vel;

void main() {
  vec2 step = u_tex_vel_texel_size;
  vec2 base = tc;

  vec2 mv = vec2(0.0);
  float mv2 = 0.0;

  for (int i = -EXTENT; i <= EXTENT; i++) {
    for (int j = -EXTENT; j <= EXTENT; j++) {
      vec2 v = texture(u_tex_vel, base + vec2(i, j) * step).xy;
      float v2 = dot(v,v);
      if (v2 > mv2) {
        mv = v;
        mv2 = v2;
      }
    }
  }

  out_max_neighbor_vel = vec4(mv, 0.0, 0.0);
}
)");

// ------ Temporal AA and motion blur ------

constexpr str_t fShaderSrcTemporal = STR(
R"(
#version 150 core

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
#ifndef USE_MOTION_BLUR
#define USE_MOTION_BLUR 1
#endif
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
)");

} // namespace mol::internal
