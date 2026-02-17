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

const float FltEps = 0.00000001f;

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

in Data {
  vec2 texCoords;
} in_data;

layout(location = 0) out vec4 outBuff;
layout(location = 1) out vec4 outFrag;

// This is ported straight from the implementation given by playdeadgames (MIT License)
// https://github.com/playdeadgames/temporal/blob/master/Assets/Shaders/TemporalReprojection.shader

uniform sampler2D texMain;
uniform sampler2D texPrev;
uniform sampler2D texLinearDepth;
uniform sampler2D texVel;
uniform sampler2D texVelNeighbormax;
uniform vec4 texelSize;
uniform vec4 jitterUv;
uniform float time;
uniform float feedbackMin = 0.88;
uniform float feedbackMax = 0.97;
uniform float motionScale = 0.1;


// https://software.intel.com/en-us/node/503873
vec3 rgbToYcocg(vec3 c) {
  // Y = R/4 + G/2 + B/4
  // Co = R/2 - B/2
  // Cg = -R/4 + G/2 - B/4
  return vec3(
    c.r / 4.0 + c.g / 2.0 + c.b / 4.0,
    c.r / 2.0 - c.b / 2.0,
    -c.r / 4.0 + c.g / 2.0 - c.b / 4.0
  );
}

// https://software.intel.com/en-us/node/503873
vec3 ycocgToRgb(vec3 c) {
  // R = Y + Co - Cg
  // G = Y + Cg
  // B = Y - Co - Cg
  return vec3(
    c.r + c.g - c.b,
    c.r + c.b,
    c.b - c.g - c.b
  );
}

float luminance(vec3 c) {
  const vec3 W = vec3(0.2125, 0.7154, 0.0721);
  return dot(c, W);
}

float PDsrand(vec2 n) {
  float r = fract(sin(dot(n.xy, vec2(12.9898, 78.233)))* 43758.5453);
  return r * 2.0 - 1.0;
}

vec3 findClosestFragment(vec2 uv) {
  vec2 dd = abs(texelSize.xy);
  vec2 du = vec2(dd.x, 0.0);
  vec2 dv = vec2(0.0, dd.y);

  vec3 dtl = vec3(-1.0, -1.0, texture(texLinearDepth, uv - dv - du).x);
  vec3 dtc = vec3( 0.0, -1.0, texture(texLinearDepth, uv - dv).x);
  vec3 dtr = vec3( 1.0, -1.0, texture(texLinearDepth, uv - dv + du).x);

  vec3 dml = vec3(-1.0, 0.0, texture(texLinearDepth, uv - du).x);
  vec3 dmc = vec3( 0.0, 0.0, texture(texLinearDepth, uv).x);
  vec3 dmr = vec3( 1.0, 0.0, texture(texLinearDepth, uv + du).x);

  vec3 dbl = vec3(-1.0, 1.0, texture(texLinearDepth, uv + dv - du).x);
  vec3 dbc = vec3( 0.0, 1.0, texture(texLinearDepth, uv + dv).x);
  vec3 dbr = vec3( 1.0, 1.0, texture(texLinearDepth, uv + dv + du).x);

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

vec4 sampleColor(sampler2D tex, vec2 uv) {
#if USE_YCOCG
  vec4 c = texture(tex, uv);
  return vec4(rgbToYcocg(c.rgb), c.a);
#else // USE_YCOCG
  return texture(tex, uv);
#endif // USE_YCOCG
}

vec4 resolveColor(vec4 c) {
#if USE_YCOCG
  return vec4(ycocgToRgb(c.rgb), c.a);
#else // USE_YCOCG
  return c;
#endif // USE_YCOCG
}

vec4 clipAabb(vec3 aabbMin, vec3 aabbMax, vec4 p, vec4 q) {
#if USE_OPTIMIZATIONS
  // note: only clips towards aabb center (but fast!)
  vec3 pClip = 0.5 * (aabbMax + aabbMin);
  vec3 eClip = 0.5 * (aabbMax - aabbMin) + FltEps;

  vec4 vClip = q - vec4(pClip, p.w);
  vec3 vUnit = vClip.xyz / eClip;
  vec3 aUnit = abs(vUnit);
  float maUnit = max(aUnit.x, max(aUnit.y, aUnit.z));

  if (maUnit > 1.0) {
    return vec4(pClip, p.w) + vClip / maUnit;
  }
  else {
    // point inside aabb
    return q;
  }
#else // USE_OPTIMIZATIONS
  vec4 r = q - p;
  vec3 rmax = aabbMax - p.xyz;
  vec3 rmin = aabbMin - p.xyz;

  if (r.x > rmax.x + FltEps) {
    r *= (rmax.x / r.x);
  }
  if (r.y > rmax.y + FltEps) {
    r *= (rmax.y / r.y);
  }
  if (r.z > rmax.z + FltEps) {
    r *= (rmax.z / r.z);
  }

  if (r.x < rmin.x - FltEps) {
    r *= (rmin.x / r.x);
  }
  if (r.y < rmin.y - FltEps) {
    r *= (rmin.y / r.y);
  }
  if (r.z < rmin.z - FltEps) {
    r *= (rmin.z / r.z);
  }

  return p + r;
#endif // USE_OPTIMIZATIONS
}

vec2 sampleVelocityDilated(sampler2D tex, vec2 uv, int support) {
  vec2 du = vec2(texelSize.x, 0.0);
  vec2 dv = vec2(0.0, texelSize.y);
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

vec4 sampleColorMotion(sampler2D tex, vec2 uv, vec2 ssVel) {
  const int Taps = 3; // on either side!
  vec2 v = 0.5 * ssVel;

  vec2 vtap = v / Taps;
  vec2 pos0 = uv + vtap * 0.5 * PDsrand(uv + vec2(time));
  vec4 accu = sampleColor(tex, uv);
  float wsum = 1.0;

  for (int i = -Taps; i <= Taps; i++) {
    vec2 uv = pos0 + i * vtap;
    float w = Taps - abs(i) + 1; // triangle
    accu += w * sampleColor(tex, pos0 + i * vtap);
    wsum += w;
  }

  return accu / wsum;
}

// McGuire2012Blur
float cone(float d, float t) {
  return clamp(1.0 - d / t, 0.0, 1.0);
}

float cylinder(float d, float t) {
  return 1.0 - smoothstep(0.95 * t, 1.05 * t, d);
}

float soft_depth_compare(float z_a, float z_b) {
  const float SoftZExtent = -0.05;
  return clamp(1.0 - (z_a - z_b) / SoftZExtent, 0.0, 1.0);
}

vec4 reconstructColor(sampler2D colorTex, vec2 uv0, vec2 ssVelMax) {
  const int Taps = 3; // on either side!

  if (length(ssVelMax) < FltEps) {
    return sampleColor(colorTex, uv0);
  }

  float srand = PDsrand(uv0 + vec2(time));
  vec2 vtap = 0.5 * ssVelMax / Taps;
  vec2 p0 = uv0 + vtap * (0.5 * srand);
  float d0 = texture(texLinearDepth, p0).x;
  float v0 = length(texture(texVel, p0).xy);
  float w = 1.0;
  float wsum = w;
  vec4 accu = w * sampleColor(colorTex, p0);

  for (int i = 1; i <= Taps; i++) {
    vec2 uv = p0 - i * vtap; // Round to nearest?
    float d = texture(texLinearDepth, uv).x;
    float v = length(texture(texVel, uv).xy);
    float f = soft_depth_compare(d0, d);
    float b = soft_depth_compare(d, d0);
    float duv = length(uv0 - uv);

    w = f * cone(duv, v) + b * cone(duv, v0) + cylinder(duv, v) * cylinder(duv, v0) * 2.0;
    wsum += w;
    accu += w * sampleColor(colorTex, uv);
  }

  for (int i = 1; i <= Taps; i++) {
    vec2 uv = p0 + i * vtap; // Round to nearest?
    float d = texture(texLinearDepth, uv).x;
    float v = length(texture(texVel, uv).xy);
    float f = soft_depth_compare(d0, d);
    float b = soft_depth_compare(d, d0);
    float duv = length(uv0 - uv);

    w = f * cone(duv, v) + b * cone(duv, v0) + cylinder(duv, v) * cylinder(duv, v0) * 2.0;
    wsum += w;
    accu += w * sampleColor(colorTex, uv);
  }

  return accu / wsum;
}

vec4 temporalReprojection(vec2 ssTxc, vec2 ssVel, float vsDist) {
  // read texels
#if UNJITTER_COLORSAMPLES
  vec4 texel0 = sampleColor(texMain, ssTxc - jitterUv.xy);
#else // UNJITTER_COLORSAMPLES
  vec4 texel0 = sampleColor(texMain, ssTxc);
#endif // UNJITTER_COLORSAMPLES

#if UNJITTER_PREV_SAMPLES
  vec4 texel1 = sampleColor(texPrev, ssTxc - jitterUv.zw - ssVel);
#else // UNJITTER_PREV_SAMPLES
  vec4 texel1 = sampleColor(texPrev, ssTxc - ssVel);
#endif // UNJITTER_PREV_SAMPLES

  // calc min-max of current neighbourhood
#if UNJITTER_NEIGHBORHOOD
  vec2 uv = ssTxc - jitterUv.xy;
#else // UNJITTER_NEIGHBORHOOD
  vec2 uv = ssTxc;
#endif // UNJITTER_NEIGHBORHOOD

#if MINMAX_3X3 || MINMAX_3X3_ROUNDED
  vec2 du = vec2(texelSize.x, 0.0);
  vec2 dv = vec2(0.0, texelSize.y);

  vec4 ctl = sampleColor(texMain, uv - dv - du);
  vec4 ctc = sampleColor(texMain, uv - dv);
  vec4 ctr = sampleColor(texMain, uv - dv + du);
  vec4 cml = sampleColor(texMain, uv - du);
  vec4 cmc = sampleColor(texMain, uv);
  vec4 cmr = sampleColor(texMain, uv + du);
  vec4 cbl = sampleColor(texMain, uv + dv - du);
  vec4 cbc = sampleColor(texMain, uv + dv);
  vec4 cbr = sampleColor(texMain, uv + dv + du);

  vec4 cmin = min(
    ctl, min(ctc, min(ctr, min(cml, min(cmc, min(cmr, min(cbl, min(cbc, cbr)))))))
  );
  vec4 cmax = max(
    ctl, max(ctc, max(ctr, max(cml, max(cmc, max(cmr, max(cbl, max(cbc, cbr)))))))
  );

#if MINMAX_3X3_ROUNDED || USE_YCOCG || USE_CLIPPING
  vec4 cavg = (ctl + ctc + ctr + cml + cmc + cmr + cbl + cbc + cbr) / 9.0;
#endif // MINMAX_3X3_ROUNDED || USE_YCOCG || USE_CLIPPING

#if MINMAX_3X3_ROUNDED
  vec4 cmin5 = min(ctc, min(cml, min(cmc, min(cmr, cbc))));
  vec4 cmax5 = max(ctc, max(cml, max(cmc, max(cmr, cbc))));
  vec4 cavg5 = (ctc + cml + cmc + cmr + cbc) / 5.0;
  cmin = 0.5 * (cmin + cmin5);
  cmax = 0.5 * (cmax + cmax5);
  cavg = 0.5 * (cavg + cavg5);
#endif // MINMAX_3X3_ROUNDED

// this is the method used in v2 (PDTemporalReprojection2)
#elif MINMAX_4TAP_VARYING
  const float SubpixelThreshold = 0.5;
  const float GatherBase = 0.5;
  const float GatherSubpixelMotion = 0.1666;

  vec2 texelVel = ssVel * texelSize.xy;
  float texelVelMag = length(texelVel) * vsDist;
  float kSubpixelMotion = clamp(SubpixelThreshold / (FltEps + texelVelMag), 0.0, 1.0);
  float kMinMaxSupport = GatherBase + GatherSubpixelMotion * kSubpixelMotion;

  vec2 ssOffset01 = kMinMaxSupport * vec2(-texelSize.x, texelSize.y);
  vec2 ssOffset11 = kMinMaxSupport * vec2(texelSize.x, texelSize.y);
  vec4 c00 = sampleColor(texMain, uv - ssOffset11);
  vec4 c10 = sampleColor(texMain, uv - ssOffset01);
  vec4 c01 = sampleColor(texMain, uv + ssOffset01);
  vec4 c11 = sampleColor(texMain, uv + ssOffset11);

  vec4 cmin = min(c00, min(c10, min(c01, c11)));
  vec4 cmax = max(c00, max(c10, max(c01, c11)));

#if USE_YCOCG || USE_CLIPPING
  vec4 cavg = (c00 + c10 + c01 + c11) / 4.0;
#endif // USE_YCOCG || USE_CLIPPING
#endif // MINMAX_4TAP_VARYING

  // shrink chroma min-max
#if USE_YCOCG
  vec2 chroma_extent = vec2(0.25 * 0.5 * (cmax.r - cmin.r));
  vec2 chroma_center = texel0.gb;
  cmin.yz = chroma_center - chroma_extent;
  cmax.yz = chroma_center + chroma_extent;
  cavg.yz = chroma_center;
#endif // USE_YCOCG

  // clamp to neighbourhood of current sample
#if USE_CLIPPING
  texel1 = clipAabb(cmin.xyz, cmax.xyz, clamp(cavg, cmin, cmax), texel1);
#else // USE_CLIPPING
  texel1 = clamp(texel1, cmin, cmax);
#endif // USE_CLIPPING

  // feedback weight from unbiased luminance diff (t.lottes)
#if USE_YCOCG
  float lum0 = texel0.r;
  float lum1 = texel1.r;
#else // USE_YCOCG
  float lum0 = luminance(texel0.rgb);
  float lum1 = luminance(texel1.rgb);
#endif // USE_YCOCG

  float unbiasedDiff = abs(lum0 - lum1) / max(lum0, max(lum1, 0.2));
  float unbiasedWeight = 1.0 - unbiasedDiff;
  float k_feedback = mix(feedbackMin, feedbackMax, unbiasedWeight * unbiasedWeight);

  return mix(texel0, texel1, k_feedback);
}


void main() {
#if UNJITTER_REPROJECTION
  vec2 uv = in_data.texCoords - jitterUv.xy;
#else // UNJITTER_REPROJECTION
  vec2 uv = in_data.texCoords;
#endif // UNJITTER_REPROJECTION

#if USE_DILATION
  //--- 3x3 norm (sucks)
  //vec2 ssVel = sampleVelocityDilated(_VelocityBuffer, uv, 1);
  //float vsDist = texture(texLinearDepth, uv).x;

  //--- 5 tap nearest (decent)
  //vec3 c_frag = find_closest_fragment_5tap(uv);
  //vec2 ssVel = tex2D(_VelocityBuffer, c_frag.xy).xy;
  //float vsDist = depth_resolve_linear(c_frag.z);

  //--- 3x3 nearest (good)
  vec3 c_frag = findClosestFragment(uv);
  vec2 ssVel = texture(texVel, c_frag.xy).xy;
  float vsDist = c_frag.z;
#else // USE_DILATION
  vec2 ssVel = texture(texVel, uv).xy;
  float vsDist = texture(texLinearDepth, uv).x;
#endif // USE_DILATION

  // temporal resolve
  vec4 colorTemporal = temporalReprojection(uv, ssVel, vsDist);

  // prepare outputs
  vec4 toBuffer = resolveColor(colorTemporal);

#if USE_MOTION_BLUR
#if USE_MOTION_BLUR_NEIGHBORMAX
  ssVel = motionScale * texture(texVelNeighbormax, uv).xy;
#else // USE_MOTION_BLUR_NEIGHBORMAX
  ssVel = motionScale * ssVel;
#endif // USE_MOTION_BLUR_NEIGHBORMAX

  float velMag = length(ssVel * texelSize.zw);
  const float VelTrustFull = 2.0;  // 2.0
  const float VelTrustNone = 15.0; // 15.0
  const float VelTrustSpan = VelTrustNone - VelTrustFull;
  float trust = 1.0 - clamp(velMag - VelTrustFull, 0.0, VelTrustSpan) / VelTrustSpan;

#if UNJITTER_COLORSAMPLES
  //vec4 colorMotion = reconstructColor(texMain, uv, ssVel);
  vec4 colorMotion = sampleColorMotion(texMain, uv, ssVel);
#else // UNJITTER_COLORSAMPLES
  //vec4 colorMotion = reconstructColor(texMain, uv - jitterUv.xy, ssVel);
  vec4 colorMotion = sampleColorMotion(texMain, uv - jitterUv.xy, ssVel);
#endif // UNJITTER_COLORSAMPLES

  vec4 toScreen = resolveColor(mix(colorMotion, colorTemporal, trust));
#else // USE_MOTION_BLUR
  vec4 toScreen = resolveColor(colorTemporal);
#endif // USE_MOTION_BLUR

  outBuff = toBuffer;
  outFrag = toScreen;
}
