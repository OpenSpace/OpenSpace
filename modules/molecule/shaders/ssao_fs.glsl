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

#ifndef AO_NUM_SAMPLES
#define AO_NUM_SAMPLES 32
#endif

in vec2 tc;
out vec4 fragColor;

struct HBAOData {
  float radiusToScreen;
  float negInvR2;
  float nDotVBias;
  float normalBias;

  float aoMultiplier;
  float powExponent;
  vec2 invFullRes;

  vec4 projInfo;

  vec4 samplePattern[32];
};

layout(std140) uniform controlBuffer {
  HBAOData control;
};

uniform sampler2D texLinearDepth;
uniform sampler2D texNormal;
uniform sampler2D texRandom;
uniform bool isPerspective;


vec3 uvToView(vec2 uv, float eyeZ) {
  if (isPerspective) {
    return vec3((uv * control.projInfo.xy + control.projInfo.zw) * eyeZ, eyeZ);
  }
  else {
    return vec3((uv * control.projInfo.xy + control.projInfo.zw), eyeZ);
  }
}

vec3 fetchViewPos(vec2 uv, float lod) {
  float view_depth = textureLod(texLinearDepth, uv, lod).x;
  return uvToView(uv, view_depth);
}

vec3 decodeNormal(vec2 enc) {
  vec2 fenc = enc * 4.0 - 2.0;
  float f = dot(fenc, fenc);
  float g = sqrt(1.0 - f / 4.0);
  return vec3(fenc * g, 1.0 - f / 2.0);
}

vec3 fetchViewNormal(vec2 uv) {
  vec2 enc = texelFetch(texNormal, ivec2(gl_FragCoord.xy), 0).xy;
  vec3 n = decodeNormal(enc);
  return n * vec3(1.0, 1.0, -1.0);
}

float falloff(float dist2) {
  // 1 scalar mad instruction
  return dist2 * control.negInvR2 + 1.0;
}

// P = view-space position at the kernel center
// N = view-space normal at the kernel center
// S = view-space position of the current sample
float computePixelObscurance(vec3 P, vec3 N, vec3 S) {
  vec3 V = S - P;
  float VdotV = dot(V, V);
  float NdotV = dot(N, V) * 1.0 / sqrt(VdotV);

  float falloffMult = max(0.0, falloff(VdotV));
  return max(0.0, NdotV - control.nDotVBias) * falloffMult;
}

vec2 rotateSample(vec2 smpl, vec2 cosSin) {
  return vec2(
    smpl.x * cosSin.x - smpl.y * cosSin.y,
    smpl.x * cosSin.y + smpl.y * cosSin.x
  );
}

vec4 getJitter(vec2 uv) {
  // (cos(Alpha),sin(Alpha),rand1,rand2)
  vec2 coord = gl_FragCoord.xy / AO_RANDOM_TEX_SIZE;
  vec4 jitter = textureLod(texRandom, coord, 0);
  return jitter;
}

float computeAo(vec2 fullResUv, float radiusPixels, vec4 jitter, vec3 viewPosition,
                 vec3 viewNormal)
{
  // -4.3 is recomended in the intel ASSAO implementation
  const float GlobalMipOffset = -4.3;
  float mipOffset = log2(radiusPixels) + GlobalMipOffset;

  float weightSum = 0.0;
  float ao = 0.0;

  vec3 normal = mix(vec3(0.0, 0.0, 1.0), viewNormal, control.normalBias);

  for (int i = 0; i < AO_NUM_SAMPLES; i++) {
    vec4 smpl = control.samplePattern[i];
    vec2 uv = rotateSample(smpl.xy, jitter.xy) * jitter.z;
    float weightScale = smpl.z;
    float mipLevel = mipOffset + smpl.w;

    vec2 snappedUv = round(radiusPixels * uv) * control.invFullRes + fullResUv;
    vec3 viewSample = fetchViewPos(snappedUv, mipLevel);
    ao += computePixelObscurance(viewPosition, viewNormal, viewSample) * weightScale;
    weightSum += 1.0;
  }
  ao *= control.aoMultiplier / weightSum;

  return clamp(1.0 - ao, 0.0, 1.0);
}

//----------------------------------------------------------------------------------
void main() {
  vec2 uv = tc;
  vec3 viewPosition = fetchViewPos(uv, 0);
  vec3 viewNormal = fetchViewNormal(uv);

  // Compute projection of disk of radius control.R into screen space
  float radiusPixels = control.radiusToScreen;
  if (isPerspective) {
    radiusPixels /= viewPosition.z;
  }

  // Get jitter vector for the current full-res pixel
  vec4 jitter = getJitter(uv);
  float ao = computeAo(uv, radiusPixels, jitter, viewPosition, viewNormal);

  fragColor = vec4(vec3(pow(ao, control.powExponent)), 1.0);
}
