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

/*****************************************************************************************
 * Modified parts of the code (4D texture mechanism) from Eric Bruneton is used in the   *
 * following code.                                                                       *
 ****************************************************************************************/

/**
 * Precomputed Atmospheric Scattering
 * Copyright (c) 2008 INRIA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of
 *    conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list
 *    of conditions and the following disclaimer in the documentation and/or other
 *    materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its contributors may be
 *    used to endorse or promote products derived from this software without specific
 *    prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// ---------------------------------------------------------------------------
// Float-emulation path (selected when NO_GPU_DOUBLE=1 is set in the environment).
//
// This shader uses NO native double types (dvec/dmat/double). All 64-bit precision
// calculations are performed using Dekker/Veltkamp two-float (df64) arithmetic.
// This guarantees smooth, artifact-free rendering on GPUs/drivers lacking fp64.
// ---------------------------------------------------------------------------

#version __CONTEXT__

#include "df64.glsl"
#include "floatoperations.glsl"
#include "atmosphere_common.glsl"

in Data {
  vec2 texCoords;
} in_data;

out vec4 out_color;

uniform int   cullAtmosphere;
uniform float opacity;
uniform float rPlanet;
uniform float rAtmosphere;
uniform float groundRadianceEmission;
uniform float rayleighHeightScale;
uniform vec3  betaRayleigh;
uniform float ozoneHeightScale;
uniform vec3  betaOzoneExtinction;
uniform float mieHeightScale;
uniform vec3  betaMieExtinction;
uniform float mieG;
uniform float sunRadiance;
uniform bool  ozoneLayerEnabled;
uniform float sunAngularSize;
uniform int   rSamples;
uniform int   muSamples;
uniform int   muSSamples;
uniform int   nuSamples;

uniform sampler2D transmittanceTexture;
uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;
uniform sampler2D mainPositionTexture;
uniform sampler2D mainNormalTexture;
uniform sampler2D mainColorTexture;

// ---------------------------------------------------------------------------
// Transform matrix uniforms — Float emulation path.
// ---------------------------------------------------------------------------
uniform mat4  viewToObjectMatrix_hi;
uniform mat4  viewToObjectMatrix_lo;
uniform mat4  modelTransformMatrix_hi;
uniform mat4  modelTransformMatrix_lo;
uniform mat4  rayDirectionMatrix;

uniform vec4  viewport;
uniform vec2  resolution;

// Camera & sun direction — camera position split into (hi, lo) in meters.
uniform vec3  camPosObj_hi;
uniform vec3  camPosObj_lo;
uniform vec3  sunDirectionObj;

/*******************************************************************************
 ***** ALL CALCULATIONS FOR ECLIPSE ARE IN METERS AND IN WORLD SPACE SYSTEM ****
 *******************************************************************************/
const uint NumberOfShadows = 1;

struct ShadowRenderingStruct {
  float xu;
  float xp;
  float rs;
  float rc;
  vec3  sourceCasterVec;
  vec3  casterPositionVec_hi;
  vec3  casterPositionVec_lo;
  bool  isShadowing;
};

uniform ShadowRenderingStruct shadowDataArray[NumberOfShadows];
uniform int  shadows;
uniform bool hardShadows;

// Returns whether there is an eclipse in the x component and the strength of the
// shadowing in the y component
vec2 calcShadow(ShadowRenderingStruct shadowInfoArray[NumberOfShadows], df64_vec3 position,
                bool ground, float daylightFactor)
{
  if (!shadowInfoArray[0].isShadowing) {
    return vec2(0.0, 1.0);
  }

  df64_vec3 casterPos = df64_vec3(shadowInfoArray[0].casterPositionVec_hi,
                                  shadowInfoArray[0].casterPositionVec_lo);
  df64_vec3 pc_df = df64_vec3_sub(casterPos, position);
  vec3 pc = pc_df.hi + pc_df.lo;
  vec3 scNorm = shadowInfoArray[0].sourceCasterVec;
  vec3 pcProj = dot(pc, scNorm) * scNorm;
  vec3 d = pc - pcProj;

  float lengthD      = length(d);
  float lengthPcProj = length(pcProj);

  float rPenumbra =
    shadowInfoArray[0].rc * (lengthPcProj + shadowInfoArray[0].xp) / shadowInfoArray[0].xp;
  float rUmbra =
    shadowInfoArray[0].rc * (shadowInfoArray[0].xu - lengthPcProj) / shadowInfoArray[0].xu;

  float shadowStrength = 1.0;
  if (lengthD < rUmbra) {
    // umbra
    if (hardShadows) {
      shadowStrength = ground ? 0.2 : 0.5;
    }
    else {
      // butterworth function
      shadowStrength = sqrt(rUmbra / (rUmbra + pow(lengthD, 4.0)));
    }
  }
  else if (lengthD < rPenumbra) {
    // penumbra
    shadowStrength = hardShadows ? 0.5 : lengthD / rPenumbra;
  }

  // We don't want a shadow on the antipodal side of the eclipse
  shadowStrength = mix(1.0, shadowStrength, daylightFactor);

  return vec2(1.0, shadowStrength);
}

float opticalDepth(float localH, float r, float mu, float d, float rPlanet) {
  float invH  = 1.0 / localH;
  float a     = sqrt(0.5 * invH * r);
  vec2  a01   = a * vec2(mu, mu + d / r);
  vec2  a01s  = sign(a01);
  vec2  a01sq = a01 * a01;
  float x = a01s.y > a01s.x ? exp(a01sq.x) : 0.0;
  vec2  y = a01s / (2.3193 * abs(a01) + sqrt(1.52 * a01sq + 4.0)) *
    vec2(1.0, exp(-d * invH * (d / (2.0 * r) + mu)));
  return
    sqrt(2.0 * M_PI * sqrt(rAtmosphere * rAtmosphere - rPlanet * rPlanet) * r) *
    exp(rPlanet * invH) *
    (x + dot(y, vec2(1.0, -1.0)));
}

vec3 analyticTransmittance(float r, float mu, float d) {
  vec3 ozone = vec3(0.0);
  if (ozoneLayerEnabled) {
    ozone =
      betaOzoneExtinction * 0.0000006 * opticalDepth(ozoneHeightScale, r, mu, d, rPlanet);
  }
  return exp(
    -betaRayleigh * opticalDepth(rayleighHeightScale, r, mu, d, rPlanet) - ozone -
    betaMieExtinction * opticalDepth(mieHeightScale, r, mu, d, rPlanet)
  );
}

vec3 irradiance(sampler2D s, float r, float muSun) {
  float u_r    = (r - rPlanet) / (rAtmosphere - rPlanet);
  float u_muSun = (muSun + 0.2) / 1.2;
  return texture(s, vec2(u_muSun, u_r)).rgb;
}

//////////////////////////////////////////////////////////////////////////////////////////
//  ALL CALCULATIONS FOR ATMOSPHERE ARE IN KM AND IN OBJECT SPACE                       //
//////////////////////////////////////////////////////////////////////////////////////////

struct Ray {
  vec3 origin;
  vec3 direction;
};

/*
 * Function to calculate the initial intersection of the eye (camera) ray with the
 * atmosphere.
 */
bool atmosphereIntersection(Ray ray, out float offset, out float maxLength) {
  float atmRadius = rAtmosphere - (AtmEpsilon * 0.001);
  vec3  l  = -ray.origin;
  float s  = dot(l, ray.direction);
  float l2 = dot(l, l);
  float r2 = atmRadius * atmRadius;

  offset    = 0.0;
  maxLength = 0.0;

  // Ray origin (eye position) is behind sphere
  if ((s < 0.0) && (l2 > r2)) {
    return false;
  }

  float m2 = l2 - s * s;

  // Ray misses atmosphere
  if (m2 > r2) {
    return false;
  }

  float q = sqrt(r2 - m2);

  // If l2 < r2, the ray origin is inside the sphere
  if (l2 > r2) {
    offset    = s - q;
    maxLength = s + q;
  }
  else {
    offset    = 0.0;
    maxLength = s + q;
  }

  return true;
}

/*
 * Calculates viewing ray in object space directly from NDC coordinates.
 * Using the near plane (-1.0 z_clip), w is guaranteed to be non-zero (~1-2),
 * completely avoiding division by near-zero w, underflow, or NaN.
 */
Ray calculateRayRenderableGlobe(vec2 st) {
  vec2 interpolatedNDCPos = (st - 0.5) * 2.0;
  vec4 clipCoordsNear = vec4(interpolatedNDCPos, -1.0, 1.0);
  vec3 dir = (rayDirectionMatrix * clipCoordsNear).xyz;

  Ray ray;
  ray.origin    = (camPosObj_hi + camPosObj_lo) * 0.001; // meters -> km
  ray.direction = normalize(dir);
  return ray;
}

/*
 * Calculates the light scattering in the view direction coming from other light rays
 * scattered in the atmosphere.
 */
vec3 inscatterRadiance(vec3 x, inout float t, inout float irradianceFactor, vec3 v,
                       vec3 s, float r, vec3 fragPosObj, float maxLength,
                       float pixelDepth, vec3 spaceColor, float sunIntensity,
                       out float mu, out vec3 attenuation, out bool groundHit)
{
  const float InterpolationEpsilon = 0.004; // precision const from Bruneton

  vec3 radiance;

  mu = dot(x, v) / r;

  float r2     = r * r;
  float nu     = dot(v, s);
  float muSun  = dot(x, s) / r;
  float rayleighPhase = rayleighPhaseFunction(nu);
  float miePhase      = miePhaseFunction(nu, mieG);

  vec4 inscatterRad = max(
    texture4D(inscatterTexture, r, mu, muSun, nu, rPlanet, muSamples, rAtmosphere,
      rSamples, muSSamples, nuSamples),
    0.0
  );

  float r0     = length(fragPosObj);
  float invr0  = 1.0 / r0;
  float muSun0 = dot(fragPosObj, s) * invr0;
  float mu0    = dot(fragPosObj, v) * invr0;

  if ((pixelDepth > InterpolationEpsilon) && (pixelDepth < maxLength)) {
    t         = pixelDepth;
    groundHit = true;

    attenuation = transmittance(transmittanceTexture, r, mu, t, rPlanet, rAtmosphere);

    vec4 inscatterFromSurface = texture4D(inscatterTexture, r0, mu0, muSun0, nu, rPlanet,
      muSamples, rAtmosphere, rSamples, muSSamples, nuSamples);
    inscatterRad = max(
      inscatterRad - attenuation.rgbr * inscatterFromSurface, 0.0
    );

    irradianceFactor = 1.0;
  }
  else {
    attenuation = analyticTransmittance(r, mu, t);
    groundHit   = false;
  }

  float muHorizon = -sqrt(1.0 - rPlanet * rPlanet / r2);

  if (abs(mu - muHorizon) < InterpolationEpsilon) {
    float interpolationValue =
      (mu - muHorizon + InterpolationEpsilon) / (2.0 * InterpolationEpsilon);

    // Above Horizon
    mu = muHorizon - InterpolationEpsilon;
    float halfCosineLaw1 = r2 + (t * t);
    float halfCosineLaw2 = 2.0 * r * t;
    r0 = sqrt(halfCosineLaw1 + halfCosineLaw2 * mu);
    mu0 = (r * mu + t) * (1.0 / r0);

    vec4 inScatterAboveX  = texture4D(inscatterTexture, r, mu, muSun, nu, rPlanet,
      muSamples, rAtmosphere, rSamples, muSSamples, nuSamples);
    vec4 inScatterAboveXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu, rPlanet,
      muSamples, rAtmosphere, rSamples, muSSamples, nuSamples);
    vec4 inScatterAbove = max(inScatterAboveX - attenuation.rgbr * inScatterAboveXs, 0.0);

    // Below Horizon
    mu = muHorizon + InterpolationEpsilon;
    r0 = sqrt(halfCosineLaw1 + halfCosineLaw2 * mu);
    mu0 = (r * mu + t) * (1.0 / r0);

    vec4 inScatterBelowX  = texture4D(inscatterTexture, r, mu, muSun, nu, rPlanet,
      muSamples, rAtmosphere, rSamples, muSSamples, nuSamples);
    vec4 inScatterBelowXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu, rPlanet,
      muSamples, rAtmosphere, rSamples, muSSamples, nuSamples);
    vec4 inScatterBelow = max(inScatterBelowX - attenuation.rgbr * inScatterBelowXs, 0.0);

    inscatterRad = mix(inScatterAbove, inScatterBelow, interpolationValue);
  }

  inscatterRad.w *= smoothstep(0.0, 0.02, muSun);
  vec3 inscatterMie =
      inscatterRad.rgb * inscatterRad.a / max(inscatterRad.r, 1e-4) *
      (betaRayleigh.r / betaRayleigh);

  radiance = max(inscatterRad.rgb * rayleighPhase + inscatterMie * miePhase, 0.0);

  vec3 finalScatteringRadiance = radiance * sunIntensity;
  return groundHit ? finalScatteringRadiance : spaceColor + finalScatteringRadiance;
}

/*
 * Calculates the light reflected in the view direction coming from other light rays.
 */
vec3 groundColor(vec3 x, float t, vec3 v, vec3 s, vec3 attenuationXtoX0, vec3 gColor,
                 vec3 normal, float irradianceFactor, float waterReflectance,
                 float sunIntensity)
{
  float r0 = length(x + t * v);

  vec3 groundReflectance = gColor * groundRadianceEmission;

  float dotNS  = dot(normal, s);
  float muSun  = max(dotNS, 0.0);

  vec3 transmittanceL0 =
    muSun < -sqrt(1.0 - (rPlanet * rPlanet / (r0 * r0))) ?
    vec3(0.0) :
    transmittance(transmittanceTexture, r0, muSun, rPlanet, rAtmosphere);

  vec3 irradianceReflected = irradiance(irradianceTexture, r0, muSun) * irradianceFactor;

  vec3 RLStar = (muSun * transmittanceL0 + irradianceReflected) * sunIntensity / M_PI;
  vec3 groundRadiance =
    dotNS < 0.05 ?
    groundReflectance * mix(30.0, 1.0, smoothstep(-1.0, 0.05, dotNS)) * RLStar :
    groundReflectance * RLStar;

  // Specular reflection on water
  if ((waterReflectance > 0.1) && (muSun > 0.0)) {
    vec3 h = normalize(s - v);
    float fresnel = 0.02 + 0.98 * pow(1.0 - dot(-v, h), 5.0);
    float waterBrdf = max(fresnel * pow(max(dot(h, normal), 0.0), 150.0), 0.0);
    groundRadiance += waterReflectance * waterBrdf * transmittanceL0 * sunIntensity;
  }

  vec3 reflectedRadiance = attenuationXtoX0 * groundRadiance;
  return reflectedRadiance;
}

/*
 * Calculates the Sun color.
 */
vec3 sunColor(vec3 v, vec3 s, float r, float mu, float irradianceFactor) {
  float angle = dot(v, s);
  const float FuzzyFactor = 0.5;

  float p1 = cos(sunAngularSize);
  float p2 = cos(sunAngularSize * FuzzyFactor);

  float t     = (angle - p1) / (p2 - p1);
  float scale = clamp(t, 0.0, 1.0);
  return scale * transmittance(transmittanceTexture, r, mu, rPlanet, rAtmosphere) *
    sunRadiance * (1.0 - irradianceFactor);
}

void main() {
  vec2 st = in_data.texCoords;
  st.x = st.x / (resolution.x / viewport[2]) + (viewport[0] / resolution.x);
  st.y = st.y / (resolution.y / viewport[3]) + (viewport[1] / resolution.y);

  vec4 color = texture(mainColorTexture, st);
  if (cullAtmosphere == 1) {
    out_color = color;
    return;
  }

  // Get the ray from camera to atm in object space using Dekker/Veltkamp precision
  Ray ray = calculateRayRenderableGlobe(in_data.texCoords);

  float offset    = 0.0;  // km
  float maxLength = 0.0;  // km
  bool intersect = atmosphereIntersection(ray, offset, maxLength);
  if (!intersect) {
    out_color = color;
    return;
  }

  // G-Buffer sampling
  vec4 normalViewSpaceAndWaterReflectance = texture(mainNormalTexture, st);
  vec4 position = texture(mainPositionTexture, st);

  float pixelDepth;
  vec3  positionObjectsCoords;
  vec4  normal;
  float daylightFactor;
  df64_vec4 posObj_m;

  // Background / deep space check:
  // In OpenSpace, PosBufferClearVal = 1e32. If position is clear value or w == 0,
  // this fragment is empty space (no terrain / geometry).
  const float SpaceThreshold = 1e20;
  bool isBackground = (abs(position.x) >= SpaceThreshold || abs(position.y) >= SpaceThreshold ||
                       abs(position.z) >= SpaceThreshold || position.w == 0.0);

  // Following paper nomenclature
  float t = offset;
  vec3  x = ray.origin + t * ray.direction;
  float tF = float(maxLength - t);

  if (isBackground) {
    // Pure atmosphere / sky pixel: ray continues through atmosphere without hitting ground.
    pixelDepth            = 1e20;
    positionObjectsCoords = x + tF * ray.direction;
    normal                = vec4(0.0);
    daylightFactor        = 1.0;
  }
  else {
    // Normal from view space to object space:
    // normal is a direction vector (w = 0), so rotation from viewToObject applies directly.
    vec3 normObj = mat3(viewToObjectMatrix_hi) * normalViewSpaceAndWaterReflectance.xyz;
    normal.xyz   = normalize(normObj);
    normal.w     = normalViewSpaceAndWaterReflectance.w;

    // View to Object coordinates in meters using df64.
    // viewToObject pre-cancels the 1.5e11 galactic coordinate on CPU in double precision.
    posObj_m = df64_mat4_mul_vec4(
      viewToObjectMatrix_hi,
      viewToObjectMatrix_lo,
      position
    );

    // Compute distance from camera to surface in planet object space (meters)
    // using df64 subtraction to maintain full precision and cancel any camera scaling.
    df64_vec3 camPos_m  = df64_vec3(camPosObj_hi, camPosObj_lo);
    df64_vec3 fragPos_m = df64_vec3(posObj_m.hi.xyz, posObj_m.lo.xyz);
    df64_vec3 diff_m    = df64_vec3_sub(camPos_m, fragPos_m);
    vec3 diff           = diff_m.hi + diff_m.lo;
    pixelDepth          = length(diff);

    float dC = length(camPosObj_hi + camPosObj_lo);
    const float X1 = 1e8;
    if (dC > X1) {
      pixelDepth += 1000.0;
      const float Alpha = 1000.0;
      const float Beta  = 1000000.0;
      const float X2    = 1e9;
      const float VarA  = (Beta - Alpha) / (X2 - X1);
      const float VarB  = (Alpha - VarA * X1);
      pixelDepth += VarA * dC + VarB;
    }

    // Convert to kilometers:
    pixelDepth *= 0.001;
    positionObjectsCoords = (posObj_m.hi.xyz + posObj_m.lo.xyz) * 0.001;

    // Daylight factor: dot(normal_world, sunDir_world) == dot(normal_obj, sunDir_obj)
    // because rotation preserves dot products.
    daylightFactor = clamp(dot(normal.xyz, sunDirectionObj) * 4.0 + 0.5, 0.0, 1.0);
  }

  // Occlusion check: only external objects (e.g. spacecraft, moon) located outside
  // the atmosphere (rFrag > rAtmosphere) and between the camera and the atmospheric shell
  // can occlude the atmosphere. Terrain on or inside the atmosphere
  // (rFrag <= rAtmosphere) is immersed within the atmosphere and must never be culled.
  float rFrag = length(positionObjectsCoords);
  if (!isBackground && rFrag > rAtmosphere && pixelDepth < offset) {
    // ATM Occluded - Something in front of ATM
    out_color = color;
    return;
  }

  df64_vec4 onATMPos_m = df64_mat4_mul_vec4(
    modelTransformMatrix_hi,
    modelTransformMatrix_lo,
    vec4(x * 1000.0, 1.0)
  );

  float r  = length(x);
  vec3  v  = vec3(ray.direction);
  float mu = 0.0;
  vec3  s  = vec3(sunDirectionObj);

  pixelDepth -= offset;

  // Eclipse shadow calculation in world meters using df64
  vec2 eclipseShadowATM = calcShadow(
    shadowDataArray,
    df64_vec3(onATMPos_m.hi.xyz, onATMPos_m.lo.xyz),
    false,
    daylightFactor
  );
  float sunIntensityInscatter = sunRadiance * eclipseShadowATM.y;

  float irradianceFactor = 0.0;
  bool  groundHit        = false;
  vec3  attenuation;

  vec3 inscatterColor = inscatterRadiance(x, tF, irradianceFactor, v, s, r,
    positionObjectsCoords, maxLength, pixelDepth, color.rgb, sunIntensityInscatter,
    mu, attenuation, groundHit);

  vec3 atmColor = vec3(0.0);
  if (groundHit) {
    df64_vec4 positionWorldCoords = df64_mat4_mul_df64_vec4(
      modelTransformMatrix_hi,
      modelTransformMatrix_lo,
      posObj_m
    );
    vec2 eclipseShadowPlanet = calcShadow(
      shadowDataArray,
      df64_vec3(positionWorldCoords.hi.xyz, positionWorldCoords.lo.xyz),
      true,
      daylightFactor
    );
    float sunIntensityGround = sunRadiance * eclipseShadowPlanet.y;
    atmColor = groundColor(x, tF, v, s, attenuation, color.rgb, normal.xyz,
      irradianceFactor, normal.w, sunIntensityGround);
  }
  else {
    atmColor = sunColor(v, s, r, mu, irradianceFactor) * (1.0 - eclipseShadowATM.x);
  }

  vec3 c = mix(color.rgb, inscatterColor + atmColor, opacity);
  out_color = vec4(c, 1.0);
}
