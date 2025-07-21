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

#version __CONTEXT__

#include "floatoperations.glsl"
#include "atmosphere_common.glsl"

in vec2 texCoord;

out vec4 renderTarget;

uniform int cullAtmosphere;
uniform float opacity;
uniform float Rg;
uniform float Rt;
uniform float groundRadianceEmission;
uniform float HR;
uniform vec3 betaRayleigh;
uniform float HO;
uniform vec3 betaOzoneExtinction;
uniform float HM;
uniform vec3 betaMieExtinction;
uniform float mieG;
uniform float sunRadiance;
uniform bool ozoneLayerEnabled;
uniform float sunAngularSize;
uniform int SAMPLES_R;
uniform int SAMPLES_MU;
uniform int SAMPLES_MU_S;
uniform int SAMPLES_NU;
uniform sampler2D transmittanceTexture;
uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;
uniform sampler2D mainPositionTexture;
uniform sampler2D mainNormalTexture;
uniform sampler2D mainColorTexture;
uniform dmat4 inverseModelTransformMatrix;
uniform dmat4 modelTransformMatrix;
uniform dmat4 viewToWorldMatrix;
uniform dmat4 projectionToModelTransformMatrix;
uniform vec4 viewport;
uniform vec2 resolution;
uniform dvec3 camPosObj;
uniform dvec3 sunDirectionObj;

/*******************************************************************************
 ***** ALL CALCULATIONS FOR ECLIPSE ARE IN METERS AND IN WORLD SPACE SYSTEM ****
 *******************************************************************************/
// JCC: Remove and use dictionary to decide the number of shadows
const uint numberOfShadows = 1;


struct ShadowRenderingStruct {
  double xu;
  double xp;
  double rs;
  double rc;
  dvec3 sourceCasterVec;
  dvec3 casterPositionVec;
  bool isShadowing;
};

// Eclipse shadow data
// JCC: Remove and use dictionary to decide the number of shadows
uniform ShadowRenderingStruct shadowDataArray[numberOfShadows];
uniform int shadows;
uniform bool hardShadows;

// Returns whether there is an eclipse in the x component and the strength of the
// shadowing in the y component
vec2 calcShadow(ShadowRenderingStruct shadowInfoArray[numberOfShadows], dvec3 position,
                 bool ground)
{
  if (!shadowInfoArray[0].isShadowing) {
    return vec2(0.0, 1.0);
  }

  dvec3 pc = shadowInfoArray[0].casterPositionVec - position;
  dvec3 scNorm = shadowInfoArray[0].sourceCasterVec;
  dvec3 pcProj = dot(pc, scNorm) * scNorm;
  dvec3 d = pc - pcProj;

  float length_d = float(length(d));
  double lengthPcProj = length(pcProj);

  float r_p_pi = float(shadowInfoArray[0].rc * (lengthPcProj + shadowInfoArray[0].xp) / shadowInfoArray[0].xp);
  float r_u_pi = float(shadowInfoArray[0].rc * (shadowInfoArray[0].xu - lengthPcProj) / shadowInfoArray[0].xu);

  if (length_d < r_u_pi) {
    // umbra
    if (hardShadows) {
      return ground  ?  vec2(1.0, 0.2)  :  vec2(1.0, 0.5);
    }
    else {
      // butterworth function
      return vec2(1.0, sqrt(r_u_pi / (r_u_pi + pow(length_d, 4.0))));
    }
  }
  else if (length_d < r_p_pi) {
    // penumbra
    return hardShadows  ?  vec2(1.0, 0.5)  :  vec2(1.0, length_d / r_p_pi);
  }
  else {
    return vec2(1.0, 1.0);
  }
}

float opticalDepth(float localH, float r, float mu, float d, float Rg) {
  float invH = 1.0 / localH;
  float a = sqrt(0.5 * invH * r);
  vec2 a01 = a * vec2(mu, mu + d / r);
  vec2 a01s = sign(a01);
  vec2 a01sq = a01 * a01;
  float x = a01s.y > a01s.x ? exp(a01sq.x) : 0.0;
  vec2 y = a01s / (2.3193 * abs(a01) + sqrt(1.52 * a01sq + 4.0)) *
    vec2(1.0, exp(-d * invH * (d / (2.0 * r) + mu)));
  return sqrt(2.0 * M_PI * sqrt(Rt*Rt - Rg*Rg) * r) * exp((Rg-r)*invH) * (x + dot(y, vec2(1.0, -1.0)));
}

vec3 analyticTransmittance(float r, float mu, float d) {
  vec3 ozone = vec3(0.0);
  if (ozoneLayerEnabled) {
    ozone = betaOzoneExtinction * 0.0000006 * opticalDepth(HO, r, mu, d, Rg);
  }
  return exp(-betaRayleigh * opticalDepth(HR, r, mu, d, Rg) - ozone -
    betaMieExtinction * opticalDepth(HM, r, mu, d, Rg));
}

vec3 irradiance(sampler2D s, float r, float muSun) {
  float u_r = (r - Rg) / (Rt - Rg);
  float u_muSun = (muSun + 0.2) / 1.2;
  return texture(s, vec2(u_muSun, u_r)).rgb;
}

//////////////////////////////////////////////////////////////////////////////////////////
//  ALL CALCULATIONS FOR ATMOSPHERE ARE KM AND IN WORLD SPACE SYSTEM                    //
//////////////////////////////////////////////////////////////////////////////////////////

struct Ray {
  dvec3 origin;
  dvec3 direction;
};

/*
 * Function to calculate the initial intersection of the eye (camera) ray with the
 * atmosphere.
 * In (all parameters in the same coordinate system and same units):
 *   - ray direction (normalized)
 *   - atmosphere radius
 * Out: true if an intersection happens, false otherwise
 *   - return: true if the ray origin is inside atmosphere, false otherwise
 *   - offset: the initial intersection distance from eye position when the eye is outside
 *             the atmosphere
 *   - maxLength: the second intersection distance from eye position when the eye is
 *                outside the atmosphere or the initial (and only) intersection of the ray
 *                with atmosphere when the eye position is inside atmosphere.
 */
bool atmosphereIntersection(Ray ray, double atmRadius, out double offset,
                            out double maxLength)
{
  dvec3 l = -ray.origin;
  double s = dot(l, ray.direction);
  double l2 = dot(l, l);
  double r2 = atmRadius * atmRadius; // avoiding surface acne

  offset = 0.0;
  maxLength = 0.0;

  // Ray origin (eye position) is behind sphere
  if ((s < 0.0) && (l2 > r2)) {
    return false;
  }

  double m2 = l2 - s * s;

  // Ray misses atmosphere
  if (m2 > r2) {
    return false;
  }

  // If q = 0.0, there is only one intersection
  double q = sqrt(r2 - m2);

  // If l2 < r2, the ray origin is inside the sphere
  if (l2 > r2) {
    offset = s - q;
    maxLength = s + q;
  }
  else {
    offset = 0.0;
    maxLength = s + q;
  }

  return true;
}

/*
 * Calculates Intersection Ray by walking through all the graphic pipeline transformations
 * in the opposite direction. Instead of passing through all the pipeline, it starts at
 * NDC from the interpolated positions from the screen quad. This method avoids matrices
 * multiplications wherever is possible.
 */
Ray calculateRayRenderableGlobe(vec2 st) {
  vec2 interpolatedNDCPos = (st - 0.5) * 2.0;
  dvec4 clipCoords = dvec4(interpolatedNDCPos, 1.0, 1.0);

  // Clip to Object Coords
  dvec4 objectCoords = projectionToModelTransformMatrix * clipCoords;
  objectCoords.xyz /= objectCoords.w;

  // Building Ray
  // Ray in object space (in KM)
  Ray ray;
  ray.origin = camPosObj * 0.001;
  ray.direction = normalize(objectCoords.xyz * dvec3(0.001) - ray.origin);
  return ray;
}

/*
 * Calculates the light scattering in the view direction comming from other light rays
 * scattered in the atmosphere.
 * Following the paper:  S[L]|x - T(x,xs) * S[L]|xs
 * The view direction here is the ray: x + tv, s is the sun direction, r and mu the
 * position and zenith cosine angle as in the paper.
 * Arguments:
 * x := camera position
 * t := ray displacement variable after calculating the intersection with the
 *      atmosphere. It is the distance from the camera to the last intersection with the
 *      atmosphere. If the ray hits the ground, t is updated to the correct value
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := out of ||x|| inside atmosphere (or top of atmosphere)
 * mu := out of cosine of the zenith view angle
 * attenuation := out of transmittance T(x,x0). This will be used later when calculating
 *                the reflectance R[L]
 */
vec3 inscatterRadiance(vec3 x, inout float t, inout float irradianceFactor, vec3 v, vec3 s,
                       float r, vec3 fragPosObj, double maxLength, double pixelDepth,
                       vec3 spaceColor, float sunIntensity,
                       out float mu, out vec3 attenuation, out bool groundHit)
{
  const float INTERPOLATION_EPS = 0.004; // precision const from Brunetton

  vec3 radiance;

  mu = dot(x, v) / r;

  float r2 = r * r;
  float nu = dot(v, s);
  float muSun = dot(x, s) / r;
  float rayleighPhase = rayleighPhaseFunction(nu);
  float miePhase = miePhaseFunction(nu, mieG);

  // S[L](x,s,v)
  // I.e. the next line has the scattering light for the "infinite" ray passing through
  // the atmosphere. If this ray hits something inside the atmosphere, we will subtract
  // the attenuated scattering light from that path in the current path
  vec4 inscatterRadiance = max(
    texture4D(inscatterTexture, r, mu, muSun, nu, Rg, SAMPLES_MU, Rt, SAMPLES_R,
      SAMPLES_MU_S, SAMPLES_NU),
    0.0
  );

  // After removing the initial path from camera pos to top of atmosphere (for an
  // observer in the space) we test if the light ray is hitting the atmosphere
  float r0 = length(fragPosObj);
  float invr0 = 1.0 / r0;
  float muSun0 = dot(fragPosObj, s) * invr0;
  float mu0 = dot(fragPosObj, v) * invr0;

  if ((pixelDepth > INTERPOLATION_EPS) && (pixelDepth < maxLength)) {
    t = float(pixelDepth);
    groundHit = true;

    // Transmittance from point r, direction mu, distance t
    // By Analytical calculation
    // attenuation = analyticTransmittance(r, mu, t);
    // JCC: change from analytical to LUT transmittance to avoid
    // acme on planet surface when looking from far away. (11/02/2017)
    attenuation = transmittance(transmittanceTexture, r, mu, t, Rg, Rt);

    // Here we use the idea of S[L](a->b) = S[L](b->a), and get the S[L](x0, v, s)
    // Then we calculate S[L] = S[L]|x - T(x, x0)*S[L]|x0
    // The "infinite" ray hist something inside the atmosphere, so we need to remove
    // the unsused contribution to the final radiance.
    vec4 inscatterFromSurface = texture4D(inscatterTexture, r0, mu0, muSun0, nu, Rg,
      SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU);
    inscatterRadiance = max(inscatterRadiance - attenuation.rgbr * inscatterFromSurface, 0.0);

    // We set the irradianceFactor to 1.0 so the reflected irradiance will be considered
    // when calculating the reflected light on the ground.
    irradianceFactor = 1.0;
  }
  else {
    attenuation = analyticTransmittance(r, mu, t);
    groundHit = false;
  }

  // cos(PI-thetaH) = dist/r
  // cos(thetaH) = -dist/r
  // muHorizon = -sqrt(r^2-Rg^2)/r = -sqrt(1-(Rg/r)^2)
  float muHorizon = -sqrt(1.0 - Rg*Rg / r2);

  // In order to avoid precision problems near horizon, we interpolate between two
  // points: above and below horizon
  if (abs(mu - muHorizon) < INTERPOLATION_EPS) {
    // We want an interpolation value close to 1/2, so the contribution of each radiance
    // value is almost the same or it has a heavy weight if from above or below horizon
    float interpolationValue = (mu - muHorizon + INTERPOLATION_EPS) / (2.0 * INTERPOLATION_EPS);

    // Above Horizon
    mu = muHorizon - INTERPOLATION_EPS;
    // r0  = sqrt(r * r + t * t + 2.0 * r * t * mu);
    // From cosine law where t = distance between x and x0
    // r0^2 = r^2 + t^2 - 2 * r * t * cos(PI-theta)
    // r0  = sqrt(r2 + t2 + 2.0 * r * t * mu);
    float halfCosineLaw1 = r2 + (t * t);
    float halfCosineLaw2 = 2.0 * r * t;
    r0 = sqrt(halfCosineLaw1 + halfCosineLaw2 * mu);

    // From the dot product: cos(theta0) = (x0 dot v)/(||ro||*||v||)
    // mu0 = ((x + t) dot v) / r0
    // mu0 = (x dot v + t dot v) / r0
    // mu0 = (r*mu + t) / r0
    mu0 = (r * mu + t) * (1.0 / r0);

    vec4 inScatterAboveX = texture4D(inscatterTexture, r, mu, muSun, nu, Rg,
      SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU);
    vec4 inScatterAboveXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu, Rg,
      SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU);
    // Attention for the attenuation.r value applied to the S_Mie
    vec4 inScatterAbove = max(inScatterAboveX - attenuation.rgbr * inScatterAboveXs, 0.0);

    // Below Horizon
    mu = muHorizon + INTERPOLATION_EPS;
    //r0  = sqrt(r2 + t2 + 2.0 * r * t * mu);
    r0 = sqrt(halfCosineLaw1 + halfCosineLaw2 * mu);

    mu0 = (r * mu + t) * (1.0 / r0);

    vec4 inScatterBelowX = texture4D(inscatterTexture, r, mu, muSun, nu, Rg,
      SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU);
    vec4 inScatterBelowXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu, Rg,
      SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU);
    // Attention for the attenuation.r value applied to the S_Mie
    vec4 inScatterBelow = max(inScatterBelowX - attenuation.rgbr * inScatterBelowXs, 0.0);

    // Interpolate between above and below inScattering radiance
    inscatterRadiance = mix(inScatterAbove, inScatterBelow, interpolationValue);
  }

  // The w component of inscatterRadiance has stored the Cm,r value (Cm = Sm[L0])
  // So, we must reintroduce the Mie inscatter by the proximity rule as described in the
  // paper by Bruneton and Neyret in "Angular precision" paragraph:

  // Hermite interpolation between two values
  // This step is done because imprecision problems happen when the Sun is slightly
  // below the horizon. When this happens, we avoid the Mie scattering contribution
  inscatterRadiance.w *= smoothstep(0.0, 0.02, muSun);
  vec3 inscatterMie =
      inscatterRadiance.rgb * inscatterRadiance.a / max(inscatterRadiance.r, 1e-4) *
      (betaRayleigh.r / betaRayleigh);

  radiance = max(inscatterRadiance.rgb * rayleighPhase + inscatterMie * miePhase, 0.0);

  // Finally we add the Lsun (all calculations are done with no Lsun so we can change it
  // on the fly with no precomputations)
  vec3 finalScatteringRadiance = radiance * sunIntensity;
  return groundHit ?  finalScatteringRadiance  :  spaceColor + finalScatteringRadiance;
}

/*
 * Calculates the light reflected in the view direction comming from other light rays
 * integrated over the hemispehre plus the direct light (L0) from Sun.
 * Following the paper: R[L]= R[L0]+R[L*]
 * The ray is x + tv, v the view direction, s is the sun direction, r and mu the position
 * and zenith cosine angle as in the paper.
 * As for all calculations in the atmosphere, the center of the coordinate system is the
 * planet's center of coordiante system, i.e., the planet's position is (0,0,0).
 * Arguments:
 * x := camera position
 * t := ray displacement variable. Here, differently from the inScatter light calculation,
 *      the position of the camera is already offset (on top of atmosphere) or inside
 *      the atmosphere
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * mu := cosine of the zenith view angle
 * attenuationXtoX0 := transmittance T(x,x0)
 */
vec3 groundColor(vec3 x, float t, vec3 v, vec3 s, vec3 attenuationXtoX0, vec3 groundColor,
                 vec3 normal, float irradianceFactor, float waterReflectance,
                 float sunIntensity)
{
  // First we obtain the ray's end point on the surface
  float r0 = length(x + t * v);

  vec3 groundReflectance = groundColor * groundRadianceEmission;

  // L0 is not included in the irradiance texture.
  // We first calculate the light attenuation from the top of the atmosphere to x0
  float dotNS = dot(normal, s);
  float muSun = max(dotNS, 0.0);

  // Is direct Sun light arriving at x0? If not, there is no direct light from Sun (shadowed)
  vec3 transmittanceL0 =
    muSun < -sqrt(1.0 - (Rg*Rg / (r0 * r0)))  ?  vec3(0.0)  :  transmittance(transmittanceTexture, r0, muSun, Rg, Rt);

  // E[L*] at x0
  vec3 irradianceReflected = irradiance(irradianceTexture, r0, muSun) * irradianceFactor;

  // R[L0] + R[L*]
  vec3 RLStar = (muSun * transmittanceL0 + irradianceReflected) * sunIntensity / M_PI;
  vec3 groundRadiance =
    dotNS < 0.05 ?
    groundReflectance * mix(30.0, 1.0, smoothstep(-1.0, 0.05, dotNS)) * RLStar :
    groundReflectance * RLStar;

  // Specular reflection from sun on oceans and rivers
  if ((waterReflectance > 0.1) && (muSun > 0.0)) {
    vec3 h = normalize(s - v);
    // Fresnell Schlick's approximation
    float fresnel = 0.02 + 0.98 * pow(1.0 - dot(-v, h), 5.0);
    // Walter BRDF approximation
    float waterBrdf = max(fresnel * pow(max(dot(h, normal), 0.0), 150.0), 0.0);
    // Adding Fresnell and Water BRDFs approximation to the final surface color
    // after adding the sunRadiance and the attenuation of the Sun through atmosphere
    groundRadiance += waterReflectance * waterBrdf * transmittanceL0 * sunIntensity;
  }

  // Finally, we attenuate the surface Radiance from the point x0 to the camera location
  vec3 reflectedRadiance = attenuationXtoX0 * groundRadiance;
  return reflectedRadiance;
}

/*
 * Calculates the Sun color. The ray is x + tv, v the view direction, s is the sun
 * direction, r and mu the position and zenith cosine angle as in the paper. As for all
 * calculations in the atmosphere, the center of the coordinate system is the planet's
 * center of coordiante system, i.e., the planet's position is (0,0,0). Arguments:
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := ||x|| inside atmosphere (or top of atmosphere). r <= Rt here.
 * mu := cosine of the zenith view angle
 * attenuation := transmittance T(x,x0)
 */
vec3 sunColor(vec3 v, vec3 s, float r, float mu, float irradianceFactor) {
  // v = normalize(vec3(inverseModelTransformMatrix * dvec4(sunWorld, 1.0)));
  float angle = dot(v, s);

  // JCC: Change this function to a impostor texture with gaussian decay color weighted
  // by the sunRadiance, transmittance and irradianceColor (11/03/2017)

  // @TODO (abock, 2021-07-01) This value is hard-coded to our sun+earth right now
  // Convert 0.3 degrees -> radians
  // const float SunAngularSize = (0.3 * M_PI / 180.0);
  const float FuzzyFactor = 0.5; // How fuzzy should the edges be

  float p1 = cos(sunAngularSize);
  float p2 = cos(sunAngularSize * FuzzyFactor);

  float t = (angle - p1) / (p2 - p1);
  float scale = clamp(t, 0.0, 1.0);
  return scale * transmittance(transmittanceTexture, r, mu, Rg, Rt) * sunRadiance * (1.0 - irradianceFactor);
}


void main() {
  // Modify the texCoord based on the Viewport and Resolution. This modification is
  // necessary in case of side-by-side stereo as we only want to access the part of the
  // feeding texture that we are currently responsible for.  Otherwise we would map the
  // entire feeding texture into our half of the result texture, leading to a doubling
  // of the "missing" half.  If you don't believe me, load a configuration file with the
  // side_by_side stereo mode enabled, disable FXAA, and remove this modification.
  // The same calculation is done in the FXAA shader and the HDR resolving
  vec2 st = texCoord;
  st.x = st.x / (resolution.x / viewport[2]) + (viewport[0] / resolution.x);
  st.y = st.y / (resolution.y / viewport[3]) + (viewport[1] / resolution.y);

  // Color from G-Buffer
  vec4 color = texture(mainColorTexture, st);
  if (cullAtmosphere == 1) {
    renderTarget = color;
    return;
  }

  // Get the ray from camera to atm in object space
  Ray ray = calculateRayRenderableGlobe(texCoord);

  double offset = 0.0;   // in KM
  double maxLength = 0.0;   // in KM
  bool intersect = atmosphereIntersection(ray, Rt - (ATM_EPSILON * 0.001), offset, maxLength);
  if (!intersect) {
    renderTarget = color;
    return;
  }

  // Now we check is if the atmosphere is occluded, i.e., if the distance to the pixel in
  // the G-Buffer positions is less than the distance to the atmosphere then the
  // atmosphere is occluded. Fragments positions into G-Buffer are written in SGCT Eye
  // Space (View plus Camera Rig Coords) when using their positions later, one must
  // convert them to the planet's coords

  // Normal is stored in view space and transformed to the current object space
  vec4 normalViewSpaceAndWaterReflectance = texture(mainNormalTexture, st);
  dvec4 normalViewSpace = vec4(normalViewSpaceAndWaterReflectance.xyz, 0.0);
  dvec4 normalWorldSpace = viewToWorldMatrix * normalViewSpace;
  vec4 normal = vec4(inverseModelTransformMatrix * normalWorldSpace);
  normal.xyz = normalize(normal.xyz);
  normal.w = normalViewSpaceAndWaterReflectance.w;

  // Data in the mainPositionTexture are written in view space (view plus camera rig)
  vec4 position = texture(mainPositionTexture, st);

  // OS Eye to World coords
  dvec4 positionWorldCoords = viewToWorldMatrix * position;

  // World to Object (Normal and Position in meters)
  dvec3 positionObjectsCoords = (inverseModelTransformMatrix * positionWorldCoords).xyz;

  // Distance of the pixel in the gBuffer to the observer
  // JCC (12/12/2017): AMD distance function is buggy.
  //double pixelDepth = distance(cameraPositionInObject.xyz, positionObjectsCoords.xyz);
  double pixelDepth = length(camPosObj - positionObjectsCoords);

  // JCC (12/13/2017): Trick to remove floating error in texture.
  // We see a squared noise on planet's surface when seeing the planet from far away
  // @TODO (abock, 2021-07-01) I don't think this does anything. Remove?
  float dC = float(length(camPosObj));
  const float x1 = 1e8;
  if (dC > x1) {
    pixelDepth += 1000.0;
    const float alpha = 1000.0;
    const float beta = 1000000.0;
    const float x2 = 1e9;
    const float diffGreek = beta - alpha;
    const float diffDist = x2 - x1;
    const float varA = diffGreek / diffDist;
    const float varB = (alpha - varA * x1);
    pixelDepth += double(varA * dC + varB);
  }

  // All calculations are done in KM:
  pixelDepth *= 0.001;
  positionObjectsCoords *= 0.001;

  if (pixelDepth < offset) {
    // ATM Occluded - Something in front of ATM
    renderTarget = color;
    return;
  }

  // Following paper nomenclature
  double t = offset;

  // Moving observer from camera location to top atmosphere. If the observer is already
  // inside the atm, offset = 0.0 and no changes at all
  vec3 x = vec3(ray.origin + t * ray.direction);
  float r = length(x);
  vec3 v = vec3(ray.direction);
  float mu = 0.0; // dot(x, v) / r;
  vec3 s = vec3(sunDirectionObj);
  float tF = float(maxLength - t);

  // Because we may move the camera origin to the top of atmosphere we also need to
  // adjust the pixelDepth for tdCalculateRayRenderableGlobe' offset so the next
  // comparison with the planet's ground make sense:
  pixelDepth -= offset;

  dvec3 onATMPos = (modelTransformMatrix * dvec4(x * 1000.0, 1.0)).xyz;
  vec2 eclipseShadowATM = calcShadow(shadowDataArray, onATMPos, false);
  float sunIntensityInscatter = sunRadiance * eclipseShadowATM.y;

  float irradianceFactor = 0.0;

  bool groundHit = false;
  vec3 attenuation;

  vec3 inscatterColor = inscatterRadiance(x, tF, irradianceFactor, v, s, r,
    vec3(positionObjectsCoords), maxLength, pixelDepth, color.rgb, sunIntensityInscatter, mu,
    attenuation, groundHit);
  vec3 atmColor = vec3(0.0);
  if (groundHit) {
    vec2 eclipseShadowPlanet = calcShadow(shadowDataArray, positionWorldCoords.xyz, true);
    float sunIntensityGround = sunRadiance * eclipseShadowPlanet.y;
    atmColor = groundColor(x, tF, v, s, attenuation, color.rgb, normal.xyz, irradianceFactor,
      normal.w, sunIntensityGround);
  }
  else {
    // In order to get better performance, we are not tracing multiple rays per pixel
    // when the ray doesn't intersect the ground
    atmColor = sunColor(v, s, r, mu, irradianceFactor) * (1.0 - eclipseShadowATM.x);
  }

  // Final Color of ATM plus terrain. We want to support opacity so we blend between the
  // planet color and the full atmosphere color using the opacity value
  vec3 c = mix(color.rgb, inscatterColor + atmColor, opacity);
  renderTarget = vec4(c, 1.0);
}
