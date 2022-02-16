/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
 * Modified parts of the code (4D texture mechanism, analytical transmittance etc)       *
 * from Eric Bruneton is used in the following code.                                     * 
 ****************************************************************************************/

 /**
 * Precomputed Atmospheric Scattering
 * Copyright (c) 2008 INRIA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */


// Atmosphere Rendering Parameters 
uniform float Rg;
uniform float Rt;
uniform float AverageGroundReflectance;
uniform float groundRadianceEmittion;
uniform float HR;
uniform vec3 betaRayleigh;
uniform float HO2;
uniform vec3 sigmaOzoneAbsCrossSecion;
uniform float HM;
uniform vec3 betaMieScattering;
uniform vec3 betaMieExtinction;
uniform float mieG;
uniform float sunRadiance;

uniform bool ozoneLayerEnabled;
uniform bool oxygenAbsLayerEnabled;

uniform int TRANSMITTANCE_W;
uniform int TRANSMITTANCE_H;
uniform int SKY_W;
uniform int SKY_H;
uniform int OTHER_TEXTURES_W;
uniform int OTHER_TEXTURES_H;
uniform int SAMPLES_R;
uniform int SAMPLES_MU;
uniform int SAMPLES_MU_S;
uniform int SAMPLES_NU;

const float ATM_EPSILON = 1.0;

// Integration steps
const int TRANSMITTANCE_STEPS = 500;
const int INSCATTER_INTEGRAL_SAMPLES = 50;
const int IRRADIANCE_INTEGRAL_SAMPLES = 32;
const int INSCATTER_SPHERICAL_INTEGRAL_SAMPLES = 16;

const float M_PI = 3.14159265359f;
const float INV_M_PI = 0.318309886184f;
const float TO_RADIANS = M_PI / 180.f;

uniform sampler2D transmittanceTexture;

float Rg2    = Rg * Rg;
float Rt2    = Rt * Rt;
float H      = sqrt(Rt2 - Rg2);
float H2     = Rt2 - Rg2;
float invSamplesMu = 1.0f / float(SAMPLES_MU);
float invSamplesR = 1.0f / float(SAMPLES_R);
float invSamplesMuS = 1.0f / float(SAMPLES_MU_S);
float invSamplesNu = 1.0f / float(SAMPLES_NU);
float RtMinusRg = float(Rt - Rg);
float invRtMinusRg = 1.0f / RtMinusRg;

uniform bool  advancedModeEnabled;
uniform bool  useOnlyAdvancedMie;
uniform bool  useCornettePhaseFunction;
uniform bool  usePenndorfPhaseFunction;
uniform float deltaPolarizability;
uniform vec3  n_real_rayleigh;
uniform vec3  n_complex_rayleigh;
uniform vec3  n_real_mie;
uniform vec3  n_complex_mie;
uniform vec3  lambdaArray;
uniform vec3  g1;
uniform vec3  g2;
uniform float alpha;
uniform float N_rayleigh; // in m^3
uniform float N_mie;
uniform float N_rayleigh_abs_molecule;
uniform float radius_abs_molecule_rayleigh;
uniform float mean_radius_particle_mie;
uniform float turbidity;
uniform float jungeExponent;
uniform vec3  Kappa;

//================================================//
//=============== General Functions ==============//
//================================================//
// In the following shaders r (altitude) is the length of vector/position x in the
// atmosphere (or on the top of it when considering an observer in space),
// where the light is comming from the opposite direction of the view direction,
// here the vector v or viewDirection.
// Rg is the planet radius and Rt the atmosphere radius.

//--- Calculate the distance of the ray starting at x (height r)
// until the planet's ground or top of atmosphere. ---
// r := || vec(x) || e [0, Rt]
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r 
float rayDistance(const float r, const float mu) {
  // The light ray starting at the observer in/on the atmosphere can
  // have to possible end points: the top of the atmosphere or the
  // planet ground. So the shortest path is the one we are looking for,
  // otherwise we may be passing through the ground.
  
  // cosine law
  float atmRadiusEps = Rt + ATM_EPSILON;
  float rayDistanceAtmosphere = -r * mu +
    sqrt(r * r * (mu * mu - 1.0f) + atmRadiusEps * atmRadiusEps); 
  float delta = r * r * (mu * mu - 1.0f) + Rg * Rg;

  // Ray may be hitting ground
  if (delta >= 0.0f) {
    float rayDistanceGround = -r * mu - sqrt(delta);
    if (rayDistanceGround >= 0.0f) {
      return min(rayDistanceAtmosphere, rayDistanceGround);
    }
  }
  return rayDistanceAtmosphere;
}


//-- Given the texture coordinate (in [0, 1]), calculates the
// height (r in km) from the Bruneton mapping equations.
// u_r := texture coordinate e [0, 1]
float unmappingR(const float u) {
  float rho = H * u;
  float rho2 = rho * rho;
  
  return sqrt(Rg2 + rho2);
}

//-- Given the heigh from the center of the planet (in km), calculates the
// texture coordinate (in [0, 1]) from the Bruneton mapping equations.
// r := observer's heigh position (top of atm if in space) in km
float mappingUfromR(const float r) {
  return sqrt((r*r - Rg2) / H2);
}

//-- Given the texture coordinate (in [0, 1]), calculates the
// height (r in km) from the Bruneton mapping equations.
// u_r  := texture coordinate in [0, 1] for LUT of r = ||x||
// u_mu := texture coordinate in [0, 1] for LUT of mu = (x dot v) / ||x||
float unmappingMu(const float r, const float u_r, const float u_mu) {
  // Paper Version
  // float rho   = H * u_r;
  // float rho2  = rho * rho;
  // float d_min = Rt - r;
  // float d_max = rho + H;
  // float d     = d_min + u_mu * (d_max - d_min);

  // float mu = 0.f;

  // if (d == 0.f) {
  //   mu = 1.f;  
  // } else {
  //   mu = clamp((H * H - rho2 - d * d) / (2.0 * r * d), -1.f, 1.f);
  // }

  // return mu;

  // Earth's optimized
  return -0.15f + tan(1.5f * u_mu) / tan(1.5f) * (1.0f + 0.15f);
}

//-- Given cosine between the view direction and the zenith angle (mu), 
// calculates the texture coordinate (in [0, 1]) from the Bruneton 
// mapping equations.
// r  := height of starting point vec(x)
// mu := cosine of theta [-delta, 1]
float mappingUfromMu(const float r, const float mu) {
  // Paper Verion
  // float rho   = sqrt(r * r - Rg2);
  // float delta = r * r * (mu * mu - 1.f) + Rt2;
  // float d     = max(-r * mu + sqrt(delta), 0.f); 
  // float d_min = Rt - r;
  // float d_max = rho + H;
  // float u_mu  = (d - d_min) / (d_max - d_min);
  
  // Earth's optimized
  float u_mu = atan((mu + 0.15f) / (1.0f + 0.15f) * tan(1.5f)) / 1.5f;

  return u_mu;
}

//-- Given the window's fragment coordinates, for a defined
// viewport, gives back the interpolated r e [Rg, Rt] and mu e [-1, 1] --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
void unmappingRAndMu(out float r, out float mu) {
  float u_mu  = gl_FragCoord.x / float(TRANSMITTANCE_W);
  float u_r   = gl_FragCoord.y / float(TRANSMITTANCE_H);
  
  r  = unmappingR(u_r);
  mu = unmappingMu(r, u_r, u_mu);

  // old
  //mu = -0.15f + tan(1.5f * u_mu) / tan(1.5f) * (1.0f + 0.15f);
}

//-- Given the windows's fragment coordinates, for a defined view port,
// gives back the interpolated r e [Rg, Rt] and muSun e [-1, 1] --
// r := height of starting point vec(x)
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
void unmappingRAndMuSun(out float r, out float muSun) {
  // See Bruneton and Colliene to understand the mapping.
  muSun = -0.2f + (gl_FragCoord.x - 0.5f) / (float(OTHER_TEXTURES_W) - 1.0f) * (1.0f + 0.2f);
  //r  = Rg + (gl_FragCoord.y - 0.5f) / (float(OTHER_TEXTURES_H) - 1.0f) * (Rt - Rg);
  float u_r = (gl_FragCoord.y - 0.5f) / (float(OTHER_TEXTURES_H) - 1.0f);
  r = unmappingR(u_r);
}

//-- Given the windows's fragment coordinates, for a defined view port,
// gives back the interpolated r e [Rg, Rt] and muSun e [-1, 1] for the
// Irradiance deltaE texture table --
// r := height of starting point vect(x)
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
void unmappingRAndMuSunIrradiance(out float r, out float muSun) {
  // See Bruneton and Colliene to understand the mapping.
  muSun = -0.2f + (gl_FragCoord.x - 0.5f) / (float(SKY_W) - 1.0f) * (1.0f + 0.2f);
  float u_r = (gl_FragCoord.y - 0.5f) / (float(SKY_H) - 1.0f);
  r = unmappingR(u_r);
}

//-- Given the windows's fragment coordinates, for a defined view port,
// gives back the interpolated r e [Rg, Rt] and mu, muSun amd nu e [-1, 1] --
// r     := height of starting point vect(x)
// mu    := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// nu    := cosone of the angle between vec(s) and vec(v)
// dhdH  := it is a vec4. dhdH.x stores the dminT := Rt - r, dhdH.y stores the dH value (see paper),
// dhdH.z stores dminG := r - Rg and dhdH.w stores dh (see paper).
void unmappingMuMuSunNu(const float r, vec4 dhdH, out float mu, out float muSun, out float nu) {
  /* =========================
     ------ New Version ------
     =========================
  */
  /*
  // Window coordinates of pixel (uncentering also)
  float fragmentX = gl_FragCoord.x - 0.5f;
  float fragmentY = gl_FragCoord.y - 0.5f;
  float r2  = r * r;
  
  float halfSAMPLE_MU = float(SAMPLES_MU) / 2.0f;

  // If the (vec(x) dot vec(v))/r is negative, i.e.,
  // the light ray has great probability to touch
  // the ground, we obtain mu considering the geometry
  // of the ground
  if (fragmentY < halfSAMPLE_MU) {
    float rho   = dhdH.w;
    float d_min = dhdH.z;
    float d_max = rho;
    //ud = frag_coord.z / SCATTERING_TEXTURE_R_SIZE
    float ud = 1.0f - (fragmentY / (halfSAMPLE_MU - 1.0f))
    float varInterpolate =  1.0 - 2.0 * ud;
    float MAX_TEXTURE_SIZE = halfSAMPLE_MU
    float d =  d_min + (d_max - d_min) * ((varInterpolate - 0.5 / MAX_TEXTURE_SIZE) / (1.0 - 1.0 / MAX_TEXTURE_SIZE));

    mu = d == 0.f ? -1.f : clamp(-(rho * rho + d * d) / (2.0 * r * d), -1.f, 1.f);
  }
  // The light ray is touching the atmosphere and
  // not the ground
  else {
    float rho   = dhdH.w;
    float d_min = dhdH.x;
    float d_max = rho + H;
    //ud = frag_coord.z / SCATTERING_TEXTURE_R_SIZE
    float ud = 1.0f - (fragmentY / (halfSAMPLE_MU - 1.0f))
    float varInterpolate =  2.0 * ud - 1.0;
    float d = d_min + (d_max - d_min) * ((varInterpolate - 0.5 / MAX_TEXTURE_SIZE) / (1.0 - 1.0 / MAX_TEXTURE_SIZE));
    mu = d == 0.f ? 1.f : clamp((H * H - rho * rho - d * d) / (2.0 * r * d, -1.f, 1.f));
  }
  
  float modValueMuSun = mod(fragmentX, float(SAMPLES_MU_S)) / (float(SAMPLES_MU_S) - 1.0f);
  float uMuSun = (modValueMuSun - 0.5 / float(SAMPLES_MU_S)) / (1.0 - 1.0 / float(SAMPLES_MU_S));
  float d_min = Rt2;
  float d_max = H;
  
  float minMuSun = cos(102.0 * TO_RADIANS);
  float A = -2.0 * minMuSun * Rg / (d_max - d_min);
  float a = (A - uMuSun * A) / (1.0 + uMuSun * A);
  Length d = d_min + min(a, A) * (d_max - d_min);
  muSun = d == 0.f ? 1.f : clamp((H2 - d * d) / (2.0 * Rg * d));

  nu = -1.0f + floor(fragmentX / float(SAMPLES_MU_S)) / (float(SAMPLES_NU) - 1.0f) * 2.0f;
  */

  /* =========================
     ------ Old Version ------
     =========================
  */
  // Window coordinates of pixel (uncentering also)
  float fragmentX = gl_FragCoord.x - 0.5f;
  float fragmentY = gl_FragCoord.y - 0.5f;

  // Pre-calculations
  //float Rg2 = Rg * Rg;
  //float Rt2 = Rt * Rt;
  float r2  = r * r;
  
  float halfSAMPLE_MU = float(SAMPLES_MU) / 2.0f;
  // If the (vec(x) dot vec(v))/r is negative, i.e.,
  // the light ray has great probability to touch
  // the ground, we obtain mu considering the geometry
  // of the ground
  if (fragmentY < halfSAMPLE_MU) {
    float ud = 1.0f - (fragmentY / (halfSAMPLE_MU - 1.0f));
    float d  = min(max(dhdH.z, ud * dhdH.w), dhdH.w * 0.999);
    // cosine law: Rg^2 = r^2 + d^2 - 2rdcos(pi-theta)
    // where cosine(theta) = mu
    mu = (Rg2 - r2 - d * d) / (2.0 * r * d);
    // We can't handle a ray inside the planet, i.e.,
    // when r ~ Rg, so we check against it.
    // If that is the case, we approximate to
    // a ray touching the ground.
    // cosine(pi-theta) = dh/r = sqrt(r^2-Rg^2)
    // cosine(theta) = - sqrt(1 - Rg^2/r^2)
    mu = min(mu, -sqrt(1.0 - (Rg2 / r2)) - 0.001);
  }
  // The light ray is touching the atmosphere and
  // not the ground
  else {
    float d = (fragmentY - halfSAMPLE_MU) / (halfSAMPLE_MU - 1.0f);
    d = min(max(dhdH.x, d * dhdH.y), dhdH.y * 0.999);
    // cosine law: Rt^2 = r^2 + d^2 - 2rdcos(pi-theta)
    // whre cosine(theta) = mu
    mu = (Rt2 - r2 - d * d) / (2.0f * r * d);
  }
  
  float modValueMuSun = mod(fragmentX, float(SAMPLES_MU_S)) / (float(SAMPLES_MU_S) - 1.0f);
  // The following mapping is different from the paper. See Colliene for an details.
  muSun = tan((2.0f * modValueMuSun - 1.0f + 0.26f) * 1.1f) / tan(1.26f * 1.1f);
  nu = -1.0f + floor(fragmentX / float(SAMPLES_MU_S)) / (float(SAMPLES_NU) - 1.0f) * 2.0f;


}

// -- Given the height rm view-zenith angle (cosine) mu,
// sun-zenith angle (cosine) muSun and the angle (cosine)
// between the vec(s) and vec(v), nu, we access the 3D textures
// and interpolate between them (r) to find the value for the
// 4D texture. --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// nu := cosine of the angle between vec(s) and vec(v)
vec4 texture4D(sampler3D table, const float r, const float mu, 
                const float muSun, const float nu)
{ 
  /* =========================
     ------ New Version ------
     =========================
  */
  /*
=== PAREI AQUI!! ===
  //float Rg2    = Rg * Rg;
  //float Rt2    = Rt * Rt;
  float r2     = r * r;
  //float H      = sqrt(Rt2 - Rg2);
  float rho    = sqrt(r2 - Rg2);
  float rmu    = r * mu;
  float delta  = rmu * rmu - r2 + Rg2;
  //float invSamplesMu = 1.0f / float(SAMPLES_MU);
  //float invSamplesR = 1.0f / float(SAMPLES_R);
  //float invSamplesMuS = 1.0f / float(SAMPLES_MU_S);
  //float invSamplesNu = 1.0f / float(SAMPLES_NU);
  // vec4 cst     = rmu < 0.0f && delta > 0.0f ?
  //   vec4(1.0f, 0.0f, 0.0f, 0.5f - 0.5f / float(SAMPLES_MU)) :
  //   vec4(-1.0f, H * H, H, 0.5f + 0.5f / float(SAMPLES_MU));

  vec4 cst     = rmu < 0.0f && delta > 0.0f ?
    vec4(1.0f, 0.0f, 0.0f, 0.5f - 0.5f * invSamplesMu) :
    vec4(-1.0f, H2, H, 0.5f + 0.5f * invSamplesMu);

  //float u_r    = 0.5f / float(SAMPLES_R) + rho / H * (1.0f - 1.0f / float(SAMPLES_R));
  float u_r    = 0.5f * invSamplesR + (rho / H) * (1.0f - invSamplesR);
  // @Todo: JCC: Change to new version of mapping
  //float u_mu   = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5f - 1.0f / float(SAMPLES_MU));
  float u_mu   = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5f - invSamplesMu);
  // float u_mu_s = 0.5f / float(SAMPLES_MU_S) +
  //   (atan(max(muSun, -0.1975) * tan(1.26f * 1.1f)) / 1.1f + (1.0f - 0.26f)) * 0.5f * (1.0f - 1.0f / float(SAMPLES_MU_S));
  float u_mu_s = 0.5f * invSamplesMuS +
    (atan(max(muSun, -0.1975) * tan(1.386f)) * 0.9090909090909090 + (0.74f)) * 0.5f * (1.0f - invSamplesMuS);
  float lerp = (nu + 1.0f) / 2.0f * (float(SAMPLES_NU) - 1.0f);
  float u_nu = floor(lerp);
  lerp = lerp - u_nu;

  // return texture(table, vec3((u_nu + u_mu_s) / float(SAMPLES_NU), u_mu, u_r)) * (1.0f - lerp) +
  //   texture(table, vec3((u_nu + u_mu_s + 1.0f) / float(SAMPLES_NU), u_mu, u_r)) * lerp;
  
Number GetTextureCoordFromUnitRange(Number x, int texture_size) {
  return 0.5 / Number(texture_size) + x * (1.0 - 1.0 / Number(texture_size));
}


  float d = DistanceToTopAtmosphereBoundary( atmosphere, atmosphere.bottom_radius, muSun);
  float d_min = Rt2;
  float d_max = H;
  float a = (d - d_min) / (d_max - d_min);
  float minMuSun = cos(102.0 * TO_RADIANS);
  float A = -2.0 * minMuSun * Rg / (d_max - d_min);
  float uMuSunInterpolation = max(1.0 - a / A, 0.0) / (1.0 + a);
  float u_mu_s = 0.5 / float(SAMPLES_MU_S) + uMuSunInterpolation * (1.0 - 1.0 / float(SAMPLES_MU_S));

  float u_nu = (nu + 1.0) / 2.0;


  return texture(table, vec3((u_nu + u_mu_s) * invSamplesNu, u_mu, u_r)) * (1.0f - lerp) +
    texture(table, vec3((u_nu + u_mu_s + 1.0f) * invSamplesNu, u_mu, u_r)) * lerp;
  */

  /* =========================
     ------ Old Version ------
     =========================
  */
  //float Rg2    = Rg * Rg;
  //float Rt2    = Rt * Rt;
  float r2     = r * r;
  //float H      = sqrt(Rt2 - Rg2);
  float rho    = sqrt(r2 - Rg2);
  float rmu    = r * mu;
  float delta  = rmu * rmu - r2 + Rg2;
  //float invSamplesMu = 1.0f / float(SAMPLES_MU);
  //float invSamplesR = 1.0f / float(SAMPLES_R);
  //float invSamplesMuS = 1.0f / float(SAMPLES_MU_S);
  //float invSamplesNu = 1.0f / float(SAMPLES_NU);
  // vec4 cst     = rmu < 0.0f && delta > 0.0f ?
  //   vec4(1.0f, 0.0f, 0.0f, 0.5f - 0.5f / float(SAMPLES_MU)) :
  //   vec4(-1.0f, H * H, H, 0.5f + 0.5f / float(SAMPLES_MU));

  vec4 cst     = rmu < 0.0f && delta > 0.0f ?
    vec4(1.0f, 0.0f, 0.0f, 0.5f - 0.5f * invSamplesMu) :
    vec4(-1.0f, H2, H, 0.5f + 0.5f * invSamplesMu);

  //float u_r    = 0.5f / float(SAMPLES_R) + rho / H * (1.0f - 1.0f / float(SAMPLES_R));
  float u_r    = 0.5f * invSamplesR + rho / H * (1.0f - invSamplesR);
  //float u_mu   = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5f - 1.0f / float(SAMPLES_MU));
  float u_mu   = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5f - invSamplesMu);
  // float u_mu_s = 0.5f / float(SAMPLES_MU_S) +
  //   (atan(max(muSun, -0.1975) * tan(1.26f * 1.1f)) / 1.1f + (1.0f - 0.26f)) * 0.5f * (1.0f - 1.0f / float(SAMPLES_MU_S));
  float u_mu_s = 0.5f * invSamplesMuS +
    (atan(max(muSun, -0.1975) * tan(1.386f)) * 0.9090909090909090 + (0.74f)) * 0.5f * (1.0f - invSamplesMuS);
  float lerp = (nu + 1.0f) / 2.0f * (float(SAMPLES_NU) - 1.0f);
  float u_nu = floor(lerp);
  lerp = lerp - u_nu;

  // return texture(table, vec3((u_nu + u_mu_s) / float(SAMPLES_NU), u_mu, u_r)) * (1.0f - lerp) +
  //   texture(table, vec3((u_nu + u_mu_s + 1.0f) / float(SAMPLES_NU), u_mu, u_r)) * lerp;
  
  return texture(table, vec3((u_nu + u_mu_s) * invSamplesNu, u_mu, u_r)) * (1.0f - lerp) +
    texture(table, vec3((u_nu + u_mu_s + 1.0f) * invSamplesNu, u_mu, u_r)) * lerp;
}

//-- Function to access the transmittance texture. Given r
// and mu, returns the transmittance of a ray starting at vec(x),
// height r, and direction vec(v), mu, and length until it hits
// the ground or the top of atmosphere. --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
vec3 transmittanceLUT(const float r, const float mu) {
  // Given the position x (here the altitude r) and the view
  // angle v (here the cosine(v)= mu), we map this
  float u_r  = mappingUfromR(r);
  // Old
  //float u_r  = sqrt((r*r - Rg*Rg) / (Rt*Rt - Rg*Rg));
  
  // Old
  // See Colliene to understand the different mapping.
  //float u_mu = atan((mu + 0.15f) / (1.0f + 0.15f) * tan(1.5f)) / 1.5f;
  
  float u_mu = mappingUfromMu(r, mu);

  return texture(transmittanceTexture, vec2(u_mu, u_r)).rgb;
}

// -- Given a position r and direction mu, calculates de transmittance
// along the ray with length d. This function uses the propriety
// of Transmittance: T(a,b) = TableT(a,v)/TableT(b, v) --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
vec3 transmittance(const float r, const float mu, const float d) {
  // Here we use the transmittance property: T(x,v) = T(x,d)*T(d,v)
  // to, given a distance d, calculates that transmittance along
  // that distance starting in x (hight r): T(x,d) = T(x,v)/T(d,v).
  // 
  // From cosine law: c^2 = a^2 + b^2 - 2*a*b*cos(ab)
  float ri = sqrt(d * d  + r * r + 2.0 * r * d * mu);
  // mu_i = (vec(d) dot vec(v)) / r_i
  //      = ((vec(x) + vec(d-x)) dot vec(v))/ r_i
  //      = (r*mu + d) / r_i
  float mui = (d + r * mu) / ri;

  // It's important to remember that we calculate the Transmittance
  // table only for zenith angles between 0 and pi/2+episilon.
  // Then, if mu < 0.0, we just need to invert the view direction
  // and the start and end points between them, i.e., if
  // x --> x0, then x0-->x.
  // Also, let's use the property: T(a,c) = T(a,b)*T(b,c)
  // Because T(a,c) and T(b,c) are already in the table T,
  // T(a,b) = T(a,c)/T(b,c).
  if (mu > 0.0f) {
    return min(transmittanceLUT(r, mu) / 
               transmittanceLUT(ri, mui), 1.0f);
  }
  else {
    return min(transmittanceLUT(ri, -mui) / 
               transmittanceLUT(r, -mu), 1.0f);
  }
}

// -- Calculates Rayleigh phase function given the
// scattering cosine angle mu --
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
float rayleighPhaseFunction(const float mu) {
  if ((advancedModeEnabled && !useOnlyAdvancedMie) || usePenndorfPhaseFunction) {
    // Penndorf
    return 0.7629f * (1.f + 0.932f * mu * mu) * INV_M_PI * 0.25f;
  } else {
    //return (3.0f / (16.0f * M_PI)) * (1.0f + mu * mu);
    return 0.0596831036 * (1.0f + mu * mu);
  }
}

// -- Calculates DHG Mie phase function given thescattering cosine angle mu --
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// g1 := forward scattering 
// g2 := backward scattering
// alpha: = ratio between g1 and g2
vec3 DHG_MiePhaseFunction(const float mu) {
  vec3 g1SQRD = g1 * g1;
  vec3 g2SQRD = g2 * g2;
  float exponent = 3.f/2f;
  
  vec3 denom1 = vec3(1.f) + g1SQRD - 2.f * g1 * mu;
  vec3 denom2 = vec3(1.f) + g2SQRD - 2.f * g2 * mu;
  vec3 d1Powered = vec3(pow(denom1.r, exponent), pow(denom1.g, exponent), pow(denom1.b, exponent));
  vec3 d2Powered = vec3(pow(denom2.r, exponent), pow(denom2.g, exponent), pow(denom2.b, exponent));
  
  return (alpha * ((vec3(1.f) - g1SQRD)/d1Powered) + (vec3(1.f) - alpha) * ((vec3(1.f) - g2SQRD)/d2Powered)) 
    * INV_M_PI * 0.25f;

  // vec3 mieG2 = g1 * g1;
  // float powerRed   = pow(1.0f + mieG2.r - 2.0f * g1.r * mu, -1.5f);
  // float powerGreen = pow(1.0f + mieG2.g - 2.0f * g1.g * mu, -1.5f);
  // float powerBlue  = pow(1.0f + mieG2.b - 2.0f * g1.b * mu, -1.5f);

  // vec3 cornette =  0.1193662072 * (vec3(1.0f) - mieG2) * 
  //   vec3(powerRed, powerGreen, powerBlue) * (1.0f + mu * mu) / (vec3(2.0f) + mieG2);

  // return vec3(cornette);
}

// -- Calculates Mie phase function given the
// scattering cosine angle mu --
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
vec3 miePhaseFunction(const float mu) {
    if (advancedModeEnabled && !useCornettePhaseFunction) {
      return DHG_MiePhaseFunction(mu);
    } else {
      // return (3.0f / (8.0f * M_PI)) * 
      //      ( ( (1.0f - (mieG * mieG) ) * (1.0f + mu * mu) ) / 
      //      ( (2.0f + mieG * mieG) *
      //        pow(1.0f + mieG * mieG - 2.0f * mieG * mu, 3.0f/2.0f) ) );
      // return 1.5f * 1.0f / (4.0f * M_PI) * (1.0f - mieG * mieG) *
      //   pow(1.0f + (mieG * mieG) - 2.0f * mieG * mu, -3.0f/2.0f) * (1.0f + mu * mu) / (2.0f + mieG*mieG);

      float mieG2 = mieG * mieG;
      float cornette =  0.1193662072 * (1.0f - mieG2) *
        pow(1.0f + mieG2 - 2.0f * mieG * mu, -1.5f) * (1.0f + mu * mu) / (2.0f + mieG2);

      return vec3(cornette);
    }
}

// -- Calculates Rayleigh Scattering Coefficients given the real part of the refractive index
//    and the wavelength of the incident light.
// lambda := wavelength of the incident light (680, 550, 440) nm in our case
// m := Real(n(lambada))
vec3 scatteringCoefficientRayleigh(const vec3 lambda) {
  if (advancedModeEnabled && !useOnlyAdvancedMie) {
    vec3 lambda4 = lambda * lambda * lambda * lambda;
    vec3 m2 = n_real_rayleigh * n_real_rayleigh;
    vec3 alpha = (m2 - vec3(1.f));
    float fDelta = (6.f + 3.f * deltaPolarizability) / (6.f - 7.f * deltaPolarizability);
    float mTokm = 1000.f;
    return mTokm * ((8.f * M_PI * M_PI * M_PI * alpha * alpha) / (3.f * N_rayleigh * lambda4)) * fDelta;
  } else {
    return betaRayleigh;
  }
}

// -- Calculates Rayleigh absorption Coefficients for a given type of molecule,
//    given the refractive index and the wavelength of the incident light.
// lambda := wavelength of the incident light (680, 550, 440) nm in our case
vec3 absorptionCoefficientRayleight(const vec3 lambda) {
  // n = m + ki => n^2 = (m^2 - k^2) + (2mk)i
  // n^2 - 1 = (m^2 - k^2 - 1) + (2mk)i = a
  // n^2 + 2 = (m^2 - k^2 + 2) + (2mk)i = b
  // a/b = [a x b*]/[b x b*], where b* is the conjugate of b
  vec3 n2_real    = (n_real_rayleigh * n_real_rayleigh) - (n_complex_rayleigh * n_complex_rayleigh);
  vec3 n2_complex = (2.f * n_real_rayleigh * n_complex_rayleigh);
  
  vec3 a_real    = n2_real - vec3(1.f);
  vec3 b_real    = n2_real + vec3(2.f);
  vec3 a_complex = n2_complex;
  vec3 b_complex = n2_complex;
  vec3 b_complex_conjugate = -b_complex;
  
  //vec3 ab_conjugate_real    = (a_real * b_real) - (a_complex * b_complex_conjugate);
  vec3 ab_conjugate_complex = (a_real * b_complex_conjugate + a_complex * b_real);
  vec3 bb_conjugate = (b_real * b_real) + (b_complex * b_complex);
  vec3 imgPart = ab_conjugate_complex / bb_conjugate;

  float r3 = radius_abs_molecule_rayleigh * radius_abs_molecule_rayleigh * radius_abs_molecule_rayleigh;
  
  return vec3(1.f) * ((8.f * M_PI * M_PI * N_rayleigh_abs_molecule * r3) / lambda) * imgPart;
}

vec3 extinctionCoefficientRayleigh(const vec3 lambda) {
  if (advancedModeEnabled && !useOnlyAdvancedMie) {
    return scatteringCoefficientRayleigh(lambda) + absorptionCoefficientRayleight(lambda);
  } else {
    return scatteringCoefficientRayleigh(lambda);
  }  
}

// -- Calculates Mie Scattering Coefficients given the wavelength of the incident light --.
// lambda := wavelength of the incident light (680, 550, 440) nm in our case
vec3 scatteringCoefficientMie(const vec3 lambda) {
  if (advancedModeEnabled) {
    vec3 tmp = (2.f * M_PI / lambda);
    float exponent = jungeExponent - 2.f;
    vec3 lambdaJunge = vec3(
      pow(tmp.x, exponent), 
      pow(tmp.y, exponent), 
      pow(tmp.z, exponent)
      );
    return 0.434f * (0.65f * turbidity - 0.65f) * 1E-16 * M_PI * Kappa * lambdaJunge;
  } else {
    return betaMieScattering;
  }
}

vec3 mieExtinctionEfficiency(const vec3 lambda) {
  if (advancedModeEnabled) {
    vec3 rho = 4.f * M_PI * mean_radius_particle_mie * (n_real_mie - vec3(1.f)) / lambda;
    vec3 tanBeta = n_complex_mie / (n_real_mie - vec3(1.f));
    vec3 beta = atan(tanBeta);
    vec3 expRhoTanBeta = exp(-rho * tanBeta);
    vec3 cosBetaOverRho = cos(beta) / rho;
    vec3 cosBetaOverRho2 = cosBetaOverRho * cosBetaOverRho;
    return 2.f - 4.f * expRhoTanBeta * cosBetaOverRho * sin(rho - beta)
          - 4.f * expRhoTanBeta * cosBetaOverRho2 * cos(rho - 2.f * beta)
          + 4.f * cosBetaOverRho2 * cos(2.f * beta);
  } else {
    return betaMieExtinction;
  }
}

// -- Calculates the Mie Absorption Coefficients for dust (of particles with same radius size and type) --
vec3 extinctionCoefficientMie(const vec3 lambda) {
  if (advancedModeEnabled) {
    return mieExtinctionEfficiency(lambda) * N_mie * M_PI * (mean_radius_particle_mie * mean_radius_particle_mie);
  } else {
    return betaMieExtinction;
  }
}

// -- Given the irradiance texture table, the cosine of zenith sun vector
// and the height of the observer (ray's stating point x), calculates the
// mapping for u_r and u_muSun and returns the value in the LUT. --
// lut   := OpenGL texture2D sampler (the irradiance texture deltaE)
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// r     := height of starting point vect(x)
vec3 irradianceLUT(sampler2D lut, const float muSun, const float r) {
  // See Bruneton paper and Coliene to understand the mapping
  float u_muSun = (muSun + 0.2f) / (1.0f + 0.2f);
  float u_r     = mappingUfromR(r);
  
  return texture(lut, vec2(u_muSun, u_r)).rgb;
}

vec3 irradiance(sampler2D sampler, const float r, const float muSun) {
  float u_r     =  mappingUfromR(r);
  float u_muSun = (muSun + 0.2) / (1.0 + 0.2);
  return texture(sampler, vec2(u_muSun, u_r)).rgb;
}
