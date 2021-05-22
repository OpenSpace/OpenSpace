/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

// Atmosphere Rendering Parameters 
uniform float Rg;
uniform float Rt;
uniform float AverageGroundReflectance;
uniform float groundRadianceEmission;
uniform float HR;
uniform vec3 betaRayleigh;
uniform float HO;
uniform vec3 betaOzoneExtinction;
uniform float HM;
uniform vec3 betaMieScattering;
uniform vec3 betaMieExtinction;
uniform float mieG;
uniform float sunRadiance;

uniform bool ozoneLayerEnabled;

uniform ivec2 TRANSMITTANCE;
uniform ivec2 SKY;
uniform ivec2 OTHER_TEXTURES;
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

const float M_PI = 3.141592657;
const float M_2PI = 2.0 * M_PI;

uniform sampler2D transmittanceTexture;

float Rg2 = Rg * Rg;
float Rt2 = Rt * Rt;
float H = sqrt(Rt2 - Rg2);
float H2 = Rt2 - Rg2;
float invSamplesMu = 1.0 / float(SAMPLES_MU);
float invSamplesR = 1.0 / float(SAMPLES_R);
float invSamplesMuS = 1.0 / float(SAMPLES_MU_S);
float invSamplesNu = 1.0 / float(SAMPLES_NU);
float RtMinusRg = float(Rt - Rg);
float invRtMinusRg = 1.0 / RtMinusRg;

float opticalDepth(float localH, float r, float mu, float d) {
  float invH = 1.0 / localH;
  float a = sqrt(0.5 * invH * r);
  vec2 a01 = a * vec2(mu, mu + d / r);
  vec2 a01s = sign(a01);
  vec2 a01sq = a01 * a01;
  float x = a01s.y > a01s.x ? exp(a01sq.x) : 0.0;
  vec2 y = a01s / (2.3193 * abs(a01) + sqrt(1.52 * a01sq + 4.0)) * vec2(1.0, exp(-d * invH * (d / (2.0 * r) + mu)));
  return sqrt(M_2PI * H * r) * exp((Rg-r)*invH) * (x + dot(y, vec2(1.0, -1.0)));
}

vec3 analyticTransmittance(float r, float mu, float d) {
  vec3 ozone = vec3(0.0);
  if (ozoneLayerEnabled) {
    ozone = betaOzoneExtinction * (0.0000006) * opticalDepth(HO, r, mu, d);
  }
  return exp(-betaRayleigh * opticalDepth(HR, r, mu, d) - ozone -
    betaMieExtinction * opticalDepth(HM, r, mu, d));
}

vec3 irradiance(sampler2D sampler, float r, float muSun) {
  float u_r = (r - Rg) * invRtMinusRg;
  float u_muSun = (muSun + 0.2) / 1.2;
  return texture(sampler, vec2(u_muSun, u_r)).rgb;
}


//================================================//
//=============== General Functions ==============//
//================================================//
// In the following shaders r (altitude) is the length of vector/position x in the
// atmosphere (or on the top of it when considering an observer in space), where the light
// is coming from the opposite direction of the view direction, here the vector v or
// viewDirection. Rg is the planet radius and Rt the atmosphere radius.

// Calculate the distance of the ray starting at x (height r) until the planet's ground
// or top of atmosphere
// r := || vec(x) || e [0, Rt]
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r 
float rayDistance(float r, float mu) {
  // The light ray starting at the observer in/on the atmosphere can have to possible end
  // points: the top of the atmosphere or the planet ground. So the shortest path is the
  // one we are looking for, otherwise we may be passing through the ground
  
  // cosine law
  float atmRadiusEps2 = (Rt + ATM_EPSILON) * (Rt + ATM_EPSILON);
  float mu2 = mu * mu;
  float r2 = r * r;
  float rg2 = Rg * Rg;
  float rayDistanceAtmosphere = -r * mu + sqrt(r2 * (mu2 - 1.0) + atmRadiusEps2); 
  float delta = r2 * (mu2 - 1.0) + rg2;

  // Ray may be hitting ground
  if (delta >= 0.0) {
    float rayDistanceGround = -r * mu - sqrt(delta);
    if (rayDistanceGround >= 0.0) {
      return min(rayDistanceAtmosphere, rayDistanceGround);
    }
  }
  return rayDistanceAtmosphere;
}

// Given the windows's fragment coordinates, for a defined view port, gives back the
// interpolated r e [Rg, Rt] and mu, muSun amd nu e [-1, 1]
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// nu := cosone of the angle between vec(s) and vec(v)
// dhdH := it is a vec4. dhdH.x stores the dminT := Rt - r, dhdH.y stores the dH value
//         (see paper), dhdH.z stores dminG := r - Rg and dhdH.w stores dh (see paper)
void unmappingMuMuSunNu(float r, vec4 dhdH, out float mu, out float muSun, out float nu) {
  // Window coordinates of pixel (uncentering also)
  vec2 fragment = gl_FragCoord.xy - vec2(0.5);

  // Pre-calculations
  float r2  = r * r;
  
  float halfSAMPLE_MU = float(SAMPLES_MU) / 2.0;
  // If the (vec(x) dot vec(v))/r is negative, i.e., the light ray has great probability
  // to touch the ground, we obtain mu considering the geometry of the ground
  if (fragment.y < halfSAMPLE_MU) {
    float ud = 1.0 - (fragment.y / (halfSAMPLE_MU - 1.0));
    float d  = min(max(dhdH.z, ud * dhdH.w), dhdH.w * 0.999);
    // cosine law: Rg^2 = r^2 + d^2 - 2rdcos(pi-theta) where cosine(theta) = mu
    mu = (Rg2 - r2 - d * d) / (2.0 * r * d);
    // We can't handle a ray inside the planet, i.e., when r ~ Rg, so we check against it.
    // If that is the case, we approximate to a ray touching the ground.
    // cosine(pi-theta) = dh/r = sqrt(r^2-Rg^2)
    // cosine(theta) = - sqrt(1 - Rg^2/r^2)
    mu = min(mu, -sqrt(1.0 - (Rg2 / r2)) - 0.001);
  }
  // The light ray is touching the atmosphere and not the ground
  else {
    float d = (fragment.y - halfSAMPLE_MU) / (halfSAMPLE_MU - 1.0);
    d = min(max(dhdH.x, d * dhdH.y), dhdH.y * 0.999);
    // cosine law: Rt^2 = r^2 + d^2 - 2rdcos(pi-theta) where cosine(theta) = mu
    mu = (Rt2 - r2 - d * d) / (2.0 * r * d);
  }
  
  float modValueMuSun = mod(fragment.x, float(SAMPLES_MU_S)) / (float(SAMPLES_MU_S) - 1.0);
  // The following mapping is different from the paper. See Colliene for an details.
  muSun = tan((2.0 * modValueMuSun - 1.0 + 0.26) * 1.1f) / tan(1.26 * 1.1);
  nu = -1.0 + floor(fragment.x / float(SAMPLES_MU_S)) / (float(SAMPLES_NU) - 1.0) * 2.0;
}

// Function to access the transmittance texture. Given r and mu, returns the transmittance
// of a ray starting at vec(x), height r, and direction vec(v), mu, and length until it
// hits the ground or the top of atmosphere.
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
vec3 transmittance(float r, float mu) {
  // Given the position x (here the altitude r) and the view angle v
  // (here the cosine(v)= mu), we map this
  float u_r = sqrt((r - Rg) * invRtMinusRg);
  // See Colliene to understand the mapping
  float u_mu = atan((mu + 0.15) / 1.15 * tan(1.5)) / 1.5;
  
  return texture(transmittanceTexture, vec2(u_mu, u_r)).rgb;
}

// Given a position r and direction mu, calculates de transmittance along the ray with
// length d. This function uses the propriety of Transmittance:
// T(a,b) = TableT(a,v)/TableT(b, v)
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
vec3 transmittance(float r, float mu, float d) {
  // Here we use the transmittance property: T(x,v) = T(x,d)*T(d,v) to, given a distance
  // d, calculates that transmittance along that distance starting in x (height r):
  // T(x,d) = T(x,v)/T(d,v).
  // 
  // From cosine law: c^2 = a^2 + b^2 - 2*a*b*cos(ab)
  float ri = sqrt(d * d  + r * r + 2.0 * r * d * mu);
  // mu_i = (vec(d) dot vec(v)) / r_i
  //      = ((vec(x) + vec(d-x)) dot vec(v))/ r_i
  //      = (r*mu + d) / r_i
  float mui = (d + r * mu) / ri;

  // It's important to remember that we calculate the Transmittance table only for zenith
  // angles between 0 and pi/2+episilon. Then, if mu < 0.0, we just need to invert the
  // view direction and the start and end points between them, i.e., if
  // x --> x0, then x0-->x.
  // Also, let's use the property: T(a,c) = T(a,b)*T(b,c)
  // Because T(a,c) and T(b,c) are already in the table T, T(a,b) = T(a,c)/T(b,c).
  if (mu > 0.0) {
    return min(transmittance(r, mu) /  transmittance(ri, mui), 1.0);
  }
  else {
    return min(transmittance(ri, -mui) / transmittance(r, -mu), 1.0);
  }
}

// Calculates Rayleigh phase function given the scattering cosine angle mu
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
float rayleighPhaseFunction(float mu) {
    //return (3.0f / (16.0f * M_PI)) * (1.0f + mu * mu);
    return 0.0596831036 * (1.0 + mu * mu);
}

// Calculates Mie phase function given the scattering cosine angle mu
// mu   := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v)) / r
// mieG := mie phase function value
float miePhaseFunction(float mu, float mieG) {
  float mieG2 = mieG * mieG;
  return 0.1193662072 * (1.0 - mieG2) *
    pow(1.0 + mieG2 - 2.0 * mieG * mu, -1.5) * (1.0 + mu * mu) / (2.0 + mieG2);
}

// Given the height rm view-zenith angle (cosine) mu, sun-zenith angle (cosine) muSun and
// the angle (cosine) between the vec(s) and vec(v), nu, we access the 3D textures and
// interpolate between them (r) to find the value for the 4D texture.
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// nu := cosine of the angle between vec(s) and vec(v)
vec4 texture4D(sampler3D table, float r, float mu, float muSun, float nu) {
  float r2 = r * r;
  float rho = sqrt(r2 - Rg2);
  float rmu = r * mu;
  float delta = rmu * rmu - r2 + Rg2;

  vec4 cst = rmu < 0.0 && delta > 0.0 ?
    vec4(1.0, 0.0, 0.0, 0.5 - 0.5 * invSamplesMu) :
    vec4(-1.0, H2, H, 0.5 + 0.5 * invSamplesMu);

  float u_r = 0.5 * invSamplesR + rho / H * (1.0 - invSamplesR);
  float u_mu = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5 - invSamplesMu);
  float u_mu_s = 0.5 * invSamplesMuS +
    (atan(max(muSun, -0.1975) * tan(1.386)) * 0.9090909090909090 + 0.74) * 0.5 * (1.0 - invSamplesMuS);
  float lerp = (nu + 1.0) / 2.0 * (float(SAMPLES_NU) - 1.0);
  float u_nu = floor(lerp);
  lerp = lerp - u_nu;

  return texture(
    table, vec3((u_nu + u_mu_s) * invSamplesNu, u_mu, u_r)) * (1.0 - lerp) +
    texture(table, vec3((u_nu + u_mu_s + 1.0) * invSamplesNu, u_mu, u_r)) * lerp;
}

// Given the irradiance texture table, the cosine of zenith sun vector and the height of
// the observer (ray's stating point x), calculates the mapping for u_r and u_muSun and
// returns the value in the LUT
// lut   := OpenGL texture2D sampler (the irradiance texture deltaE)
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// r     := height of starting point vect(x)
vec3 irradianceLUT(sampler2D lut, float muSun, float r) {
  // See Bruneton paper and Coliene to understand the mapping
  float u_muSun = (muSun + 0.2) / 1.2;
  float u_r = (r - Rg) * invRtMinusRg;
  return texture(lut, vec2(u_muSun, u_r)).rgb;
}
