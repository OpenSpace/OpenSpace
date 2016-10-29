/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#version 330

#include "atmosphere_common.glsl"

layout(location = 0) out vec4 renderTarget1;
layout(location = 1) out vec4 renderTarget2;

uniform float r;
uniform vec4 dhdH;

uniform sampler2D transmittanceTexture;

// In the following shaders r (altitude) is the length of vector/position x in the
// atmosphere (or on the top of it when considering an observer in space),
// where the light is comming from the opposity direction of the view direction,
// here the vector v or viewDirection.
// Rg is the planet radius

void unmappingMuMuSunNu(const float r, vec4 dhdH, out float mu, out float muSun, out float nu) {
  float x = gl_FragCoord.x - 0.5f;
  float y = gl_FragCoord.y - 0.5f;
  if (y < (float(RES_MU) / 2.0f)) {
    float d = 1.0f - y / (float(RES_MU) / 2.0f - 1.0f);
    d = min(max(dhdH.z, d * dhdH.w), dhdH.w * 0.999);
    mu = (Rg * Rg - r * r - d * d) / (2.0 * r * d);
    mu = min(mu, -sqrt(1.0 - (Rg / r) * (Rg / r)) - 0.001);
  } else {
    float d = (y - float(RES_MU) / 2.0f) / (float(RES_MU) / 2.0f - 1.0f);
    d = min(max(dhdH.x, d * dhdH.y), dhdH.y * 0.999);
    mu = (Rt * Rt - r * r - d * d) / (2.0f * r * d);
  }
  muSun = mod(x, float(RES_MU_S)) / (float(RES_MU_S) - 1.0f);
  muSun = tan((2.0f * muSun - 1.0f + 0.26f) * 1.1f) / tan(1.26f * 1.1f);
  nu = -1.0f + floor(x / float(RES_MU_S)) / (float(RES_NU) - 1.0f) * 2.0f;
}

vec3 transmittanceFromTexture(const float r, const float mu) {
  // Given the position x (here the altitude r) and the view
  // angle v (here the cosine(v)= mu), we map this 
  float u_r  = sqrt((r - Rg) / (Rt - Rg));
  // See Colliene to understand the different mapping.
  float u_mu = atan((mu + 0.15) / (1.0 + 0.15) * tan(1.5)) / 1.5;
  
  return texture(transmittanceTexture, vec2(u_mu, u_r)).rgb;
}

vec3 transmittance(const float r, const float mu, const float d) {
  // Here we use the transmittance property: T(x,v) = T(x,d)*T(d,v)
  // to, given a distance d, calculates that transmittance along
  // that distance starting in x (hight r): T(x,d) = T(x,v)/T(d,v).
  // 
  // From cosine law: c^2 = a^2 + b^2 - 2*a*b*cos(ab)
  float ri = sqrt(d * d  + r * r + 2.0 * r * d * mu);
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
    return min(transmittanceFromTexture(r, mu) / 
               transmittanceFromTexture(ri, mui), 1.0f);
  } else {
    return min(transmittanceFromTexture(ri, -mui) / 
               transmittanceFromTexture(r, -mu), 1.0f);
  }
}

void integrand(const float r, const float mu, const float muSun, const float nu, 
                const float y, out vec3 S_R, out vec3 S_M) {
  // The integral's integrand is the single inscattering radiance:
  // S[L0] = P_M*S_M[L0] + P_R*S_R[L0]
  // where S_M[L0] = T*(betaMScattering * exp(-h/H_M))*L0 and
  // S_R[L0] = T*(betaRScattering * exp(-h/H_R))*L0.
  // T = transmittance.
  // One must remember that because the occlusion on L0, the integrand
  // here will be equal to 0 in that cases.
  // Also it is important to remember that the phase function for the
  // Rayleigh and Mie scattering are addded during the rendering time
  // to increase the angular precision
  S_R = vec3(0.0);
  S_M = vec3(0.0);
  
  // cosine law
  float ri = sqrt(r * r + y * y + 2.0 * r * mu * y);
  // Approximate and interpolate muSun_i
  float muSun_i = (nu * y + muSun * r) / ri;

  // ri >= Rg
  ri = max(Rg, ri);

  // If the muSun_i is smaller than the angle to horizon (no sun radiance
  // hitting the point y), we return S_R = S_M = 0.0f.
  if (muSun_i >= -sqrt(1.0 - Rg * Rg / (ri * ri))) {
    // It's the transmittance from the point y (ri) to the top of atmosphere
    // in direction of the sun (muSun_i) and the transmittance from the observer
    // at x (r) to y (ri).
    vec3 transmittanceY = transmittance(r, mu, y) * transmittanceFromTexture(ri, muSun_i);
    // exp(-h/H)*T(x,v)
    S_R = exp( -(ri - Rg) / HR ) * transmittanceY;
    S_M = exp( -(ri - Rg) / HM ) * transmittanceY;
    // The L0 (sun radiance) is added in real-time.
  }
}

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
  // No imaginary numbers... :-)
  if (delta >= 0.0f) {
    float rayDistanceGround = -r * mu - sqrt(delta);
    if (rayDistanceGround >= 0.0f) {
      return min(rayDistanceAtmosphere, rayDistanceGround);
    }
  }
  return rayDistanceAtmosphere;
}

void inscatter(const float r, const float mu, const float muSun, const float nu,
               out vec3 S_R, out vec3 S_M) {
  // Let's calculate S_M and S_R by integration along the eye ray path inside
  // the atmosphere, given a position r, a view angle (cosine) mu, a sun
  // position angle (cosine) muSun, and the angle (cosine) between the sun position
  // and the view direction, nu.
  // Integrating using the Trapezoidal rule:
  // Integral(f(y)dy)(from a to b) = (b-a)/2n_steps*(Sum(f(y_i+1)+f(y_i)))
  S_R = vec3(0.0f);
  S_M = vec3(0.0f);
  float rayDist = rayDistance(r, mu);
  float dy =  rayDist / float(INSCATTER_INTEGRAL_SAMPLES);
  float yi = 0.0f;
  vec3 S_Ri;
  vec3 S_Mi;
  integrand(r, mu, muSun, nu, 0.0, S_Ri, S_Mi);
  for (int i = 1; i <= INSCATTER_INTEGRAL_SAMPLES; ++i) {
    float yj = float(i) * dy;
    vec3 S_Rj;
    vec3 S_Mj;
    integrand(r, mu, muSun, nu, yj, S_Rj, S_Mj);
    S_R += (S_Ri + S_Rj);
    S_M += (S_Mi + S_Mj);
    yi = yj;
    S_Ri = S_Rj;
    S_Mi = S_Mj;
  }
  S_R *= betaRayleigh * (rayDist / (2.0f * float(INSCATTER_INTEGRAL_SAMPLES)));
  S_M *= betaMieScattering * (rayDist / (2.0f * float(INSCATTER_INTEGRAL_SAMPLES)));
}

void main(void) {
  vec3 S_R;
  vec3 S_M;
  float mu, muSun, nu;
  unmappingMuMuSunNu(r, dhdH, mu, muSun, nu);
  // Here we calculate the single inScattered light.
  // Because this is a single inscattering, the light
  // that arrives at a point y in the path from the
  // eye to the infinity (top of atmosphere or planet's
  // ground), comes only from the light source, i.e., the
  // sun. So, the there is no need to integrate over the
  // whole solid angle (4pi), we need only to consider
  // the Sun position (cosine of sun pos = muSun).
  // Then, following the paper notation:
  // S[L] = P_R*S_R[L0] + P_M*S_M[L0] + S[L*]
  // For single inscattering only:
  // S[L0] = P_R*S_R[L0] + P_M*S_M[L0]
  // In order to save memory, we just store the red component
  // of S_M[L0], and later we use the proportionality rule
  // to calcule the other components.
  inscatter(r, mu, muSun, nu, S_R, S_M);
  renderTarget1 = vec4(S_R, 1.0);
  renderTarget2 = vec4(S_M, 1.0);
}
