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

#version __CONTEXT__

#include "atmosphere_common.glsl"

layout(location = 0) out vec4 renderTarget1;
layout(location = 1) out vec4 renderTarget2;

uniform float Rg;
uniform float Rt;
uniform float HR;
uniform vec3 betaRayleigh;
uniform float HO;
uniform float HM;
uniform vec3 betaMieScattering;
uniform bool ozoneLayerEnabled;
uniform int SAMPLES_MU;
uniform int SAMPLES_MU_S;
uniform int SAMPLES_NU;
uniform sampler2D transmittanceTexture;
uniform float r;
uniform vec4 dhdH;


void integrand(float r, float mu, float muSun, float nu, float y, out vec3 S_R,
               out vec3 S_M)
{
  // The integral's integrand is the single inscattering radiance:
  // S[L0] = P_M*S_M[L0] + P_R*S_R[L0]
  // where S_M[L0] = T*(betaMScattering * exp(-h/H_M))*L0 and
  // S_R[L0] = T*(betaRScattering * exp(-h/H_R))*L0.
  // T = transmittance.
  // One must remember that because the occlusion on L0, the integrand here will be equal
  // to 0 in that cases. Also it is important to remember that the phase function for the
  // Rayleigh and Mie scattering are added during the rendering time to increase the
  // angular precision
  S_R = vec3(0.0);
  S_M = vec3(0.0);

  // cosine law
  float ri = max(sqrt(r * r + y * y + 2.0 * r * mu * y), Rg);

  // Considering the Sun as a parallel light source, thew vector s_i = s.
  // So muSun_i = (vec(y_i) dot vec(s))/r_i = ((vec(x) + vec(yi-x)) dot vec(s))/r_i
  // muSun_i = (vec(x) dot vec(s) + vec(yi-x) dot vec(s))/r_i = (r*muSun + yi*nu)/r_i
  float muSun_i = (nu * y + muSun * r) / ri;

  // If the muSun_i is smaller than the angle to horizon (no sun radiance hitting the
  // point y), we return S_R = S_M = 0.0.
  if (muSun_i >= -sqrt(1.0 - Rg * Rg / (ri * ri))) {
    // It's the transmittance from the point y (ri) to the top of atmosphere in direction
    // of the sun (muSun_i) and the transmittance from the observer at x (r) to y (ri).
    vec3 transmittanceY =
      transmittance(transmittanceTexture, r, mu, y, Rg, Rt) *
      transmittance(transmittanceTexture, ri, muSun_i, Rg, Rt);
    // exp(-h/H)*T(x,v)
    if (ozoneLayerEnabled) {
      S_R = (exp(-(ri - Rg) / HO) + exp(-(ri - Rg) / HR)) * transmittanceY;
      S_M = exp(-(ri - Rg) / HM) * transmittanceY;
    }
    else {
      S_R = exp(-(ri - Rg) / HR) * transmittanceY;
      S_M = exp(-(ri - Rg) / HM) * transmittanceY;
    }
    // The L0 (sun radiance) is added in real-time.
  }
}

void inscatter(float r, float mu, float muSun, float nu, out vec3 S_R, out vec3 S_M) {
  // Let's calculate S_M and S_R by integration along the eye ray path inside the
  // atmosphere, given a position r, a view angle (cosine) mu, a sun position angle
  // (cosine) muSun, and the angle (cosine) between the sun position and the view
  // direction, nu. Integrating using the Trapezoidal rule:
  // Integral(f(y)dy)(from a to b) = (b-a)/2n_steps*(Sum(f(y_i+1)+f(y_i)))
  S_R = vec3(0.0);
  S_M = vec3(0.0);

  float rayDist = rayDistance(r, mu, Rt, Rg);
  float dy = rayDist / float(INSCATTER_INTEGRAL_SAMPLES);
  vec3 S_Ri;
  vec3 S_Mi;
  integrand(r, mu, muSun, nu, 0.0, S_Ri, S_Mi);
  for (int i = 1; i <= INSCATTER_INTEGRAL_SAMPLES; i++) {
    float yj = float(i) * dy;
    vec3 S_Rj;
    vec3 S_Mj;
    integrand(r, mu, muSun, nu, yj, S_Rj, S_Mj);
    S_R += (S_Ri + S_Rj);
    S_M += (S_Mi + S_Mj);
    S_Ri = S_Rj;
    S_Mi = S_Mj;
  }
  S_R *= betaRayleigh * (rayDist / (2.0 * float(INSCATTER_INTEGRAL_SAMPLES)));
  S_M *= betaMieScattering * (rayDist / (2.0 * float(INSCATTER_INTEGRAL_SAMPLES)));
}


void main() {
  // From the layer interpolation (see C++ code for layer to r) and the textures
  // parameters (uv), we unmapping mu, muSun and nu.
  float mu, muSun, nu;
  unmappingMuMuSunNu(r, dhdH, SAMPLES_MU, Rg, Rt, SAMPLES_MU_S, SAMPLES_NU, mu, muSun, nu);

  // Here we calculate the single inScattered light. Because this is a single
  // inscattering, the light that arrives at a point y in the path from the eye to the
  // infinity (top of atmosphere or planet's ground), comes only from the light source,
  // i.e., the sun. So, the there is no need to integrate over the whole solid angle
  // (4pi), we need only to consider the Sun position (cosine of sun pos = muSun). Then,
  // following the paper notation:
  // S[L] = P_R*S_R[L0] + P_M*S_M[L0] + S[L*]
  // For single inscattering only:
  // S[L0] = P_R*S_R[L0] + P_M*S_M[L0]
  // In order to save memory, we just store the red component of S_M[L0], and later we use
  // the proportionality rule to calcule the other components.
  vec3 S_R; // First Order Rayleigh InScattering
  vec3 S_M; // First Order Mie InScattering
  inscatter(r, mu, muSun, nu, S_R, S_M);
  renderTarget1 = vec4(S_R, 1.0);
  renderTarget2 = vec4(S_M, 1.0);
}
