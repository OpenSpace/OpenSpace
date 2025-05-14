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

out vec4 renderTarget;

uniform float Rg;
uniform float Rt;
uniform int SAMPLES_R;
uniform int SAMPLES_MU;
uniform int SAMPLES_MU_S;
uniform int SAMPLES_NU;
uniform sampler2D transmittanceTexture;
uniform float r;
uniform vec4 dhdH;
uniform sampler3D deltaJTexture;


// The integrand here is the f(y) of the trapezoidal rule:
vec3 integrand(float r, float mu, float muSun, float nu, float dist) {
  // We can calculate r_i by the cosine law: r_i^2=dist^2 + r^2 - 2*r*dist*cos(PI-theta)
  float r_i = sqrt(r * r + dist * dist + 2.0 * r * dist * mu);
  // r_i can be found using the dot product:
  // vec(y_i) dot vec(dist) = cos(theta_i) * ||vec(y_i)|| * ||vec(dist)||
  // But vec(y_i) = vec(x) + vec(dist), also: vec(x) dot vec(dist) = cos(theta) = mu
  // So, cos(theta_i) = mu_i = (r*dist**mu + dist*2)/(r_i*dist)
  float mu_i = (r * mu + dist) / r_i;
  // muSun_i can also be found by the dot product:
  // cos(sigma_i) = muSun_i = (vec(s) dot vec(y_i))/(||vec(y_i)|| * ||vec(s)||)
  // But vec(y_i) = vec(x) + vec(dist), and vec(x) dot vec(s) = muSun, cos(sigma_i + theta_i) = nu
  float muSun_i = (r * muSun + dist * nu) / r_i;
  // The irradiance attenuated from point r until y (y-x = dist)
  return
    transmittance(transmittanceTexture, r, mu, dist, Rg, Rt) *
    texture4D(deltaJTexture, r_i, mu_i, muSun_i, nu, Rg, SAMPLES_MU, Rt, SAMPLES_R,
      SAMPLES_MU_S, SAMPLES_NU).rgb;
}

vec3 inscatter(float r, float mu, float muSun, float nu) {
  vec3 inScatteringRadiance = vec3(0.0);
  float dy = rayDistance(r, mu, Rt, Rg) / float(INSCATTER_INTEGRAL_SAMPLES);
  vec3 inScatteringRadiance_i = integrand(r, mu, muSun, nu, 0.0);

  // In order to solve the integral from equation (11) we use the trapezoidal rule:
  // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
  // where y_i+1 = y_j
  for (int i = 1; i <= INSCATTER_INTEGRAL_SAMPLES; i++) {
    float y_j = float(i) * dy;
    vec3 inScatteringRadiance_j = integrand(r, mu, muSun, nu, y_j);
    inScatteringRadiance += (inScatteringRadiance_i + inScatteringRadiance_j) / 2.0 * dy;
    inScatteringRadiance_i = inScatteringRadiance_j;
  }
  return inScatteringRadiance;
}


void main() {
  float mu = 0.0;
  float muSun = 0.0;
  float nu = 0.0;
  // Unmapping the variables from texture texels coordinates to mapped coordinates
  unmappingMuMuSunNu(r, dhdH, SAMPLES_MU, Rg, Rt, SAMPLES_MU_S, SAMPLES_NU, mu, muSun, nu);

  // Write to texture deltaSR
  renderTarget = vec4(inscatter(r, mu, muSun, nu), 1.0);
}
