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

out vec4 renderTableColor;

uniform float Rg;
uniform float Rt;
uniform float HR;
uniform vec3 betaRayleigh;
uniform float HO;
uniform vec3 betaOzoneExtinction;
uniform float HM;
uniform vec3 betaMieExtinction;
uniform bool ozoneLayerEnabled;
uniform ivec2 TRANSMITTANCE;

const int TRANSMITTANCE_STEPS = 500;


// Optical depth by integration, from ray starting at point vec(x), i.e, height r and
// angle mu (cosine of vec(v)) until top of atmosphere or planet's ground.
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// H := Thickness of atmosphere if its density were uniform (used for Rayleigh and Mie)
float opticalDepth(float r, float mu, float H) {
  float r2 = r * r;
  // Is ray below horizon? The transmittance table will have only the values for
  // transmittance starting at r (x) until the light ray touches the atmosphere or the
  // ground and only for view angles v between 0 and pi/2 + eps. That's because we can
  // calculate the transmittance for angles bigger than pi/2 just inverting the ray
  // direction and starting and ending points.

  // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
  float cosZenithHorizon = -sqrt(1.0 - ((Rg * Rg) / r2));
  if (mu < cosZenithHorizon) {
    return 1e9;
  }

  // Integrating using the Trapezoidal rule:
  // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
  float b_a = rayDistance(r, mu, Rt, Rg);
  float deltaStep = b_a / float(TRANSMITTANCE_STEPS);
  // cosine law
  float y_i = exp(-(r - Rg) / H);

  float accumulation = 0.0;
  for (int i = 1; i <= TRANSMITTANCE_STEPS; i++) {
    float x_i = float(i) * deltaStep;
    // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
    // In this case, a = r, b = x_i and cos(alpha) = cos(PI-zenithView) = mu
    float y_ii = exp(-(sqrt(r2 + x_i * x_i + 2.0 * x_i * r * mu) - Rg) / H);
    accumulation += (y_ii + y_i);
    y_i = y_ii;
  }
  return accumulation * (b_a / (2.0 * TRANSMITTANCE_STEPS));
}


void main() {
  float u_mu  = gl_FragCoord.x / float(TRANSMITTANCE.x);
  float u_r = gl_FragCoord.y / float(TRANSMITTANCE.y);

  // In the paper u_r^2 = (r^2-Rg^2)/(Rt^2-Rg^2)
  // So, extracting r from u_r in the above equation:
  float r = Rg + (u_r * u_r) * (Rt - Rg);

  // In the paper the Bruneton suggest mu = dot(v,x)/||x|| with ||v|| = 1.0
  // Later he proposes u_mu = (1-exp(-3mu-0.6))/(1-exp(-3.6))
  // But the below one is better. See Collienne.
  // One must remember that mu is defined from 0 to PI/2 + epsilon
  float muSun = -0.15 + tan(1.5 * u_mu) / tan(1.5) * 1.15;

  vec3 ozoneContribution = vec3(0.0);
  if (ozoneLayerEnabled) {
    ozoneContribution = betaOzoneExtinction * 0.0000006 * opticalDepth(r, muSun, HO);
  }
  vec3 opDepth = ozoneContribution +
    betaMieExtinction * opticalDepth(r, muSun, HM) +
    betaRayleigh * opticalDepth(r, muSun, HR);

  renderTableColor = vec4(exp(-opDepth), 0.0);
}
