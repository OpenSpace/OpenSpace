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
 
#version __CONTEXT__

#include "atmosphere_common.glsl"

out vec4 renderTableColor;

uniform int firstIteraction;
//uniform float firstIteraction;

// -- Spherical Coordinates Steps. phi e [0,2PI] and theta e [0, PI/2]
const float stepPhi = (2.0f * M_PI) / float(IRRADIANCE_INTEGRAL_SAMPLES);
const float stepTheta = M_PI / (2.0f * float(IRRADIANCE_INTEGRAL_SAMPLES));

//uniform sampler2D transmittanceTexture;
uniform sampler3D deltaSRTexture;
uniform sampler3D deltaSMTexture;

void main(void) {
  
  float r     = 0.0f;
  float muSun = 0.0f;
  // Unmapping the variables from texture texels coordinates
  // to mapped coordinates
  unmappingRAndMuSunIrradiance(r, muSun);

  // We know that muSun = cos(sigma) = s.z/||s||
  // But, ||s|| = 1, so s.z = muSun. Also,
  // ||s|| = 1, so s.x = sin(sigma) = sqrt(1-muSun^2) and s.y = 0.0f
  vec3 s = vec3(max(sqrt(1.0f - muSun * muSun), 0.0f), 0.0f, muSun);

  // In order to solve the integral from equation (15) we use the trapezoidal
  // rule: Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
  vec3 irradianceE = vec3(0.0f);
  for (int iphi = 0; iphi < IRRADIANCE_INTEGRAL_SAMPLES; ++iphi) {
    float phi = (float(iphi) + 0.5f) * stepPhi;
    for (int itheta = 0; itheta < IRRADIANCE_INTEGRAL_SAMPLES; ++itheta) {
      float theta = (float(itheta) + 0.5f) * stepTheta;
      // spherical coordinates: dw = dtheta*dphi*sin(theta)*rho^2
      // rho = 1, we are integrating over a unit sphere
      float dw    = stepTheta * stepPhi * sin(theta);
      // w = (cos(phi) * sin(theta) * rho, sin(phi) * sin(theta) * rho, cos(theta) * rho)
      vec3  w     = vec3(cos(phi) * sin(theta), sin(phi) * sin(theta), cos(theta));
      float nu    = dot(s, w);

      // The first iteraction is different from the others, that's because in the first
      // iteraction all the light arriving are coming from the initial pre-computed
      // single scattered light. We stored these values in the deltaS textures (Ray and Mie),
      // and in order to avoid problems with the high angle dependency in the phase functions,
      // we don't include the phase functions on those tables (that's why we calculate them now).
      if (firstIteraction == 1) {
        float phaseRay = rayleighPhaseFunction(nu);
        float phaseMie = miePhaseFunction(nu);
        vec3 singleRay = texture4D(deltaSRTexture, r, w.z, muSun, nu).rgb;
        vec3 singleMie = texture4D(deltaSMTexture, r, w.z, muSun, nu).rgb;
        // w.z is the cosine(theta) = mu for vec(w) and also vec(w) dot vec(n(xo))
        irradianceE += (singleRay * phaseRay + singleMie * phaseMie) * w.z * dw;
      } else {
        // On line 10 of the algorithm, the texture table deltaE is updated, so when we are not in the first
        // iteraction, we are getting the updated result of deltaE (not the single irradiance light but the
        // accumulated (higher order) irradiance light.
        // w.z is the cosine(theta) = mu for vec(w) and also vec(w) dot vec(n(xo))
        irradianceE += texture4D(deltaSRTexture, r, w.z, muSun, nu).rgb * w.z * dw;
      }
    }
  }

  // Write the higher oder irradiance to texture deltaE
  renderTableColor = vec4(irradianceE, 0.0);    
}
