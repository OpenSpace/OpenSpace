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

//layout(location = 1) out vec4 renderTableColor;
out vec4 renderTableColor;

//-- Optical depth by integration, from ray starting at point vec(x), i.e,
// height r and angle mu (cosine of vec(v)) until top of atmosphere
// or planet's ground. --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// H := Thickness of atmosphere if its density were uniform (can be used
//      for Rayleigh and Mie.
float opticalDepth(const float r, const float mu, const float H) {    
  float r2 = r*r;
  // Is ray below horizon? The transmittance table will have only
  // the values for transmittance starting at r (x) until the
  // light ray touches the atmosphere or the ground and only for
  // view angles v between 0 and pi/2 + eps. That's because we
  // can calculate the transmittance for angles bigger than pi/2
  // just inverting the ray direction and starting and ending points.
  
  // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
  float cosZenithHorizon = -sqrt( 1.0f - ( ( Rg * Rg ) / r2 ) );
  if (mu < cosZenithHorizon)
    return 1e9;

  // Integrating using the Trapezoidal rule:
  // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
  float b_a = rayDistance(r, mu);
  float deltaStep = b_a / float(TRANSMITTANCE_STEPS);
  // cosine law
  float y_i = exp(-(r - Rg) / H);
  
  float x_step       = 0.0f;
  float accumulation = 0.0f;
  for (int i = 1; i <= TRANSMITTANCE_STEPS; ++i) {
    float x_i = float(i) * deltaStep;
    // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
    // In this case, a = r, b = x_i and cos(alpha) = cos(PI-zenithView) = mu
    float y_ii    = exp(-(sqrt(r2 + x_i * x_i + 2.0 * x_i * r * mu) - Rg) / H);
    accumulation += (y_ii + y_i);
    //x_step = x_i;
    y_i = y_ii;
  }
  return accumulation * ( b_a / ( 2 * TRANSMITTANCE_STEPS ) );
}


void main(void) {
  float r, muSun;    
  unmappingRAndMu(r, muSun);
  vec3 opDepth = betaMieExtinction * opticalDepth(r, muSun, HM) +
    betaRayleigh * opticalDepth(r, muSun, HR);
  
  renderTableColor = vec4( exp( -opDepth ), 0.0f );
}
