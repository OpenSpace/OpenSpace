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
#version __CONTEXT__

#include "atmosphere_common.glsl"

out vec4 renderTableColor;

const int ozoneConcentration = 1;
const int hidrostaticApproximation = 2;

// - Height and scaleHeight in Km
float hidrostaticConcentration(const float height, const float scaleHeight) {
  return exp(-height / scaleHeight);
  //eturn pow(2.f, -height / scaleHeight);
}

// - height in Km
float ozoneDensityConcentration(const float height) {
  float x = height;
  float result = 0.0;
  if (x >= 0.0 && x <= 1.0)
    result = -1.9594 * 1e10 * x*x*x + -9.1994 * 1e-47 * x*x + 1.9594 * 1e10 * x + 7.2000 * 1e11 ;
  else if (x > 1.0 && x <= 2.0)
    result = 2.7970 * 1e10 * x*x*x + -1.4269 * 1e11 * x*x + 1.6229 * 1e11 * x + 6.7244 * 1e11 ;
  else if (x > 2.0 && x <= 3.0)
    result = -2.2855 * 1e9 * x*x*x + 3.8841 * 1e10 * x*x + -2.0078 * 1e11 * x + 9.1448 * 1e11 ;
  else if (x > 3.0 && x <= 5.0)
    result = -1.7748 * 1e9 * x*x*x + 3.4244 * 1e10 * x*x + -1.8699 * 1e11 * x + 9.0069 * 1e11 ;
  else if (x > 5.0 && x <= 6.0)
    result = 3.3197 * 1e7 * x*x*x + 7.1246 * 1e9 * x*x + -5.1392 * 1e10 * x + 6.7469 * 1e11 ;
  else if (x > 6.0 && x <= 7.0)
    result = 4.5889 * 1e9 * x*x*x + -7.4877 * 1e10 * x*x + 4.4062 * 1e11 * x + -3.0933 * 1e11 ;
  else if (x > 7.0 && x <= 8.0)
    result = 1.6113 * 1e9 * x*x*x + -1.2349 * 1e10 * x*x + 2.9247 * 1e9 * x + 7.1196 * 1e11 ;
  else if (x > 8.0 && x <= 15.0)
    result = -6.2040 * 1e8 * x*x*x + 4.1212 * 1e10 * x*x + -4.2557 * 1e11 * x + 1.8546 * 1e12; 
  else if (x > 15.0 && x <= 22.46)
    result = -3.6478 * 1e9 * x*x*x + 1.7744 * 1e11 * x*x + -2.4690 * 1e12 * x + 1.2072 * 1e13;
  else if (x > 22.46 && x <= 30.0)
    result = 4.0262 * 1e9 * x*x*x + -3.3962 * 1e11 * x*x + 9.1443 * 1e12 * x + -7.4873 * 1e13; 
  else if (x > 30.0 && x <= 41.0)
    result = -5.7254 * 1e8 * x*x*x + 7.4258 * 1e10 * x*x + -3.2722 * 1e12 * x + 4.9291 * 1e13; 
  else if (x > 41.0 && x <= 55.0)
    result = -8.0773 * 1e7 * x*x*x + 1.3770 * 1e10 * x*x + -7.9215 * 1e11 * x + 1.5398 * 1e13;
  else if (x > 55.0)
    result = -2.9494 * 1e7 * x*x*x + 5.3090 * 1e9 * x*x + -3.2680 * 1e11 * x + 6.8666 * 1e12;

  return result * 5e4; // 1e5 is the magic number here. It happens also in Mie and Rayleigh
}

//-- Optical depth by integration, from ray starting at point vec(x), i.e,
// height r and angle mu (cosine of vec(v)) until top of atmosphere
// or planet's ground. --
// r        := height of starting point vect(x)
// mu       := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// H        := Thickness of atmosphere if its density were uniform (can be used
//             for Rayleigh and Mie.
// profType := concentration profile to be used
float opticalDepth(const float r, const float mu, const float H,
                   const int profType) {    
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
  
  float y_i = 0.f;

  if (profType == hidrostaticApproximation) {
    // cosine law
    y_i = hidrostaticConcentration(r - Rg, H);
  } else if (profType == ozoneConcentration) {
    y_i = ozoneDensityConcentration(r - Rg);
  }
  
  float x_step       = 0.0f;
  float accumulation = 0.0f;
  for (int i = 1; i <= TRANSMITTANCE_STEPS; ++i) {
    float x_i = float(i) * deltaStep;
    float y_ii = 0.f;
    
    // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
    // In this case, a = r, b = x_i and cos(alpha) = cos(PI-zenithView) = mu
    if (profType == hidrostaticApproximation) {
      y_ii = hidrostaticConcentration(sqrt(r2 + x_i * x_i + 2.0 * x_i * r * mu) - Rg, H);
    } else if (profType == ozoneConcentration) {
      y_ii = ozoneDensityConcentration(sqrt(r2 + x_i * x_i + 2.0 * x_i * r * mu) - Rg);
    }
    
    accumulation += (y_ii + y_i);
    //x_step = x_i;
    y_i = y_ii;
  }
  return accumulation * ( b_a / ( 2 * TRANSMITTANCE_STEPS ) );
}

void main(void) {
  float r, muSun;    

  unmappingRAndMu(r, muSun);
  
  vec3 opDepth = betaMieExtinction * opticalDepth(r, muSun, HM, hidrostaticApproximation) +
                 betaRayleigh * opticalDepth(r, muSun, HR, hidrostaticApproximation);
  
  // Oxygen absorption enabled
  if (oxygenAbsLayerEnabled) {
    //opDepth += (vec3(7.398e-30, 3.4e-29, 1.337e-29) * 0.2046 * 2.687e25 * opticalDepth(r, muSun, HR, hidrostaticApproximation));
    opDepth += vec3(4.164e-5, 1.914e-4, 7.525e-5) * opticalDepth(r, muSun, HR, hidrostaticApproximation);
  }
  
  if (ozoneLayerEnabled) {
    opDepth += vec3(1.368209e-21, 3.314053e-21, 1.360173e-22) * opticalDepth(r, muSun, 0.f, ozoneConcentration);
  } 

  renderTableColor = vec4( exp( -opDepth ), 1.0f );
}
