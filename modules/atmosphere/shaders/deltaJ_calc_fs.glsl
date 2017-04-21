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

out vec4 renderTarget1;

uniform float r;
uniform vec4 dhdH;

uniform sampler2D deltaETexture;
uniform sampler3D deltaSRTexture;
uniform sampler3D deltaSMTexture;

uniform int firstIteraction;

// -- Spherical Coordinates Steps. phi e [0,2PI] and theta e [0, PI]
const float stepPhi = (2.0f * M_PI) / float(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);
const float stepTheta = M_PI / float(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);

void inscatter(float r, float mu, float muSun, float nu, out vec3 radianceJ) {
  // Be sure to not get a cosine or height out of bounds
  r     = clamp(r, Rg, Rt);
  mu    = clamp(mu, -1.0f, 1.0f);
  muSun = clamp(muSun, -1.0f, 1.0f);

  //  s sigma | theta v
  //   \      |      /
  //    \     |     /
  //     \    |    /
  //      \   |   /   theta + signam = ni
  //       \  |  /    cos(theta) = mu
  //        \ | /     cos(sigma) = muSun
  //         \|/      cos(ni)    = nu
  float mu2              = mu * mu;
  float muSun2           = muSun * muSun;
  float sinThetaSinSigma = sqrt(1.0f - mu2) * sqrt(1.0f - muSun2);
  // cos(sigma + theta) = cos(theta)cos(sigma)-sin(theta)sin(sigma)
  // cos(ni) = nu = mu * muSun - sqrt(1.0f - mu*mu)*sqrt(1.0 - muSun*muSun) // sin(theta) = sqrt(1.0 - mu*mu)
  // Now we make sure the angle between vec(s) and vec(v) is in the right range:
  nu = clamp(nu, muSun * mu - sinThetaSinSigma, muSun * mu + sinThetaSinSigma);

  // Lets calculate the consine of the angle to the horizon:
  // theta is the angle between vec(v) and x
  // cos(PI-theta) = d/r
  // -cos(theta) = sqrt(r*r-Rg*Rg)/r
  float Rg2        = Rg * Rg;
  float r2         = r * r;
  float cosHorizon = -sqrt(r2 - Rg2)/r;

  // Now we get vec(v) and vec(s) from mu, muSun and nu:
  // Assuming:
  //              z |theta 
  //                |\ vec(v) ||vec(v)|| = 1
  //                | \
  //                |__\_____x
  // sin(PI-theta) = x/||v|| => x = sin(theta) =? x = sqrt(1-mu*mu)
  // cos(PI-theta) = z/||v|| => z = cos(theta) = mu
  // v.y = 0 because ||v|| = 1
  vec3 v   = vec3(sqrt(1.0 - mu2), 0.0, mu);

  // To obtain vec(s), we use the following properties:
  // ||vec(s)|| = 1, ||vec(v)|| = 1
  // vec(s) dot vec(v) = cos(ni) = nu
  // Following the same idea for vec(v), we now that s.z = cos(sigma) = muSun
  // So, from vec(s) dot vec(v) = cos(ni) = nu we have,
  // s.x*v.x +s.y*v.y + s.z*v.z = nu
  // s.x = (nu - s.z*v.z)/v.x = (nu - mu*muSun)/v.x
  float sx = (v.x == 0.0) ? 0.0 : (nu - muSun * mu) / v.x;
  // Also, ||vec(s)|| = 1, so:
  // 1 = sqrt(s.x*s.x + s.y*s.y + s.z*s.z)
  // s.y = sqrt(1 - s.x*s.x - s.z*s.z) = sqrt(1 - s.x*s.x - muSun*muSun)
  vec3 s = vec3(sx, sqrt(max(0.0, 1.0 - sx * sx - muSun2)), muSun);
  
  // In order to integrate over 4PI, we scan the sphere using the spherical coordinates
  // previously defined
  for ( int theta_i = 0; theta_i < INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; ++theta_i ) {
    float theta               = (float(theta_i) + 0.5f) * stepTheta;
    float cosineTheta         = cos(theta);
    float cosineTheta2        = cosineTheta * cosineTheta;
    float distanceToGround    = 0.0f;
    float groundReflectance   = 0.0f;
    vec3  groundTransmittance = vec3(0.0f);
    
    // If the ray w can see the ground we must compute the transmittance
    // effect from the starting point x to the ground point in direction -vec(v):
    if ( cosineTheta < cosHorizon ) { // ray hits ground
      // AverageGroundReflectance e [0,1]
      groundReflectance = AverageGroundReflectance / M_PI;
      // From cosine law: Rg*Rg = r*r + distanceToGround*distanceToGround - 2*r*distanceToGround*cos(PI-theta)
      distanceToGround = -r * cosineTheta - sqrt(r2 * (cosineTheta * cosineTheta - 1.0f) + Rg2);
      //               |
      //               | theta
      //               |
      //               |\ distGround
      //            r  | \  alpha
      //               |  \/
      //               |  /  
      //               | / Rg
      //               |/
      // So cos(alpha) = ((vec(x)+vec(dg)) dot -vec(distG))/(||(vec(x)+vec(distG))|| * ||vec(distG)||)
      //    cos(alpha) = (-r*distG*cos(theta) - distG*distG)/(Rg*distG)
      //      muGround = -(r*cos(theta) + distG)/Rg
      //float muGround = -(r * cosineTheta + distanceToGround) / Rg;
      // We can use the same triangle in calculate the distanceToGround to calculate the cosine of the
      // angle between the ground touching point at height Rg and the zenith angle
      float muGround = (r2 - distanceToGround*distanceToGround - Rg2)/(2*distanceToGround*Rg);
      // Acesss the Transmittance LUT in order to calculate the transmittance from the ground point Rg,
      // thorugh the atmosphere, at a distance: distanceToGround
      groundTransmittance = transmittance(Rg, muGround, distanceToGround);
    }
    //for ( int phi_i = 0; phi_i < 2*INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; ++phi_i ) {
    for ( int phi_i = 0; phi_i < INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; ++phi_i ) {
      float phi   = (float(phi_i) + 0.5) * stepPhi;
      // spherical coordinates: dw = dtheta*dphi*sin(theta)*rho^2
      // rho = 1, we are integrating over a unit sphere
      float dw    = stepTheta * stepPhi * sin(theta);
      // w = (rho*sin(theta)*cos(phi), rho*sin(theta)*sin(phi), rho*cos(theta))
      float sinPhi   = sin(phi);
      float sinTheta = sin(theta);
      float cosPhi   = cos(phi);
      vec3  w        = vec3(sinTheta * cosPhi, sinTheta * sinPhi, cosineTheta);

      // We calculate the Rayleigh and Mie phase function for the new scattering angle:
      // cos(angle between vec(v) and vec(w)), ||v|| = ||w|| = 1
      float nuWV            = dot(v, w);
      float phaseRayleighWV = rayleighPhaseFunction(nuWV);
      float phaseMieWV      = miePhaseFunction(nuWV);
      
      vec3 groundNormal     = (vec3(0.0, 0.0, r) + distanceToGround * w) / Rg;
      vec3 groundIrradiance = irradianceLUT(deltaETexture, dot(groundNormal, s), Rg);

      // We finally calculate the radiance from the reflected ray from ground (0.0 if not reflected)
      vec3 radianceJ1 = groundTransmittance * groundReflectance * groundIrradiance;

      // We calculate the Rayleigh and Mie phase function for the new scattering angle:
      // cos(angle between vec(s) and vec(w)), ||s|| = ||w|| = 1
      float nuSW = dot(s, w);
      // The first iteraction is different from the others, that's because in the first
      // iteraction all the light InScattered are coming from the initial pre-computed
      // single InScattered light. We stored these values in the deltaS textures (Ray and Mie),
      // and in order to avoid problems with the high angle dependency in the phase functions,
      // we don't include the phase functions on those tables (that's why we calculate them now).
      if ( firstIteraction == 1 ) {        
        float phaseRaySW = rayleighPhaseFunction(nuSW);
        float phaseMieSW = miePhaseFunction(nuSW);
        // We can now access the values for the single InScattering in the textures deltaS textures.
        vec3  singleRay = texture4D(deltaSRTexture, r, w.z, muSun, nuSW).rgb;
        vec3  singleMie = texture4D(deltaSMTexture, r, w.z, muSun, nuSW).rgb;

        // Initial InScattering including the phase functions
        radianceJ1 += singleRay * phaseRaySW + singleMie * phaseMieSW;        
      } else {
        // On line 9 of the algorithm, the texture table deltaSR is updated, so when we are not in the first
        // iteraction, we are getting the updated result of deltaSR (not the single inscattered light but the
        // accumulated (higher order) inscattered light.
        // w.z is the cosine(theta) = mu for vec(w)
        radianceJ1 += texture4D(deltaSRTexture, r, w.z, muSun, nuSW).rgb;
      }

      // Finally, we add the atmospheric scale height (See: Radiation Transfer on the Atmosphere and Ocean from
      // Thomas and Stamnes, pg 9-10.
      radianceJ += radianceJ1 * (betaRayleigh * exp(-(r - Rg) / HR) * phaseRayleighWV +
                                 betaMieScattering * exp(-(r - Rg) / HM) * phaseMieWV) * dw;
    }
  }
}

void main(void) {
  // cosine variables to access deltaS textures
  float mu, muSun, nu;
  // InScattering Radiance to be calculated at
  // different points in the ray path
  vec3 radianceJ = vec3(0.0f);

  // Unmapping the variables from texture texels coordinates
  // to mapped coordinates
  unmappingMuMuSunNu(r, dhdH, mu, muSun, nu);

  // Calculate the the light inScattered in direction
  // -vec(v) for the point at height r (vec(y) following Bruneton and Neyret's paper
  inscatter(r, mu, muSun, nu, radianceJ);

  // Write to texture detaJ
  renderTarget1 = vec4(radianceJ, 1.0);
}
