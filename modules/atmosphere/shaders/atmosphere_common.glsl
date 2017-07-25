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

const float M_PI = 3.141592657;

uniform sampler2D transmittanceTexture;

float opticalDepth(const float H, const float r, const float mu, const float d) {
  float a    = sqrt((0.5/H)*r);
  vec2 a01   = a*vec2(mu, mu + d / r);
  vec2 a01s  = sign(a01);
  vec2 a01sq = a01*a01;
  float x    = a01s.y > a01s.x ? exp(a01sq.x) : 0.0;
  vec2 y     = a01s / (2.3193*abs(a01) + sqrt(1.52*a01sq + 4.0)) * vec2(1.0, exp(-d/H*(d/(2.0*r)+mu)));
  return sqrt((6.2831*H)*r) * exp((Rg-r)/H) * (x + dot(y, vec2(1.0, -1.0)));
}

vec3 analyticTransmittance(const float r, const float mu, const float d) {
  if (ozoneLayerEnabled) {
    return exp(-betaRayleigh * opticalDepth(HR, r, mu, d) -
             betaOzoneExtinction * (0.0000006) * opticalDepth(HO, r, mu, d) -
             betaMieExtinction * opticalDepth(HM, r, mu, d));
  } else {
    return exp(-betaRayleigh * opticalDepth(HR, r, mu, d) -
               betaMieExtinction * opticalDepth(HM, r, mu, d));
  }
}

vec3 irradiance(sampler2D sampler, const float r, const float muSun) {
  float u_r     = (r - Rg) / (Rt - Rg);
  float u_muSun = (muSun + 0.2) / (1.0 + 0.2);
  return texture(sampler, vec2(u_muSun, u_r)).rgb;
}


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

//-- Given the window's fragment coordinates, for a defined
// viewport, gives back the interpolated r e [Rg, Rt] and
// mu e [-1, 1] --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
void unmappingRAndMu(out float r, out float mu) {
  float u_mu  = gl_FragCoord.x / float(TRANSMITTANCE_W);
  float u_r   = gl_FragCoord.y / float(TRANSMITTANCE_H);
  
  // In the paper u_r^2 = (r^2-Rg^2)/(Rt^2-Rg^2)
  // So, extracting r from u_r in the above equation:
  //r  = sqrt( Rg * Rg + (u_r * u_r) * (Rt * Rt - Rg * Rg) );
  r  = Rg + (u_r * u_r) * (Rt - Rg);
  
  // In the paper the Bruneton suggest mu = dot(v,x)/||x|| with ||v|| = 1.0
  // Later he proposes u_mu = (1-exp(-3mu-0.6))/(1-exp(-3.6))
  // But the below one is better. See Colliene.
  // One must remember that mu is defined from 0 to PI/2 + epsillon. 
  mu = -0.15f + tan(1.5f * u_mu) / tan(1.5f) * (1.0f + 0.15f);
}

//-- Given the windows's fragment coordinates, for a defined view port,
// gives back the interpolated r e [Rg, Rt] and muSun e [-1, 1] --
// r := height of starting point vect(x)
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
void unmappingRAndMuSun(out float r, out float muSun) {
  // See Bruneton and Colliene to understand the mapping.
  muSun = -0.2f + (gl_FragCoord.x - 0.5f) / (float(OTHER_TEXTURES_W) - 1.0f) * (1.0f + 0.2f);
  //r  = Rg + (gl_FragCoord.y - 0.5f) / (float(OTHER_TEXTURES_H) - 1.0f) * (Rt - Rg);
  r  = Rg + (gl_FragCoord.y - 0.5f) / (float(OTHER_TEXTURES_H) ) * (Rt - Rg);
}

//-- Given the windows's fragment coordinates, for a defined view port,
// gives back the interpolated r e [Rg, Rt] and muSun e [-1, 1] for the
// Irradiance deltaE texture table --
// r := height of starting point vect(x)
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
void unmappingRAndMuSunIrradiance(out float r, out float muSun) {
  // See Bruneton and Colliene to understand the mapping.
  muSun = -0.2f + (gl_FragCoord.x - 0.5f) / (float(SKY_W) - 1.0f) * (1.0f + 0.2f);
  r  = Rg + (gl_FragCoord.y - 0.5f) / (float(SKY_H) - 1.0f) * (Rt - Rg);
}

//-- Given the windows's fragment coordinates, for a defined view port,
// gives back the interpolated r e [Rg, Rt] and mu, muSun amd nu e [-1, 1] --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// nu := cosone of the angle between vec(s) and vec(v)
// dhdH := it is a vec4. dhdH.x stores the dminT := Rt - r, dhdH.y stores the dH value (see paper),
// dhdH.z stores dminG := r - Rg and dhdH.w stores dh (see paper).
void unmappingMuMuSunNu(const float r, vec4 dhdH, out float mu, out float muSun, out float nu) {
  // Window coordinates of pixel (uncentering also)
  float fragmentX = gl_FragCoord.x - 0.5f;
  float fragmentY = gl_FragCoord.y - 0.5f;

  // Pre-calculations
  float Rg2 = Rg * Rg;
  float Rt2 = Rt * Rt;
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


//-- Function to access the transmittance texture. Given r
// and mu, returns the transmittance of a ray starting at vec(x),
// height r, and direction vec(v), mu, and length until it hits
// the ground or the top of atmosphere. --
// r := height of starting point vect(x)
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
vec3 transmittanceLUT(const float r, const float mu) {
  // Given the position x (here the altitude r) and the view
  // angle v (here the cosine(v)= mu), we map this
  float u_r  = sqrt((r - Rg) / (Rt - Rg));
  //float u_r  = sqrt((r*r - Rg*Rg) / (Rt*Rt - Rg*Rg));
  // See Colliene to understand the different mapping.
  float u_mu = atan((mu + 0.15f) / (1.0f + 0.15f) * tan(1.5f)) / 1.5f;
  
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
  } else {
    return min(transmittanceLUT(ri, -mui) / 
               transmittanceLUT(r, -mu), 1.0f);
  }
}

// -- Calculates Rayleigh phase function given the
// scattering cosine angle mu --
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
float rayleighPhaseFunction(const float mu) {
    return (3.0f / (16.0f * M_PI)) * (1.0f + mu * mu);
}

// -- Calculates Mie phase function given the
// scattering cosine angle mu --
// mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
float miePhaseFunction(const float mu) {
  //return (3.0f / (8.0f * M_PI)) * 
  //      ( ( (1.0f - (mieG * mieG) ) * (1.0f + mu * mu) ) / 
  //      ( (2.0f + mieG * mieG) *
  //        pow(1.0f + mieG * mieG - 2.0f * mieG * mu, 3.0f/2.0f) ) );
  return 1.5f * 1.0f / (4.0f * M_PI) * (1.0f - mieG * mieG) *
    pow(1.0f + (mieG * mieG) - 2.0f * mieG * mu, -3.0f/2.0f) * (1.0f + mu * mu) / (2.0f + mieG*mieG);
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
  float Rg2    = Rg * Rg;
  float Rt2    = Rt * Rt;
  float r2     = r * r;
  float H      = sqrt(Rt2 - Rg2);
  float rho    = sqrt(r2 - Rg2);
  float rmu    = r * mu;
  float delta  = rmu * rmu - r2 + Rg2;
  vec4 cst     = rmu < 0.0f && delta > 0.0f ?
    vec4(1.0f, 0.0f, 0.0f, 0.5f - 0.5f / float(SAMPLES_MU)) :
    vec4(-1.0f, H * H, H, 0.5f + 0.5f / float(SAMPLES_MU));
  float u_r    = 0.5f / float(SAMPLES_R) + rho / H * (1.0f - 1.0f / float(SAMPLES_R));
  float u_mu   = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5f - 1.0f / float(SAMPLES_MU));
  float u_mu_s = 0.5f / float(SAMPLES_MU_S) +
    (atan(max(muSun, -0.1975) * tan(1.26f * 1.1f)) / 1.1f + (1.0f - 0.26f)) * 0.5f * (1.0f - 1.0f / float(SAMPLES_MU_S));
  float lerp = (nu + 1.0f) / 2.0f * (float(SAMPLES_NU) - 1.0f);
  float u_nu = floor(lerp);
  lerp = lerp - u_nu;
  return texture(table, vec3((u_nu + u_mu_s) / float(SAMPLES_NU), u_mu, u_r)) * (1.0f - lerp) +
    texture(table, vec3((u_nu + u_mu_s + 1.0f) / float(SAMPLES_NU), u_mu, u_r)) * lerp;
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
  float u_r     = (r - Rg) / (Rt - Rg);
  return texture(lut, vec2(u_muSun, u_r)).rgb;
}
