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

void getMuMuSNu(const float r, vec4 dhdH, out float mu, out float mu_s, out float nu) {
    float x = gl_FragCoord.x - 0.5;
    float y = gl_FragCoord.y - 0.5;
    if (y < float(RES_MU) / 2.0) {
        float d = 1.0 - y / (float(RES_MU) / 2.0 - 1.0);
        d = min(max(dhdH.z, d * dhdH.w), dhdH.w * 0.999);
        mu = (Rg * Rg - r * r - d * d) / (2.0 * r * d);
        mu = min(mu, -sqrt(1.0 - (Rg / r) * (Rg / r)) - 0.001);
    } else {
        float d = (y - float(RES_MU) / 2.0) / (float(RES_MU) / 2.0 - 1.0);
        d = min(max(dhdH.x, d * dhdH.y), dhdH.y * 0.999);
        mu = (Rt * Rt - r * r - d * d) / (2.0 * r * d);
    }
    mu_s = mod(x, float(RES_MU_S)) / (float(RES_MU_S) - 1.0);
    mu_s = tan((2.0 * mu_s - 1.0 + 0.26) * 1.1) / tan(1.26 * 1.1);
    nu = -1.0 + floor(x / float(RES_MU_S)) / (float(RES_NU) - 1.0) * 2.0;
}

vec3 transmittanceFromTexture(const float r, const float mu) {
    float u_r  = sqrt((r - Rg) / (Rt - Rg));
    // See Colliene to understand the different mapping.
    float u_mu = atan((mu + 0.15) / (1.0 + 0.15) * tan(1.5)) / 1.5;
    
    return texture(transmittanceTexture, vec2(u_mu, u_r)).rgb;
}

vec3 transmittance(const float r, const float mu, float d) {
    vec3 result;
    float r1 = sqrt(r * r + d * d + 2.0 * r * mu * d);
    float mu1 = (r * mu + d) / r1;
    if (mu > 0.0) {
        result = min(transmittanceFromTexture(r, mu) / 
            transmittanceFromTexture(r1, mu1), 1.0);
    } else {
        result = min(transmittanceFromTexture(r1, -mu1) / 
            transmittanceFromTexture(r, -mu), 1.0);
    }
    return result;
}

void integrand(const float r, const float mu, const float muS, const float nu, 
                const float t, out vec3 ray, out vec3 mie) {
    ray = vec3(0.0);
    mie = vec3(0.0);
    float ri = sqrt(r * r + t * t + 2.0 * r * mu * t);
    float muSi = (nu * t + muS * r) / ri;
    ri = max(Rg, ri);
    if (muSi >= -sqrt(1.0 - Rg * Rg / (ri * ri))) {
        vec3 ti = transmittance(r, mu, t) * transmittanceFromTexture(ri, muSi);
        ray = exp(-(ri - Rg) / HR) * ti;
        mie = exp(-(ri - Rg) / HM) * ti;
    }
}

float rayDistance(const float r, const float mu) {
    // cosine law
    float distanceAtmosphereIntersect = -r * mu + sqrt(r * r * (mu * mu - 1.0) + 
    (Rt + ATM_EPSILON)*(Rt + ATM_EPSILON)); 
    float distance = distanceAtmosphereIntersect;      
    float delta = r * r * (mu * mu - 1.0) + Rg * Rg;
    // No imaginary numbers... :-)
    if (delta >= 0.0) {
        float distanceEarthIntersect = -r * mu - sqrt(delta);
        if (distanceEarthIntersect >= 0.0) {
            distance = min(distanceAtmosphereIntersect, distanceEarthIntersect);
        }
    }
    return distance;
}

void inscatter(float r, float mu, float muS, float nu, out vec3 ray, out vec3 mie) {
    // Integrating using the Trapezoidal rule:
    // Integral(f(y)dy)(from a to b) = (b-a)/2n_steps*(Sum(f(y_i+1)+f(y_i)))
    ray = vec3(0.0);
    mie = vec3(0.0);
    float dx = rayDistance(r, mu) / float(INSCATTER_INTEGRAL_SAMPLES);
    float xi = 0.0;
    vec3 rayi;
    vec3 miei;
    integrand(r, mu, muS, nu, 0.0, rayi, miei);
    for (int i = 1; i <= INSCATTER_INTEGRAL_SAMPLES; ++i) {
        float xj = float(i) * dx;
        vec3 rayj;
        vec3 miej;
        integrand(r, mu, muS, nu, xj, rayj, miej);
        ray += (rayi + rayj) / 2.0 * dx;
        mie += (miei + miej) / 2.0 * dx;
        xi = xj;
        rayi = rayj;
        miei = miej;
    }
    ray *= betaR;
    mie *= betaMSca;
}

void main(void) {
    vec3 ray;
    vec3 mie;
    float mu, muS, nu;
    getMuMuSNu(r, dhdH, mu, muS, nu);
    inscatter(r, mu, muS, nu, ray, mie);
    // store separately Rayleigh and Mie contributions, WITHOUT the phase function factor
    // (cf "Angular precision")
    renderTarget1 = vec4(ray, 1.0);
    renderTarget2 = vec4(mie, 1.0);
}