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
 
#include "atmosphere_common.glsl"
#include "fragment.glsl"
#include "PowerScaling/powerScalingMath.hglsl"

layout(location = 1) out vec4 renderTarget1;

uniform float r;
uniform vec4 dhdH;

uniform sampler2D transmittanceTexture;
uniform sampler2D deltaETexture;
uniform sampler3D deltaSRTexture;
uniform sampler3D deltaSMTexture;
uniform float first;

const float dphi = M_PI / float(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);
const float dtheta = M_PI / float(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);

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

// Rayleigh phase
float phaseFunctionR(const float mu) {
    return (3.0 / (16.0 * M_PI)) * (1.0 + mu * mu);
}

// Mie phase
float phaseFunctionM(const float mu) {
    return (3.0 / (8.0 * M_PI)) * 
        ( ( (1.0 - (mieG*mieG) ) * (1+mu*mu) ) / 
        ( (2+mieG*mieG) * pow(1+mieG*mieG - 2.0*mieG*mu, 3.0/2.0) ) );
}

vec3 irradiance(sampler2D calcTexture, const float r, const float mu_s) {
    float u_r = (r - Rg) / (Rt - Rg);
    float u_mu_s = (mu_s + 0.2) / (1.0 + 0.2);
    return texture(calcTexture, vec2(u_mu_s, u_r)).rgb;
}

vec4 texture4D(sampler3D table, const float r, const float mu, 
                const float mu_s, const float nu)
{
    float H = sqrt(Rt * Rt - Rg * Rg);
    float rho = sqrt(r * r - Rg * Rg);
    float rmu = r * mu;
    float delta = rmu * rmu - r * r + Rg * Rg;
    vec4 cst = rmu < 0.0 && delta > 0.0 ? vec4(1.0, 0.0, 0.0, 0.5 - 0.5 / float(RES_MU)) : vec4(-1.0, H * H, H, 0.5 + 0.5 / float(RES_MU));
	float u_r = 0.5 / float(RES_R) + rho / H * (1.0 - 1.0 / float(RES_R));
    float u_mu = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5 - 1.0 / float(RES_MU));
    float u_mu_s = 0.5 / float(RES_MU_S) + (atan(max(mu_s, -0.1975) * tan(1.26 * 1.1)) / 1.1 + (1.0 - 0.26)) * 0.5 * (1.0 - 1.0 / float(RES_MU_S));
    float lerp = (nu + 1.0) / 2.0 * (float(RES_NU) - 1.0);
    float uNu = floor(lerp);
    lerp = lerp - uNu;
    return texture(table, vec3((uNu + u_mu_s) / float(RES_NU), u_mu, u_r)) * (1.0 - lerp) +
           texture(table, vec3((uNu + u_mu_s + 1.0) / float(RES_NU), u_mu, u_r)) * lerp;
}

void inscatter(float r, float mu, float mu_s, float nu, out vec3 raymie) {
    r = clamp(r, Rg, Rt);
    mu = clamp(mu, -1.0, 1.0);
    mu_s = clamp(mu_s, -1.0, 1.0);
    float var = sqrt(1.0 - mu * mu) * sqrt(1.0 - mu_s * mu_s);
    nu = clamp(nu, mu_s * mu - var, mu_s * mu + var);

    float cthetamin = -sqrt(1.0 - (Rg / r) * (Rg / r));

    vec3 v = vec3(sqrt(1.0 - mu * mu), 0.0, mu);
    float sx = v.x == 0.0 ? 0.0 : (nu - mu_s * mu) / v.x;
    vec3 s = vec3(sx, sqrt(max(0.0, 1.0 - sx * sx - mu_s * mu_s)), mu_s);

    raymie = vec3(0.0);

    for (int itheta = 0; itheta < INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; ++itheta) {
        float theta = (float(itheta) + 0.5) * dtheta;
        float ctheta = cos(theta);

        float greflectance = 0.0;
        float dground = 0.0;
        vec3 gtransp = vec3(0.0);
        if (ctheta < cthetamin) { 
            greflectance = AVERAGE_GROUND_REFLECTANCE / M_PI;
            dground = -r * ctheta - sqrt(r * r * (ctheta * ctheta - 1.0) + Rg * Rg);
            gtransp = transmittance(Rg, -(r * ctheta + dground) / Rg, dground);
        }

        for (int iphi = 0; iphi < 2 * INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; ++iphi) {
            float phi = (float(iphi) + 0.5) * dphi;
            float dw = dtheta * dphi * sin(theta);
            vec3 w = vec3(cos(phi) * sin(theta), sin(phi) * sin(theta), ctheta);

            float nu1 = dot(s, w);
            float nu2 = dot(v, w);
            float pr2 = phaseFunctionR(nu2);
            float pm2 = phaseFunctionM(nu2);

            vec3 gnormal = (vec3(0.0, 0.0, r) + dground * w) / Rg;
            vec3 girradiance = irradiance(deltaETexture, Rg, dot(gnormal, s));

            vec3 raymie1; 

            raymie1 = greflectance * girradiance * gtransp;

            if (first == 1.0) {
                float pr1 = phaseFunctionR(nu1);
                float pm1 = phaseFunctionM(nu1);
                vec3 ray1 = texture4D(deltaSRTexture, r, w.z, mu_s, nu1).rgb;
                vec3 mie1 = texture4D(deltaSMTexture, r, w.z, mu_s, nu1).rgb;
                raymie1 += ray1 * pr1 + mie1 * pm1;
            } else {
                raymie1 += texture4D(deltaSRTexture, r, w.z, mu_s, nu1).rgb;
            }

            raymie += raymie1 * (betaR * exp(-(r - Rg) / HR) * pr2 + betaMSca * exp(-(r - Rg) / HM) * pm2) * dw;
        }
    }
}

Fragment getFragment() {
    vec3 raymie;
    float mu, mu_s, nu;
    getMuMuSNu(r, dhdH, mu, mu_s, nu);
    inscatter(r, mu, mu_s, nu, raymie);
    
    renderTarget1 = vec4(raymie, 1.0);
    
    Fragment frag;
    frag.color = vec4(1.0);
    frag.depth = 1.0;
    
    return frag;
}
