/*****************************************************************************************
 *                                                                                       *
 * Joshua Carter (c) 2026                                                                *
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

// Equations taken from:
// https://elib.dlr.de/203152/1/Bachelorarbeit_Alexander_Kuzminykh_20210818.pdf

#ifndef HAPKE_GLSL
#define HAPKE_GLSL

#define HAPKE_PI  3.14159265359
#define HAPKE_EPS 1e-4

struct HapkeParameters {
    float w;      // single-scattering albedo
    float theta;  // macroscopic roughness in rad
    float c;      // backscatter fraction of the double Henyey-Greenstein phase function
    float b;      // anisotropy of the double Henyey-Greenstein phase function
    float B_S0;   // shadow hiding opposition amplitude
    float B_C0;   // coherent backscatter opposition amplitude
    float h_S;    // shadow hiding width
    float h_C;    // coherent backscatter width
    float phi;    // porosity / filling factor
};

uniform HapkeParameters hapke;
uniform bool hapkeNormalize;
uniform float hapkeExposure;

struct HapkeDerived {
    float tanTheta;
    float chi;   // 3.11
    float K;     // 3.22
    float r0;    // 3.24
};

HapkeDerived hapkeDerive(HapkeParameters p) {
    HapkeDerived d;
    d.tanTheta = max(tan(p.theta), HAPKE_EPS);
    d.chi = inversesqrt(1.0 + HAPKE_PI * d.tanTheta * d.tanTheta);

    float x = 1.209 * pow(max(p.phi, 0.0), 2.0 / 3.0);
    if (x == 0) {
        d.K = 1.0;
    } else {
        d.K = -log(1.0 - x) / x;
    }

    float gam = sqrt(max(1.0 - p.w, 0.0));
    d.r0 = (1.0 - gam) / (1.0 + gam);
    return d;
}

// 3.23, 3.24
float hapkeH(float x, float w, float r0) {
    x = max(x, HAPKE_EPS);
    return 1.0 / (1.0 - w * x * (r0 + 0.5 * (1.0 - 2.0 * x * r0) * log((1.0 + x) / x)));
}

float hapkeBRDFTimesCosI(vec3 L, vec3 N, vec3 V, HapkeParameters p, HapkeDerived d)
{
    float cos_i = min(dot(L, N), 1.0);
    if (cos_i <= 0.0) return 0.0;
    
    float cos_e = clamp(dot(V, N), 0.0, 1.0);
    float cos_g = clamp(dot(L, V), -1.0, 1.0);

    float sin_i = sqrt(max(1.0 - cos_i * cos_i, 0.0));
    float sin_e = sqrt(max(1.0 - cos_e * cos_e, 0.0));

    // 3.6
    float sinProd = sin_i * sin_e;
    float cos_psi = (sinProd > 1e-6) ? clamp((cos_g - cos_i * cos_e) / sinProd, -1.0, 1.0) : 1.0;
    float psi = acos(cos_psi);
    float s2 = 0.5 * (1.0 - cos_psi);                                          // sin^2(psi/2)
    float tanHalfPsi = sqrt((1.0 - cos_psi) / max(1.0 + cos_psi, HAPKE_EPS)); // tan(psi/2)

    // 3.13, 3.14
    float k = 1.0 / (HAPKE_PI * d.tanTheta);
    float cot_i = cos_i / max(sin_i, HAPKE_EPS);
    float cot_e = cos_e / max(sin_e, HAPKE_EPS);
    float E1i = exp(-2.0 * cot_i * k);
    float E2i = exp(-cot_i * cot_i * k / d.tanTheta);
    float E1e = exp(-2.0 * cot_e * k);
    float E2e = exp(-cot_e * cot_e * k / d.tanTheta);

    // 3.8
    float eta_i = d.chi * (cos_i + sin_i * d.tanTheta * E2i / (2.0 - E1i));
    float eta_e = d.chi * (cos_e + sin_e * d.tanTheta * E2e / (2.0 - E1e));

    bool iLEe = cos_i >= cos_e;
    float mu_0e, mu_e;
    if (iLEe) {
        float den = 2.0 - E1e - (psi / HAPKE_PI) * E1i;
        mu_0e = d.chi * (cos_i + sin_i * d.tanTheta * (cos_psi * E2e + s2 * E2i) / den);
        mu_e  = d.chi * (cos_e + sin_e * d.tanTheta * (E2e - s2 * E2i) / den);
    } else {
        float den = 2.0 - E1i - (psi / HAPKE_PI) * E1e;
        mu_0e = d.chi * (cos_i + sin_i * d.tanTheta * (E2i - s2 * E2e) / den);
        mu_e  = d.chi * (cos_e + sin_e * d.tanTheta * (cos_psi * E2i + s2 * E2e) / den);
    }

    // 3.25
    float LS = mu_0e / (mu_0e + mu_e);

    // 3.18
    float b  = clamp(p.b, -0.999, 0.999);
    float b2 = b * b;
    float t  = 2.0 * b * cos_g;
    float d1 = 1.0 + b2 - t;
    float d2 = 1.0 + b2 + t;
    float HGv = (1.0 - b2) * (0.5 * (1.0 + p.c) / (d1 * sqrt(d1))
                            + 0.5 * (1.0 - p.c) / (d2 * sqrt(d2)));
    float tanHalfG = sqrt((1.0 - cos_g) / max(1.0 + cos_g, HAPKE_EPS));

    // 3.19 
    float BS = 1.0 / (1.0 + tanHalfG / max(p.h_S, HAPKE_EPS));
    // 3.21
    float M = hapkeH(mu_0e / d.K, p.w, d.r0) * hapkeH(mu_e / d.K, p.w, d.r0) - 1.0;

    // 3.20
    float CBOE = 1.0;
    if (p.B_C0 > 0.0) {
        float x = tanHalfG / max(p.h_C, HAPKE_EPS);
        float q = (x < 1e-3) ? 1.0 - 0.5 * x : (1.0 - exp(-x)) / x;
        float BC = (1.0 + q) / (2.0 * (1.0 + x) * (1.0 + x));
        CBOE = 1.0 + p.B_C0 * BC;
    }

    // 3.10, 3.15
    float fpsi = exp(-2.0 * tanHalfPsi);
    float var  = iLEe ? cos_i / eta_i : cos_e / eta_e;
    float S    = (mu_e * cos_i * d.chi) / (eta_e * eta_i * (1.0 - fpsi + fpsi * d.chi * var));

    // 3.26, S has a cos_i term which cancels.
    return LS * d.K * p.w / (4.0 * HAPKE_PI) * (HGv * (1.0 + p.B_S0 * BS) + M) * CBOE * S;
}

float hapkeReflectance(vec3 L, vec3 N, vec3 V, HapkeParameters p, HapkeDerived d) {
    return HAPKE_PI * hapkeBRDFTimesCosI(L, N, V, p, d);
}

vec3 toneMap(vec3 color) {
    // Rec.709 -> ACEScg approximation
    const mat3 inputMatrix = mat3(
        0.84247906224151, 0.04232824226101, 0.04237565490570,
        0.07781254037158, 0.87843363533593, 0.07843363533593,
        0.07970839738700, 0.07923812240305, 0.87919070975837
    );
    color = inputMatrix * color;
    color = log2(clamp(color, 1.0 / 256.0, 256.0)) / 16.0 + 0.5;

    // Polynomial approximation for ACES S-curve.
    vec3 c2 = color * color;
    vec3 c3 = c2 * color;
    vec3 c4 = c3 * color;
    vec3 c5 = c4 * color;
    vec3 result = 15.53 * c5 - 40.07 * c4 + 31.96 * c3 - 6.87 * c2 + 0.45 * color;

    // ACEScg → Rec.709 conversion
    const mat3 outputMatrix = mat3(
        1.1961604724, -0.0528489811, -0.0528489811,
        -0.0984852928, 1.1528414546, -0.0984852928,
        -0.0976751805, -0.0999924735, 1.1513342746
    );

    return clamp(outputMatrix * result, 0.0, 1.0);
}

#endif // HAPKE_GLSL