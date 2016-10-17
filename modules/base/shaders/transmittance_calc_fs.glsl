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

// In the following shaders r (altitude) is the length of vector/position x in the
// atmosphere (or on the top of it when considering an observer in space),
// where the light is comming from the opposity direction of the view direction,
// here the vector v or viewDirection.
// Rg is the planet radius

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

float opticalDepth(const float r, const float mu, const float scaleHeight) {
    
    float r2 = r*r;
    // Is ray below horizon?
    // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
    float cosZenithHorizon = -sqrt(1.0 - ((Rg*Rg)/r2));
    if (mu < cosZenithHorizon)
        return 1e9;
    
    // Integrating using the Trapezoidal rule:
    // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
    float b_a = rayDistance(r, mu);
    float deltaStep = b_a / float(TRANSMITTANCE_STEPS);
    // cosine law
    float y_i = exp(-(r - Rg) / scaleHeight);
    
    float x_step    = 0.0;
    float accumulation = 0.0;
    for (int i = 1; i <= TRANSMITTANCE_STEPS; ++i) {
        float x_i = float(i) * deltaStep;
        // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
        // In this case, a = r, b = x_i and cos(alpha) = cos(PI-zenithView) = mu
        float y_ii = exp(-(sqrt(r2 + x_i * x_i + 2.0 * x_i * r * mu) - Rg) / scaleHeight);
        accumulation += (y_ii + y_i);
        //x_step = x_i;
        y_i = y_ii;
    }
    return accumulation * (b_a/(2*TRANSMITTANCE_STEPS));
}

void getRandMU(out float r, out float mu) {
    float u_mu  = gl_FragCoord.x / float(TRANSMITTANCE_W);
    float u_r   = gl_FragCoord.y / float(TRANSMITTANCE_H);
    
    // In the paper u_r^2 = (r^2-Rg^2)/(Rt^2-Rg^2)
    r   = sqrt(Rg*Rg + (u_r * u_r) * (Rt*Rt - Rg*Rg));
    
    // In the paper the author suggest mu = dot(v,x)/||x|| with ||v|| = 1.0
    // Later he proposes u_mu = (1-exp(-3mu-0.6))/(1-exp(-3.6))
    // But the below one is better. See Colliene. 
    mu = -0.15 + tan(1.5 * u_mu) / tan(1.5) * (1.0 + 0.15);
}

void main(void) {
    float r, mu;
    
    getRandMU(r, mu);
    vec3 depth = betaMEx * opticalDepth(r, mu, HM) + betaR * opticalDepth(r, mu, HR);
    
    renderTableColor = vec4(exp(-depth), 1.0); 
}
