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

/*****************************************************************************************
 * Modified parts of the code (4D texture mechanism) from Eric Bruneton is used in the   *
 * following code.                                                                       * 
 ****************************************************************************************/

#version __CONTEXT__

#include "floatoperations.glsl"

#include "atmosphere_common.glsl"

out vec3 renderTarget;
in vec3 interpolatedNDCPos;
in vec2 texCoord;
in float zenithMult;

uniform float sunZenith;

uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;

/*
 * Calculates the light scattering in the view direction comming from
 * the top of atm.
 * Used for the CIE's curves extraction.
 */
vec3 inscatterRadianceCIE(inout vec3 x, inout float t, const vec3 v,
                          const vec3 s, const float sunIntensity) {
    vec3 radiance;

    float r  = length(x);
    float mu = dot(x, v) / r;

    float mu2           = mu * mu;
    float r2            = r * r;
    float nu            = dot(v, s);
    float muSun         = dot(x, s) / r;
    float rayleighPhase = rayleighPhaseFunction(nu);
    float miePhase      = miePhaseFunction(nu);

    // S[L](x,s,v)
    // I.e. the next line has the scattering light for the "infinite" ray passing
    // through the atmosphere. If this ray hits something inside the atmosphere,
    // we will subtract the attenuated scattering light from that path in the
    // current path.
    vec4 inscatterRadiance = max(texture4D(inscatterTexture, r, mu, muSun, nu), 0.0);

    // The w component of inscatterRadiance has stored the Cm,r value (Cm = Sm[L0])
    // So, we must reintroduce the Mie inscatter by the proximity rule as described in the
    // paper by Bruneton and Neyret in "Angular precision" paragraph:

    // Hermite interpolation between two values
    // This step is done because imprecision problems happen when the Sun is slightly below
    // the horizon. When this happens, we avoid the Mie scattering contribution.
    inscatterRadiance.w *= smoothstep(0.0f, 0.02f, muSun);
    vec3 inscatterMie    = inscatterRadiance.rgb * inscatterRadiance.a / max(inscatterRadiance.r, 1e-4) * 
                        (betaRayleigh.r / betaRayleigh);

    radiance = max(inscatterRadiance.rgb * rayleighPhase + inscatterMie * miePhase, 0.0f);

    // Finally we add the Lsun (all calculations are done with no Lsun so
    // we can change it on the fly with no precomputations)
    // return radiance * sunRadiance;
    vec3 finalScatteringRadiance = radiance * sunIntensity;

    return finalScatteringRadiance;
}

/* 
 * Calculates the Sun color.
 * The the ray is x + tv, v the view direction, s is the sun direction,
 * r and mu the position and zenith cosine angle as in the paper.
 * As for all calculations in the atmosphere, the center of the coordinate system
 * is the planet's center of coordiante system, i.e., the planet's position is (0,0,0).
 * Arguments:
 * x := camera position
 * t := ray displacement variable. Here, differently from the inScatter light calculation,
 * the position of the camera is already offset (on top of atmosphere) or inside 
 * the atmosphere.
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := ||x|| inside atmosphere (or top of atmosphere). r <= Rt here.
 * mu := cosine of the zenith view angle
 * attenuation := transmittance T(x,x0)
 */
vec3 sunColor(const vec3 x, const float t, const vec3 v, const vec3 s, const float r,
              const float mu) {
    vec3 transmittance  = (r <= Rt) ? ( mu < -sqrt(1.0f - Rg2/(r*r)) ? 
                          vec3(0.0f) : transmittanceLUT(r, mu)) : vec3(1.0f);  
    // JCC: Change this function to a impostor texture with gaussian decay color weighted
    // by tge sunRadiance, transmittance and irradianceColor (11/03/2017)                          
    float sunFinalColor = smoothstep(cos(M_PI / 500.0f), cos(M_PI / 900.0f), dot(v, s)) * 
                          sunRadiance;

    return transmittance * sunFinalColor;
}

/* lambda in micrometers (mumeter = 1E-6 meters) 
 * This is an approximation for the photopic spectral luminous efficiency curve.
 */
float photonicResponse(const float lambda) {
    return 1.019 * exp(-285.4 * (lambda - 0.559) * (lambda - 0.559));
}

/* Lambda is the wavelength of the emmiting light */
float radianceToLuminance(const float radianceValue, const float lambda) {
    return radianceValue * 683.0 * photonicResponse(lambda);
}

void main() {
    // Following paper nomenclature and CIE definition.
    // The observer is on ground, inside the ATM and looking
    // at different directions (v), while the Sun is defined
    // by its position (angle in zenith).
    // For each iteraction, only the Sun position changes.

    vec3  x  = vec3(Rg, 0.0f, 0.0f);
    float z0 = zenithMult * (3.1415926535897932384626433832795f / 2.0f);
    float zz = zenithMult * acos(Rg/Rt);
    vec3  k  = vec3(Rt*cos(abs(zz)) - Rg, Rt*sin(zz), 0);
    float t  = length(k);
    vec3  v  = normalize(vec3(t*cos(abs(z0)), t*sin(z0), 0.0));
    vec3  s  = normalize(vec3(Rt * cos(sunZenith), Rt * sin(sunZenith), 0.0));
  
    float tZenith = (Rt - Rg);
    vec3 vZenith = vec3(1.0f, 0.0f, 0.0f);
    vec3 inscatterColorZenith = inscatterRadianceCIE(x, tZenith, vZenith, s, sunRadiance);
    vec3 inscatterColor       = inscatterRadianceCIE(x, t, v, s, sunRadiance);
    
    vec3 zenithLuminance = vec3(
        radianceToLuminance(inscatterColorZenith.r, 0.680),
        radianceToLuminance(inscatterColorZenith.g, 0.550),
        radianceToLuminance(inscatterColorZenith.b, 0.440)
    );

    //zenithLuminance = vec3(114974.916437,71305.954816,65310.548555) * inscatterColorZenith;

    vec3 elementLuminance = vec3(
        radianceToLuminance(inscatterColor.r, 0.680),
        radianceToLuminance(inscatterColor.g, 0.550),
        radianceToLuminance(inscatterColor.b, 0.440)
    );

    //elementLuminance = inscatterColor * vec3(114974.916437,71305.954816,65310.548555);

    //float mu = dot(x, v) / r;
    //vec3 sunColorV = sunColor(x, tF, v, s, r, mu);                                                
    
    // Final Color of ATM plus terrain:
    //vec4 finalRadiance = vec4(inscatterColor + sunColorV, 1.0);
    
    renderTarget = inscatterColor / inscatterColorZenith;
    //renderTarget = inscatterColorZenith;
    //renderTarget = inscatterColor;
    
    //renderTarget = elementLuminance / zenithLuminance;
    //renderTarget = elementLuminance;
}

