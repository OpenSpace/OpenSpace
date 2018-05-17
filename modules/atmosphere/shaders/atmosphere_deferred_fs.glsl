/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#version __CONTEXT__

#include "floatoperations.glsl"

#include "hdr.glsl"
#include "atmosphere_common.glsl"

out vec4 renderTarget;
in vec3 interpolatedNDCPos;

uniform int nAaSamples;
uniform double msaaSamplePatter[48];
uniform int cullAtmosphere;

// The following uniforms are
// set into the current Renderer
// Background exposure hack
uniform float backgroundConstant;
uniform bool firstPaint;
uniform float atmExposure;

uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;
uniform sampler2DMS mainPositionTexture;
uniform sampler2DMS mainNormalTexture;
uniform sampler2DMS mainColorTexture;

uniform dmat4 dInverseModelTransformMatrix; 
uniform dmat4 dModelTransformMatrix;
uniform dmat4 dSGCTViewToWorldMatrix;
uniform dmat4 dSgctProjectionToModelTransformMatrix;

uniform dvec4 dCamPosObj;
uniform dvec3 sunDirectionObj;

/*******************************************************************************
 ***** ALL CALCULATIONS FOR ECLIPSE ARE IN METERS AND IN WORLD SPACE SYSTEM ****
 *******************************************************************************/
// JCC: Remove and use dictionary to 
// decides the number of shadows
const uint numberOfShadows = 1;

struct ShadowRenderingStruct {
        double xu, xp;
        double rs, rc;
        dvec3 sourceCasterVec;
        dvec3 casterPositionVec;
        bool isShadowing;
};

// Eclipse shadow data
// JCC: Remove and use dictionary to 
// decides the number of shadows
uniform ShadowRenderingStruct shadowDataArray[numberOfShadows];
uniform int shadows;
uniform bool hardShadows;

vec4 butterworthFunc(const float d, const float r, const float n) {
    return vec4(vec3(sqrt(r/(r + pow(d, 2*n)))), 1.0);    
}

vec4 calcShadow(const ShadowRenderingStruct shadowInfoArray[numberOfShadows], const dvec3 position,
                const bool ground) {
    if (shadowInfoArray[0].isShadowing) {
        dvec3 pc = shadowInfoArray[0].casterPositionVec - position;
        dvec3 sc_norm = shadowInfoArray[0].sourceCasterVec;
        dvec3 pc_proj = dot(pc, sc_norm) * sc_norm;
        dvec3 d = pc - pc_proj;
        
        float length_d = float(length(d));
        double length_pc_proj = length(pc_proj);
        
        float r_p_pi = float(shadowInfoArray[0].rc * (length_pc_proj + shadowInfoArray[0].xp) / shadowInfoArray[0].xp);
        float r_u_pi = float(shadowInfoArray[0].rc * (shadowInfoArray[0].xu - length_pc_proj) / shadowInfoArray[0].xu);
        
        if ( length_d < r_u_pi ) { // umbra            
            if (ground) {
              if (hardShadows) {
                  return vec4(0.2, 0.2, 0.2, 1.0);
              } else {
                  return butterworthFunc(length_d, r_u_pi, 4.0);
              }
            }
            else {
              if (hardShadows) {
                  return vec4(0.5, 0.5, 0.5, 1.0);
              } else {
                  return vec4(vec3(length_d/r_p_pi), 1.0);
              }
            }
        }
        else if ( length_d < r_p_pi ) {// penumbra
            if (hardShadows) {
                return vec4(0.5, 0.5, 0.5, 1.0); 
            } else {
                return vec4(vec3(length_d/r_p_pi), 1.0);
            }
        }
    }
     
    return vec4(1.0);
}

/*******************************************************************************
 ******* ALL CALCULATIONS FOR ATMOSPHERE ARE KM AND IN WORLD SPACE SYSTEM ******
 *******************************************************************************/

struct dRay {
    dvec4 origin;
    dvec4 direction;
};

/* Function to calculate the initial intersection of the eye (camera) ray
 * with the atmosphere.
 * In (all parameters in the same coordinate system and same units):
 * - planet position
 * - ray direction (normalized)
 * - eye position
 * - atmosphere radius
 * Out: true if an intersection happens, false otherwise
 * - inside: true if the ray origin is inside atmosphere, false otherwise
 * - offset: the initial intersection distance from eye position when 
 *           the eye is outside the atmosphere
 * - maxLength : the second intersection distance from eye position when the
 *               eye is outside the atmosphere or the initial (and only) 
 *               intersection of the ray with atmosphere when the eye position
 *               is inside atmosphere.
 */
bool dAtmosphereIntersection(const dvec3 planetPosition, const dRay ray, const double atmRadius,
                             out bool inside, out double offset, out double maxLength ) {
    dvec3  l  = planetPosition - ray.origin.xyz;
    double s  = dot(l, ray.direction.xyz);
    double l2 = dot(l, l);
    double r2 = atmRadius * atmRadius; // avoiding surface acne

    // Ray origin (eye position) is behind sphere
    if ((s < 0.0) && (l2 > r2)) {
        inside    = false;
        offset    = 0.0;
        maxLength = 0.0;
        return false;
    }

    double m2 = l2 - s*s;

    // Ray misses atmospere
    if (m2 > r2) {
        inside    = false;
        offset    = 0.0;
        maxLength = 0.0;
        return false;
    }

    // We already now the ray hits the atmosphere

    // If q = 0.0f, there is only one intersection
    double q = sqrt(r2 - m2);

    // If l2 < r2, the ray origin is inside the sphere
    if (l2 > r2) {
        inside    = false;
        offset    = s - q;
        maxLength = s + q;
    } else {
        inside    = true;
        offset    = 0.0;
        maxLength = s + q;
    }
    
    return true;
}

/*
 * Calculates Intersection Ray by walking through
 * all the graphic pipile transformations in the 
 * opposite direction.
 * Instead of passing through all the pipeline,
 * it starts at NDC from the interpolated
 * positions from the screen quad.
 * This method avoids matrices multiplications
 * wherever is possible.
 */
void dCalculateRayRenderableGlobe(in int mssaSample, out dRay ray, 
                                  out dvec4 planetPositionObjectCoords, 
                                  out dvec4 cameraPositionInObject) {
    // ======================================
    // ======= Avoiding Some Matrices =======

    // Compute positions and directions in object space.
    dvec2 samplePos  = dvec2(msaaSamplePatter[mssaSample],
                             msaaSamplePatter[mssaSample+1]);
    //dvec4 clipCoords = dvec4(interpolatedNDCPos.xy + samplePos, 0.0, 1.0);
    dvec4 clipCoords = dvec4(interpolatedNDCPos.xy, 0.0, 1.0);

    // Clip to Object Coords
    dvec4 objectCoords = dSgctProjectionToModelTransformMatrix * clipCoords;
    
    // Planet Position in Object Space
    // JCC: Applying the inverse of the model transformation on the object postion in World 
    // space results in imprecision. 
    planetPositionObjectCoords = dvec4(0.0, 0.0, 0.0, 1.0);

    // Camera Position in Object Space (in meters)
    cameraPositionInObject = dCamPosObj;  
    
    // ============================
    // ====== Building Ray ========
    // Ray in object space (in KM)
    ray.origin    = cameraPositionInObject * dvec4(0.001, 0.001, 0.001, 1.0);
    //ray.direction = dvec4(normalize(objectCoords.xyz - cameraPositionInObject.xyz), 0.0);
    ray.direction = dvec4(normalize((objectCoords.xyz * dvec3(0.001))- ray.origin.xyz), 0.0);
}

/* 
 * Calculates the light scattering in the view direction comming from other 
 * light rays scattered in the atmosphere.
 * Following the paper:  S[L]|x - T(x,xs) * S[L]|xs
 * The view direction here is the ray: x + tv, s is the sun direction,
 * r and mu the position and zenith cosine angle as in the paper.
 * Arguments:
 * x := camera position
 * t := ray displacement variable after calculating the intersection with the 
 * atmosphere. It is the distance from the camera to the last intersection with
 * the atmosphere. If the ray hits the ground, t is updated to the correct value
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := out of ||x|| inside atmosphere (or top of atmosphere)
 * mu := out of cosine of the zenith view angle
 * attenuation := out of transmittance T(x,x0). This will be used later when
 * calculating the reflectance R[L].
 */
vec3 inscatterRadiance(inout vec3 x, inout float t, inout float irradianceFactor,
                       const vec3 v, const vec3 s, out float r, out float mu,
                       out vec3 attenuation, const vec3 fragPosObj,
                       const double maxLength, const double pixelDepth,
                       const vec4 spaceColor, const float sunIntensity) {

    const float INTERPOLATION_EPS = 0.004f; // precision const from Brunetton

    vec3 radiance;
    
    r  = length(x);
    mu = dot(x, v) / r;

    float mu2           = mu * mu;
    float r2            = r * r;
    float Rt2           = Rt * Rt;
    float Rg2           = Rg * Rg;
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
      
    // After removing the initial path from camera pos to top of atmosphere (for an
    // observer in the space) we test if the light ray is hitting the atmosphere
    vec3  x0     = fragPosObj;
    float r0     = length(fragPosObj);
    float invr0  = 1.0/r0;
    float muSun0 = dot(fragPosObj, s) * invr0;
    //vec3  x0     = x + float(pixelDepth) * v;      
    float mu0    = dot(x0, v) * invr0;

    bool groundHit = false;
    if ((pixelDepth > INTERPOLATION_EPS) && (pixelDepth < maxLength)) {
        t = float(pixelDepth);  
        groundHit = true;
        
        // Transmittance from point r, direction mu, distance t
        // By Analytical calculation
        //attenuation = analyticTransmittance(r, mu, t);
        // JCC: change from analytical to LUT transmittance to avoid
        // acme on planet surface when looking from far away. (11/02/2017)
        attenuation = transmittance(r, mu, t); 
        
        // Here we use the idea of S[L](a->b) = S[L](b->a), and get the S[L](x0, v, s)
        // Then we calculate S[L] = S[L]|x - T(x, x0)*S[L]|x0        
        // The "infinite" ray hist something inside the atmosphere, so we need to remove
        // the unsused contribution to the final radiance.
        vec4 inscatterFromSurface = texture4D(inscatterTexture, r0, mu0, muSun0, nu);
        inscatterRadiance = max(inscatterRadiance - attenuation.rgbr * inscatterFromSurface, 0.0);

        // We set the irradianceFactor to 1.0 so the reflected irradiance will be considered
        // when calculating the reflected light on the ground.
        irradianceFactor = 1.0;
    } else {
        attenuation = analyticTransmittance(r, mu, t);
        //attenuation = transmittance(r, mu, t); 
    }

    // cos(PI-thetaH) = dist/r
    // cos(thetaH) = - dist/r
    // muHorizon = -sqrt(r^2-Rg^2)/r = -sqrt(1-(Rg/r)^2)
    float muHorizon = -sqrt(1.0f - (Rg2 / r2));

    // In order to avoid imprecision problems near horizon,
    // we interpolate between two points: above and below horizon
    if (abs(mu - muHorizon) < INTERPOLATION_EPS) {
        // We want an interpolation value close to 1/2, so the
        // contribution of each radiance value is almost the same
        // or it has a havey weight if from above or below horizon
        float interpolationValue = ((mu - muHorizon) + INTERPOLATION_EPS) / (2.0f * INTERPOLATION_EPS);

        float t2 = t * t;
              
        // Above Horizon
        mu  = muHorizon - INTERPOLATION_EPS;
        //r0  = sqrt(r * r + t * t + 2.0f * r * t * mu);
        // From cosine law where t = distance between x and x0
        // r0^2 = r^2 + t^2 - 2 * r * t * cos(PI-theta)
        r0  = sqrt(r2 + t2 + 2.0f * r * t * mu);
        float invr0 = 1.0/r0;
        // From the dot product: cos(theta0) = (x0 dot v)/(||ro||*||v||)
        // mu0 = ((x + t) dot v) / r0
        // mu0 = (x dot v + t dot v) / r0
        // mu0 = (r*mu + t) / r0
        mu0 = (r * mu + t) * invr0;
        vec4 inScatterAboveX  = texture4D(inscatterTexture, r, mu, muSun, nu);
        vec4 inScatterAboveXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu);
        // Attention for the attenuation.r value applied to the S_Mie
        vec4 inScatterAbove = max(inScatterAboveX - attenuation.rgbr * inScatterAboveXs, 0.0f);

        // Below Horizon
        mu  = muHorizon + INTERPOLATION_EPS;
        r0  = sqrt(r2 + t2 + 2.0f * r * t * mu);
        mu0 = (r * mu + t) * invr0;
        vec4 inScatterBelowX  = texture4D(inscatterTexture, r, mu, muSun, nu);
        vec4 inScatterBelowXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu);
        // Attention for the attenuation.r value applied to the S_Mie
        vec4 inScatterBelow = max(inScatterBelowX - attenuation.rgbr * inScatterBelowXs, 0.0);

        // Interpolate between above and below inScattering radiance
        inscatterRadiance = mix(inScatterAbove, inScatterBelow, interpolationValue);
    }      

    // The w component of inscatterRadiance has stored the Cm,r value (Cm = Sm[L0])
    // So, we must reintroduce the Mie inscatter by the proximity rule as described in the
    // paper by Bruneton and Neyret in "Angular precision" paragraph:
      
    // Hermite interpolation between two values
    // This step is done because imprecision problems happen when the Sun is slightly below
    // the horizon. When this happen, we avoid the Mie scattering contribution.
    inscatterRadiance.w *= smoothstep(0.0f, 0.02f, muSun);
    vec3 inscatterMie    = inscatterRadiance.rgb * inscatterRadiance.a / max(inscatterRadiance.r, 1e-4) *
      (betaRayleigh.r / betaRayleigh);
      
    radiance = max(inscatterRadiance.rgb * rayleighPhase + inscatterMie * miePhase, 0.0f);    
    
    // Finally we add the Lsun (all calculations are done with no Lsun so
    // we can change it on the fly with no precomputations)
    // return radiance * sunRadiance;
    vec3 finalScatteringRadiance = radiance * sunIntensity;
    if (groundHit) {
        return finalScatteringRadiance;
    } else {
        return ((r-Rg)/(Rt-Rg))*spaceColor.rgb * backgroundConstant + finalScatteringRadiance;
    }
    
}

/* 
 * Calculates the light reflected in the view direction comming from other 
 * light rays integrated over the hemispehre plus the direct light (L0) from Sun.
 * Following the paper: R[L]= R[L0]+R[L*] 
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
 * attenuationXtoX0 := transmittance T(x,x0)
 */
vec3 groundColor(const vec3 x, const float t, const vec3 v, const vec3 s, const float r,
                 const float mu, const vec3 attenuationXtoX0, const vec4 groundColor, 
                 const vec3 normal, const float irradianceFactor,
                 const float waterReflectance, const float sunIntensity)
{
    vec3 reflectedRadiance = vec3(0.0f);

    // First we obtain the ray's end point on the surface
    vec3  x0                 = x + t * v;
    float r0                 = length(x0);
    // Normal of intersection point.
    // Normal must be normalized.
    vec3  n                  = normal;
    //vec4 groundReflectance = groundColor * vec4(.37);
    vec4 groundReflectance   = groundColor * 
        vec4(groundRadianceEmittion, groundRadianceEmittion, groundRadianceEmittion, 1.0f);

    // L0 is not included in the irradiance texture.
    // We first calculate the light attenuation from the top of the atmosphere
    // to x0.
    float dotNS = dot(n, s);
    float muSun = max(dotNS, 0.0f);

    // Is direct Sun light arriving at x0? If not, there is no direct light from Sun (shadowed)
    vec3  transmittanceL0     = muSun < -sqrt(1.0f - ((Rg * Rg) / (r0 * r0))) ? 
                                vec3(0.0f) : transmittanceLUT(r0, muSun);
    // E[L*] at x0
    vec3  irradianceReflected = irradiance(irradianceTexture, r0, muSun) * irradianceFactor;

    // R[L0] + R[L*]
    // vec3 groundRadiance = (dotNS < -0.2f ? groundReflectance.rgb * 15 : groundReflectance.rgb) *
    //   (muSun * transmittanceL0 + irradianceReflected) * sunIntensity / M_PI;

    vec3 groundRadiance;
    vec3 RLStar = (muSun * transmittanceL0 + irradianceReflected) * sunIntensity / M_PI;
    if (dotNS < 0.05f) {
        groundRadiance = groundReflectance.rgb * mix(30.0f, 1.0f, smoothstep(-1.0f, 0.05f, dotNS)) * RLStar;
    } else {
        groundRadiance = groundReflectance.rgb * RLStar;
    }

    //groundRadiance = groundReflectance.rgb * RLStar;

    // Specular reflection from sun on oceans and rivers  
    if ((waterReflectance > 0.1) && /*(dotNS > -0.2f)*/(muSun > 0.0)) {
        vec3  h         = normalize(s - v);
        // Fresnell Schlick's approximation
        float fresnel   = 0.02f + 0.98f * pow(1.0f - dot(-v, h), 5.0f);
        // Walter BRDF approximation
        float waterBrdf = fresnel * pow(max(dot(h, n), 0.0f), 150.0f);
        // Adding Fresnell and Water BRDFs approximation to the final surface color
        // (After adding the sunRadiance and the attenuation of the Sun through atmosphere)
        groundRadiance += waterReflectance * max(waterBrdf, 0.0) * transmittanceL0 * sunIntensity;
    }
    //return groundRadiance;  
    // Finally, we attenuate the surface Radiance from the the point x0 to the camera location.
    reflectedRadiance = attenuationXtoX0 * groundRadiance;    
      
    // Returns reflectedRadiance = 0.0 if the ray doesn't hit the ground.
    return reflectedRadiance;  
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
              const float mu, const float irradianceFactor) {
    vec3 transmittance  = (r <= Rt) ? ( mu < -sqrt(1.0f - (Rg*Rg)/(r*r)) ? 
                          vec3(0.0f) : transmittanceLUT(r, mu)) : vec3(1.0f);  
    // JCC: Change this function to a impostor texture with gaussian decay color weighted
    // by tge sunRadiance, transmittance and irradianceColor (11/03/2017)                          
    float sunFinalColor = step(cos(M_PI / 650.0), dot(v, s)) * sunRadiance * (1.0 - irradianceFactor); 

    return transmittance * sunFinalColor;      
}

void main() {
    ivec2 fragCoords = ivec2(gl_FragCoord);

    if (cullAtmosphere == 0) {
        vec4 atmosphereFinalColor = vec4(0.0f);
        int nSamples = 1;
        // First we determine if the pixel is complex (different fragments on it)
        bool complex = false;
        vec4 oldColor, currentColor;
        //vec4 colorArray[16];
        //int colorIndexArray[16];

        oldColor = texelFetch(mainColorTexture, fragCoords, 0);        
        //colorArray[0] = oldColor;
        //colorIndexArray[0] = 0;
        for (int i = 1; i < nAaSamples; i++) {
            //vec4 normal = texelFetch(mainNormalTexture, fragCoords, i);
            vec4 currentColor  = texelFetch(mainColorTexture, fragCoords, i);
            //colorArray[i] = currentColor;
            if (currentColor != oldColor) {
                complex = true;
                //nSamples = nAaSamples;
                nSamples = nAaSamples > 1 ? nAaSamples / 2 : nAaSamples;
                break;
                // for (int c = 0; c < nAaSamples; c++) {
                //     if (currentColor == colorArray[c]) {
                //         colorIndexArray[i] = c;
                //         break;
                //     }
                // }                
            } 
            //else {
            //     for (int c = 0; c < nAaSamples; c++) {
            //         if (currentColor == colorArray[c]) {
            //             colorIndexArray[i] = c;
            //             break;
            //         }
            //     }
            // }            
            oldColor = currentColor;
        }
        
        for (int i = 0; i < nSamples; i++) {
            // Color from G-Buffer
            vec4 color = texelFetch(mainColorTexture, fragCoords, i);
            
            // Ray in object space
            dRay ray;
            dvec4 planetPositionObjectCoords = dvec4(0.0);
            dvec4 cameraPositionInObject     = dvec4(0.0);        
            
            // Get the ray from camera to atm in object space
            dCalculateRayRenderableGlobe(i * 3, ray, planetPositionObjectCoords, 
                                         cameraPositionInObject);
          
            bool  insideATM    = false;
            double offset      = 0.0;   // in Km
            double maxLength   = 0.0;   // in Km  

            bool  intersectATM = false;

            intersectATM = dAtmosphereIntersection(planetPositionObjectCoords.xyz, ray,  
                                                  Rt - (ATM_EPSILON * 0.001), insideATM, offset, maxLength );
               
            if ( intersectATM ) {
                // Now we check is if the atmosphere is occluded, i.e., if the distance to the pixel 
                // in the depth buffer is less than the distance to the atmosphere then the atmosphere
                // is occluded
                // Fragments positions into G-Buffer are written in SGCT Eye Space (View plus Camera Rig Coords)
                // when using their positions later, one must convert them to the planet's coords
                
                // Get data from G-Buffer
                vec4 normal   = texelFetch(mainNormalTexture, fragCoords, i);
                // Data in the mainPositionTexture are written in view space (view plus camera rig)
                vec4 position = texelFetch(mainPositionTexture, fragCoords, i);

                // OS Eye to World coords                
                dvec4 positionWorldCoords = dSGCTViewToWorldMatrix * position;

                // World to Object (Normal and Position in meters)
                dvec4 positionObjectsCoords = dInverseModelTransformMatrix * positionWorldCoords;

                
                // Distance of the pixel in the gBuffer to the observer
                // JCC (12/12/2017): AMD distance function is buggy.
                //double pixelDepth = distance(cameraPositionInObject.xyz, positionObjectsCoords.xyz);
                double pixelDepth = length(cameraPositionInObject.xyz - positionObjectsCoords.xyz);
                
                // JCC (12/13/2017): Trick to remove floating error in texture.
                // We see a squared noise on planet's surface when seeing the planet
                // from far away.
                float dC = float(length(cameraPositionInObject.xyz));
                float x1 = 1e8;
                if (dC > x1) {
                    pixelDepth     += 1000.0;
                    float alpha     = 1000.0;
                    float beta      = 1000000.0;
                    float x2        = 1e9; 
                    float diffGreek = beta - alpha;
                    float diffDist  = x2 - x1;
                    float varA      = diffGreek/diffDist;
                    float varB      = (alpha - varA * x1);
                    pixelDepth     += double(varA * dC + varB); 
                }

                // All calculations are done in Km:
                pixelDepth                *= 0.001;
                positionObjectsCoords.xyz *= 0.001;
                
                if (position.xyz != vec3(0.0) && (pixelDepth < offset)) {
                    atmosphereFinalColor += vec4(HDR(color.xyz * backgroundConstant, atmExposure), color.a);                      
                    //discard;
                } else {
                    // Following paper nomenclature      
                    double t = offset;                  
                    vec3 attenuation;     

                    // Moving observer from camera location to top atmosphere
                    // If the observer is already inside the atm, offset = 0.0
                    // and no changes at all.
                    vec3  x  = vec3(ray.origin.xyz + t*ray.direction.xyz);
                    float r  = 0.0f;//length(x);
                    vec3  v  = vec3(ray.direction.xyz);
                    float mu = 0.0f;//dot(x, v) / r;
                    vec3  s  = vec3(sunDirectionObj);
                    float tF = float(maxLength - t);

                    // Because we may move the camera origin to the top of atmosphere 
                    // we also need to adjust the pixelDepth for tdCalculateRayRenderableGlobehis offset so the
                    // next comparison with the planet's ground make sense:
                    pixelDepth -= offset;
                    
                    dvec4 onATMPos           = dModelTransformMatrix * dvec4(x * 1000.0, 1.0);
                    vec4 eclipseShadowATM    = calcShadow(shadowDataArray, onATMPos.xyz, false);            
                    vec4 eclipseShadowPlanet = calcShadow(shadowDataArray, positionWorldCoords.xyz, true);
                  
                    float sunIntensityInscatter = sunRadiance * eclipseShadowATM.x;
                    float sunIntensityGround    = sunRadiance * eclipseShadowPlanet.x;

                    float irradianceFactor = 0.0;

                    vec3 inscatterColor = inscatterRadiance(x, tF, irradianceFactor, v,
                                                            s, r, mu, attenuation, 
                                                            vec3(positionObjectsCoords.xyz),
                                                            maxLength, pixelDepth,
                                                            color, sunIntensityInscatter); 
                    vec3 groundColor    = groundColor(x, tF, v, s, r, mu, attenuation,
                                                      color, normal.xyz, irradianceFactor, 
                                                      normal.a, sunIntensityGround);
                    vec3 sunColor       = sunColor(x, tF, v, s, r, mu, irradianceFactor); 
                    
                    // Final Color of ATM plus terrain:
                    vec4 finalRadiance  = vec4(HDR(inscatterColor + groundColor + sunColor, atmExposure), 1.0);
                    
                    atmosphereFinalColor += finalRadiance;
                }
            } 
            else { // no intersection
                //discard;
                atmosphereFinalColor += vec4(HDR(color.xyz * backgroundConstant, atmExposure), color.a);
            }           
        }  

        renderTarget = atmosphereFinalColor / float(nSamples);        
    } 
    else { // culling
        if (firstPaint) {
            vec4 bColor = vec4(0.0f);
            for (int f = 0; f < nAaSamples; f++) {
                bColor += texelFetch(mainColorTexture, fragCoords, f);
            }
            bColor /= float(nAaSamples);
            renderTarget = vec4(HDR(bColor.xyz * backgroundConstant, atmExposure), bColor.a);
        } 
        else {
            discard;
        }
        //renderTarget = vec4(1.0, 0.0, 0.0, 1.0);
        
    }
}

