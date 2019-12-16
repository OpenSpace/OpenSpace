/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include "floatoperations.glsl"
#include "PowerScaling/powerScalingMath.hglsl"

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in vec4 vs_bvLumAbsMagAppMag[];
in vec3 vs_velocity[];
in vec4 vs_gPosition[];
in float vs_speed[];

out vec4 vs_position;
out vec2 psfCoords;

flat out vec4 ge_bvLumAbsMagAppMag;
flat out vec3 ge_velocity;
flat out float ge_speed;
flat out float ge_observationDistance;
flat out float gs_screenSpaceDepth;

uniform float magnitudeExponent;
uniform dvec3 eyePosition;
uniform dvec3 cameraUp;

// uniform float FWHM;
// uniform float betaConstant;

uniform int psfParamConf;
uniform float lumCent;
uniform float radiusCent;
uniform float brightnessCent;

uniform dmat4 cameraViewProjectionMatrix;
uniform dmat4 modelMatrix;

const double PARSEC = 3.08567756E16;
//const double PARSEC = 3.08567782E16;

const vec2 corners[4] = vec2[4](
    vec2(0.0, 0.0),
    vec2(1.0, 0.0),
    vec2(1.0, 1.0),
    vec2(0.0, 1.0)
);

const float SunTemperature = 5800.0f;
const float SunAbsMagnitude = 4.83f;
const float SunRadius = 6.957E8; // meters

float bvToKelvin(float bv) {
    float tmp = 0.92f * bv;
    return 4600.f * (1.f/(tmp+1.7f) +1.f/(tmp+0.62f));
}

void main() {
    vs_position = gl_in[0].gl_Position; // in object space
    dvec4 dpos  = modelMatrix * dvec4(vs_position); 

    ge_bvLumAbsMagAppMag = vs_bvLumAbsMagAppMag[0];
    ge_velocity    = vs_velocity[0];
    ge_speed       = vs_speed[0];
        
    double scaleMultiply = 1.0;

    if (psfParamConf == 0) {
        // Working like Partiview
        double luminosity          = double(ge_bvLumAbsMagAppMag.y) * 10.0;
        double pSize               = pow(10, magnitudeExponent + 29.0);
        double distanceToStar      = length((dpos.xyz - eyePosition));  
        double apparentBrightness  = pSize * luminosity / (distanceToStar);
        scaleMultiply              = apparentBrightness;
    }
    else if (psfParamConf == 1) {
        float L_over_Lsun = pow(2.51f, SunAbsMagnitude - ge_bvLumAbsMagAppMag.z);
        float starTemperature = bvToKelvin(ge_bvLumAbsMagAppMag.x);
        float starRadius = SunRadius * pow(SunTemperature/starTemperature, 2.f) * sqrt(L_over_Lsun);
        scaleMultiply = ((lumCent * (ge_bvLumAbsMagAppMag.y + 5E9)) + 
                        (radiusCent * double(starRadius))) * pow(10.0, magnitudeExponent);
    }
    else if (psfParamConf == 2) {
        double luminosity              = double(1.0 - ge_bvLumAbsMagAppMag.y);
        double distanceToStarInParsecs = trunc(length(dpos.xyz - eyePosition) / PARSEC);
        double apparentBrightness      = luminosity / distanceToStarInParsecs;
        float L_over_Lsun              = pow(2.51f, SunAbsMagnitude - ge_bvLumAbsMagAppMag.z);
        float starTemperature          = bvToKelvin(ge_bvLumAbsMagAppMag.x);
        float starRadius               = SunRadius * pow(SunTemperature/starTemperature, 2.f) * 
                                         sqrt(L_over_Lsun);
        scaleMultiply                  = ((lumCent * (ge_bvLumAbsMagAppMag.y + 5E9)) + 
                                         (radiusCent * double(starRadius)) + 
                                         (brightnessCent * apparentBrightness * 5E15)) * 
                                         pow(10.0, magnitudeExponent);
    }
    else if (psfParamConf == 3) {
        float absMag = ge_bvLumAbsMagAppMag.z;
        scaleMultiply = (-absMag + 35.f) * pow(10.0, magnitudeExponent + 8.5f);
    }
    else if (psfParamConf == 4) {
        float absMag = vs_bvLumAbsMagAppMag[0].z;
        double distanceToStarInMeters = length(dpos.xyz - eyePosition);
        double distanceToCenterInMeters = length(eyePosition);
        float distanceToStarInParsecs = float(distanceToStarInMeters/PARSEC); 
        //float appMag = absMag + 5*log(distanceToStarInParsecs) - 5.0;
        float appMag = absMag + 5.0 * (log(distanceToStarInParsecs/10.0)/log(2.0));
        //appMag = vs_bvLumAbsMagAppMag[0].w;
        
        //scaleMultiply = (30.623 - appMag) * pow(10.0, magnitudeExponent + 7.0);// * 
        //float(distanceToStarInMeters/distanceToCenterInMeters);
        
        //scaleMultiply = (-appMag + 50.f) * pow(10.0, magnitudeExponent + 7.5f);
        //scaleMultiply =  log(35.f + appMag) *  pow(10.0, magnitudeExponent + 6.5f);
        //scaleMultiply = exp((35.f - appMag) * 0.2) * pow(10.0, magnitudeExponent + 2.5f);
        //scaleMultiply = appMag * pow(10.0, magnitudeExponent + 8.5f);
        scaleMultiply = exp((-30.0 - appMag) * 0.45) * pow(10.0, magnitudeExponent + 8.f);
        //scaleMultiply = pow(10.0, (appMag - absMag)*(1.0/5.0) + 1.0) * pow(10.0, magnitudeExponent + 3.f);
    }
    else if (psfParamConf == 5) {
        float absMag = ge_bvLumAbsMagAppMag.z;
        scaleMultiply = exp((-30.623 - absMag) * 0.462) * pow(10.0, magnitudeExponent + 12.5f) * 2000;
    }

    dvec3 scaledRight    = dvec3(0.0);
    dvec3 scaledUp       = dvec3(0.0);
    vec4 bottomLeftVertex, bottomRightVertex, topLeftVertex, topRightVertex;
  
    dvec3 normal   = normalize(eyePosition - dpos.xyz);
    dvec3 newRight = normalize(cross(cameraUp, normal));
    dvec3 newUp    = cross(normal, newRight);
    scaledRight    = scaleMultiply * newRight;
    scaledUp       = scaleMultiply * newUp;
    
    bottomLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
    gs_screenSpaceDepth  = bottomLeftVertex.w;
    
    topRightVertex = z_normalization(vec4(cameraViewProjectionMatrix *
                        dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));        

    bottomRightVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
    
    topLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));
    
    // Build primitive
    
    gl_Position = bottomLeftVertex;
    psfCoords   = vec2(-1.0, -1.0);
    EmitVertex();
    
    gl_Position = bottomRightVertex;
    psfCoords   = vec2(1.0, -1.0);
    EmitVertex();

    gl_Position = topLeftVertex;
    psfCoords   = vec2(-1.0, 1.0);
    EmitVertex();
    
    gl_Position = topRightVertex;
    psfCoords   = vec2(1.0, 1.0);
    EmitVertex();
    
    EndPrimitive();

}
