/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include "PowerScaling/powerScalingMath.hglsl"
// General Uniforms that's always needed
uniform mat4      modelViewProjection;

// Uniforms needed to color by quantity
uniform int       colorMode;
uniform sampler1D colorTable;
uniform sampler1D colorTableCMR;
uniform sampler1D colorTableEarth;
uniform sampler1D colorTableFlow;
uniform vec2      colorTableRange;

// Uniforms needed for Particle Flow
uniform vec4      flowColor;
uniform int       particleSize;
uniform int       particleSpeed;
uniform int       particleSpacing;
uniform bool      usingParticles;
uniform bool      flowColoring;

// Masking Uniforms
uniform bool      usingMasking;
uniform vec2      maskingRange;

// Domain Uniforms
uniform bool      usingDomain;
uniform vec2      domainLimX;
uniform vec2      domainLimY;
uniform vec2      domainLimZ;
uniform vec2      domainLimR;

// Fluxnodes specific uniforms
uniform float   nodeSize;
uniform float   nodeSizeLargerFlux;
uniform vec4    streamColor;
uniform float   thresholdFlux;
uniform float   filterLower;
uniform float   filterUpper;
uniform int     scalingMode;
uniform int     nodeSkipMethod;
uniform int     nodeSkip;
uniform int     nodeSkipDefault;
uniform int     nodeSkipEarth;
uniform float   nodeSkipFluxThreshold;
uniform float   nodeSkipRadiusThreshold;
uniform float   fluxColorAlpha;
uniform float   fluxColorAlphaIlluminance;
uniform vec3    earthPos;
uniform float   distanceThreshold;
uniform int     enhanceMethod;
uniform double  time;

// Speicific uniforms for cameraperspective
uniform float maxNodeDistanceSize;
uniform float nodeDistanceThreshold;

uniform vec3    cameraPos;
//uniform vec2 screenSize;
uniform bool    usingCameraPerspective;
uniform bool    usingRadiusPerspective;
uniform float   perspectiveDistanceFactor;

uniform float   maxNodeSize;
uniform float   minNodeSize;
uniform bool    usingPulse;

// Inputs
// Should be provided in meters VaPosition
layout(location = 0) in vec3 in_position;

// The extra value used to color lines. Location must correspond to VaColor in 
// renderablefluxnodes.h
layout(location = 1) in float fluxValue;

// The extra value used to mask out parts of lines. Location must correspond to 
// VaFiltering in renderablefluxnodes.h
layout(location = 2) in float rValue;

// These should correspond to the enum 'ColorMode' in renderablefluxnodes.cpp
const int colorByFluxValue  = 0;
const int uniformColor     = 1;

const int uniformskip = 0;
const int fluxSkip = 1;
const int radiusSkip = 2;

const int fluxMode = 0;
const int RFlux = 1;
const int R2Flux = 2;
const int log10RFlux = 3;
const int lnRFlux = 4;

const int sizeScaling = 0;
const int colorTables = 1;
const int sizeAndColor = 2;
const int illuminance = 3;

const float AUtoMeter = 149597871000.0;
varying out vec4    vs_color;
varying out float   vs_depth;
varying out vec2    vs_st;
varying out float   camera_IsCloseEnough;
varying out float   vs_closeToEarth;
varying out double  vs_time;
varying out vec3    vs_camerapos;

vec4 getTransferFunctionColor(sampler1D InColorTable) {
    // Remap the color scalar to a [0,1] range
    float scaleValue = 0;
    if(scalingMode == fluxMode){
        scaleValue = fluxValue;
    }
    else if(scalingMode == RFlux){
       scaleValue = rValue * fluxValue;
    }
    else if(scalingMode == log10RFlux){
        //conversion from logbase e to log10 since glsl does not support log10. 
        float logtoTen = log(rValue) / log(10);
        scaleValue = logtoTen * fluxValue;
    }
    else if(scalingMode == lnRFlux){
        scaleValue = log(rValue) * fluxValue;
    }
    else if(scalingMode == R2Flux){
        scaleValue = rValue * rValue * fluxValue;
    }

    float lookUpVal = (scaleValue - colorTableRange.x)
        /(colorTableRange.y - colorTableRange.x);
    return texture(InColorTable, lookUpVal);
}

bool CheckvertexIndex(){
    int nodeIndex = gl_VertexID;

    if(nodeSkipMethod == uniformskip){
        if(mod(nodeIndex, nodeSkip) == 0){
            return true;
        }
    }
    else if(nodeSkipMethod == fluxSkip){
        if(fluxValue > nodeSkipFluxThreshold && mod(nodeIndex, nodeSkip) == 0){
            return true;
        }
        if(fluxValue < nodeSkipFluxThreshold && mod(nodeIndex, nodeSkipDefault) == 0){
            return true;
        }
    }
    else if(nodeSkipMethod == radiusSkip){
        if(rValue < nodeSkipRadiusThreshold && mod(nodeIndex, nodeSkip) == 0){
            return true;
        }
        if(rValue > nodeSkipRadiusThreshold && mod(nodeIndex, nodeSkipDefault) == 0){
            return true;
        }
    }
    return false;
}
//todo fix gl_VertexID

bool isParticle(){
    int modulusResult = int(double(particleSpeed) * time + gl_VertexID) % particleSpacing;
    return modulusResult > 0 && modulusResult <= particleSize;
return false;
}

//function for showing nodes different depending on distance to earth
void DecidehowtoshowClosetoEarth(){
        // SizeScaling
    if(enhanceMethod == sizeScaling || enhanceMethod == illuminance){
       vec4 fluxColor = getTransferFunctionColor(colorTableCMR);
        vs_color = vec4(fluxColor.xyz, fluxColor.a);
    }
    // ColorTables
    if(enhanceMethod == colorTables){
        vec4 fluxColor = getTransferFunctionColor(colorTable);
        vs_color = vec4(fluxColor.xyz, fluxColor.a);
    }
    // SizeColor
    if(enhanceMethod == sizeAndColor){
        vec4 fluxColor3 = getTransferFunctionColor(colorTable);
        vs_color = vec4(fluxColor3.xyz, fluxColor3.a);

        float tempR2 = rValue + 0.4; 
        if(tempR2 > 1.5){
            tempR2 = 1.5;
        }
        gl_PointSize = tempR2 * tempR2 * tempR2 * gl_PointSize * 5;
    }
}

void CheckdistanceMethod() { 
        //Enhance by distance to Earth
        float maxdist = 10000000000.f * perspectiveDistanceFactor;
        float distancevec = distance(earthPos, in_position.xyz);

        vs_closeToEarth = 0;

        if(distancevec < AUtoMeter * distanceThreshold && usingPulse){ 
            vs_closeToEarth = 1;
            gl_PointSize = gl_PointSize * 5;
            vec4 fluxColor = getTransferFunctionColor(colorTable);
            vs_color = vec4(fluxColor.xyz, fluxColorAlpha);
        }
        
        if(enhanceMethod == colorTables || enhanceMethod == sizeAndColor){
             vec4 fluxColor2 = getTransferFunctionColor(colorTableEarth);
             vs_color = vec4(fluxColor2.xyz, fluxColorAlpha);
             //vs_color = vec4(0.3, 0.3, 0.3, fluxColorAlpha);
        }
         if(enhanceMethod == illuminance){
             vec4 fluxColor = getTransferFunctionColor(colorTableCMR);
             vs_color = vec4(fluxColor.xyz, fluxColorAlpha);
        }
        if(distance(earthPos, in_position) < AUtoMeter * distanceThreshold){
            if(mod(gl_VertexID, nodeSkipEarth) == 0){
                DecidehowtoshowClosetoEarth();
            }
            else{
            vs_color = vec4(0);
            }
        }
        else{
            if(enhanceMethod == illuminance){
                vs_color.a = fluxColorAlphaIlluminance;
            }

            if(fluxValue < thresholdFlux){
                vs_color.a = fluxColorAlpha;  
            }
                
        }
}

void main() {
    // Default gl_PointSize if it is not set anywhere else.
    gl_PointSize = 2;
    // Checking if we should render the vertex dependent on the vertexindex, 
    // by using modulus.
    
    if(CheckvertexIndex() || 
        distance(earthPos, in_position) < (distanceThreshold * AUtoMeter) &&
        rValue/AUtoMeter > filterLower && 
        rValue/AUtoMeter < filterUpper &&
        in_position.z > (domainLimZ.x * AUtoMeter) &&
        in_position.z < (domainLimZ.y * AUtoMeter)){
    
        // We should color it by flux
        if(colorMode == 0){
            //vec4 fluxColor = getTransferFunctionColor(colorTable);
            vec4 fluxColor = getTransferFunctionColor(colorTableCMR);
            if(fluxValue > thresholdFlux){
                vs_color = vec4(fluxColor.xyz, fluxColor.a);  
                gl_PointSize = nodeSizeLargerFlux;
            }
            else{
                vs_color = vec4(fluxColor.xyz, fluxColorAlpha);
                gl_PointSize = nodeSize;
            }
        }
        else{
            //Uniform coloring
            vs_color = streamColor;
        }
        CheckdistanceMethod();
    }
    else{
        vs_color = vec4(0);
    }

    if(usingParticles && isParticle() && rValue > 0.f){
        int modulusResult = int(double(particleSpeed) * time + gl_VertexID) 
        % particleSpacing;

        if(modulusResult >= particleSize - 30){
            if(flowColoring){
                vec4 fluxColor3 = getTransferFunctionColor(colorTable);
                vs_color = vec4(fluxColor3.xyz, flowColor.a * 0.8);
            }
            else{
                vs_color = vec4(0.9,0.9,0.9,0.5);
            }
        }
        else{
            vec4 fluxColorFlow = getTransferFunctionColor(colorTableFlow);
            vs_color = vec4(fluxColorFlow.xyz, 1);
        }
    }

    if(usingCameraPerspective){
        float rtemp = 1.0;
        if(rValue > 1.0){
            rtemp = 1.0;
         }
         else{
            rtemp = rValue;
         }
    
        float maxDistance = 100000000000.f * perspectiveDistanceFactor;
        float distanceVec = distance(cameraPos, in_position.xyz);

        if(distanceVec < maxDistance){
            camera_IsCloseEnough = 0;
        }
        else{
            camera_IsCloseEnough = 1;
        }

        if(distanceVec < maxDistance){
            float distScale = 1 - smoothstep(0, maxDistance, distanceVec);
            float factorS = 1.0;
            if(usingRadiusPerspective){
                factorS = pow(distScale, 9) * 500.0 * pow(rtemp, 2);
            }
            else{
                factorS = pow(distScale, 9) * 500.0;
            }
            gl_PointSize = factorS * maxNodeDistanceSize * 0.8; 
        }
        // else{
        // gl_PointSize = nodeSize;
        // }

        if(gl_PointSize > maxNodeSize){
            gl_PointSize = maxNodeSize;
        }
        
        if(gl_PointSize < minNodeSize){
            gl_PointSize = minNodeSize;
        }           
    }

    vs_time = time;
    vec4 position_in_meters = vec4(in_position, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    //vs_gPosition = vec4(modelViewTransform * dvec4(in_point_position, 1));
    
    gl_Position = vec4(positionClipSpace.xy, 0, positionClipSpace.w);
    vs_depth = gl_Position.w;

}
