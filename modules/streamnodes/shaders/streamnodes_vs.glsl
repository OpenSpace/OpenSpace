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

#version __CONTEXT__
#include "PowerScaling/powerScalingMath.hglsl"
// General Uniforms that's always needed
uniform vec4      lineColor;
//old not in use atm
uniform mat4      modelViewProjection;

// Uniforms needed to color by quantity
uniform int       colorMode;
uniform sampler1D colorTable;
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

// Streamnodes specific uniforms
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
uniform float   nodeSkipFluxThreshold;
uniform float   nodeSkipRadiusThreshold;
uniform float   fluxColorAlpha;
uniform vec3    earthPos;
uniform float   distanceThreshold;
uniform int     distanceMethod;
uniform int     activeStreamNumber;
uniform bool    firstRender;
uniform int     enhanceMethod;
uniform double  time;
uniform bool    usingInterestingStreams;


//uniform float interestingStreams[4];

// Speicific uniforms for cameraperspective
//uniform float scaleFactor;
//uniform float minNodeDistanceSize;
uniform float maxNodeDistanceSize;
uniform float nodeDistanceThreshold;

//uniform mat4 cameraViewProjectionMatrix;
//uniform dmat4 modelMatrix;

uniform float correctionSizeFactor;
//uniform float correctionSizeEndDistance;
//uniform vec3 up;
//uniform vec3 right;
uniform vec3    cameraLookUp;   // in world space (no SGCT View was considered)
uniform vec3    cameraPos;
//uniform vec2 screenSize;
uniform bool    usingCameraPerspective;
uniform bool    usingRadiusPerspective;
uniform float   perspectiveDistanceFactor;

uniform float   maxNodeSize;
uniform float   minNodeSize;
uniform bool    usingPulse;
// Inputs
// Should be provided in meters
layout(location = 0) in vec3 in_position;

// The extra value used to color lines. Location must correspond to _VA_COLOR in 
// renderablefieldlinessequence.h
layout(location = 1) in float fluxValue;

// The extra value used to mask out parts of lines. Location must correspond to 
// _VA_MASKING in renderablefieldlinessequence.h
layout(location = 2)
in float rValue;

// The vertex index of every node. Location must correspond to 
// _VA_INDEX in renderableStreamNodes.h
//Using built in gl_vertexID in stead. 
//layout(location = 3)
//in int nodeIndex;
// The vertex streamnumber of every node. Location must correspond to 
// VaStreamnumber in renderableStreamNodes.h
layout(location = 3)
in int Streamnumber;
layout(location = 4) 
in vec2 in_st;

//layout(location = 5) 
//in vec2 arrow;

// These should correspond to the enum 'ColorMode' in renderablestreamnodes.cpp
const int uniformColor     = 0;
const int colorByFluxValue  = 1;

const int uniformskip = 0;
const int fluxSkip = 1;
const int radiusSkip = 2;
const int streamNumberSkip = 3;

const int fluxMode = 0;
const int RFlux = 1;
const int R2Flux = 2;
const int log10RFlux = 3;
const int lnRFlux = 4;

const int sizeScaling = 0;
const int colorTables = 1;
const int outline = 2;
const int sizeAndColor = 3;
const int test = 4;

out vec4    vs_color;
out float   vs_depth;
out vec2    vs_st;
out float   camera_IsCloseEnough;
out float   vs_closeToEarth;
out double  vs_time;
out vec3    vs_camerapos;
//out vec4 vs_gPosition;

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
   // nodeIndex = gl_VertexIndex;
    //if(enhanceMethod == 3) return false;

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
    else if(nodeSkipMethod == streamNumberSkip){
        
    if(Streamnumber == activeStreamNumber){
        //vs_color = vec4(0);
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
    if(enhanceMethod == sizeScaling){
        float tempR = rValue + 0.4; 
        if(tempR > 1.5){
            tempR = 1.5;
        }
            gl_PointSize = tempR * tempR * tempR * gl_PointSize * 5;
            return;
    }
    // ColorTables
    if(enhanceMethod == colorTables){
        vec4 fluxColor = getTransferFunctionColor(colorTable);
        vs_color = vec4(fluxColor.xyz, fluxColor.a);
    }
    // Outline
    if(enhanceMethod == outline){
        if(!firstRender && vs_color.x != 0 && vs_color.y != 0){
            gl_PointSize = gl_PointSize + 1;
            vs_color = vec4(streamColor.xyz, fluxColorAlpha);
        }
        return;
    }
    //SizeColor
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

       if(distancevec < maxdist){
          if(distancevec < maxdist / 2 && usingPulse){
                vs_closeToEarth = 1;
                gl_PointSize = gl_PointSize * 5;
                vs_color = vec4(streamColor.xyz, fluxColorAlpha);
            }
        }
        if(enhanceMethod == colorTables || enhanceMethod == sizeAndColor){
             vec4 fluxColor2 = getTransferFunctionColor(colorTableEarth);
             vs_color = vec4(fluxColor2.xyz, fluxColorAlpha);
             //vs_color = vec4(0.3, 0.3, 0.3, 1.0);
        }
        if(distanceMethod == 0){
             if(distance(earthPos, in_position) < distanceThreshold){
                DecidehowtoshowClosetoEarth();
             }
        }
        else if(distanceMethod == 1){
            if(distance(earthPos.x, in_position.x) < distanceThreshold){
                DecidehowtoshowClosetoEarth();
            }
        }
        else if(distanceMethod == 2){
            if(distance(earthPos.y, in_position.y) < distanceThreshold){
                DecidehowtoshowClosetoEarth();
            }
        }
        else if(distanceMethod == 3){
            if(distance(earthPos.z, in_position.z) < distanceThreshold){
                DecidehowtoshowClosetoEarth();
            }
        }
}

void main() {
    //vs_color = streamColor;
    // Default gl_PointSize if it is not set anywhere else.
    gl_PointSize = 2;
    // Checking if we should render the vertex dependent on the vertexindex, 
    // by using modulus.
    
    if(CheckvertexIndex()){
    //Filtering by radius and z-axis
    if(rValue > filterLower && rValue < filterUpper){ //if(rValue > filterLower){
        if(in_position.z > domainLimZ.x && in_position.z < domainLimZ.y){
            //Uniform coloring
            if(colorMode == 0){
                vs_color = streamColor;
            }
            // We should color it by flux. 
            else{
                vec4 fluxColor = getTransferFunctionColor(colorTable);

                if(fluxValue > thresholdFlux){
                    vs_color = vec4(fluxColor.xyz, fluxColor.a);  
                    gl_PointSize = nodeSizeLargerFlux;
                }
                else{
                    vs_color = vec4(fluxColor.xyz, fluxColorAlpha);
                    gl_PointSize = nodeSize;
                }
            }
             CheckdistanceMethod();
        }
        else{
            vs_color = vec4(0);
            }
        }
    else{
        vs_color = vec4(0);
        }
    }
    else{
        vs_color = vec4(0);
    }

    if(usingInterestingStreams){
      // Draw every other line grey
      //vs_color = vec4(0.18, 0.18, 0.18, 1*fluxColorAlpha);

      vs_color = vec4(0);
      
      // Close to Earth (384 nodes)
      //float interestingStreams[8] = float[](339, 340, 351, 352, 353, 354, 366, 367);
      //float interestingStreams[6] = float[](154, 156, 153, 157, 158, 163);
      //float interestingStreams[26] =  float[](135, 138, 145, 146, 147, 149, 153, 154, 155, 156, 157, 158, 159, 160, 167, 163, 168, 169, 170, 172, 174, 180, 181, 183, 356, 364);
      //float interestingStreams[3] = float[](37, 154, 210);

      // Close to Earth (863 nodes)
      //float interestingStreams[7] = float[](340, 350, 351, 352, 353, 363, 364);
      //float interestingStreams[10] = float[](339, 340, 350, 351, 352, 353, 362, 363, 364, 365);
      float interestingStreams[20] = float[](326, 327, 328, 329, 338, 339, 340, 341, 350, 351, 352, 353, 362, 363, 364, 365, 374, 375, 376, 377);

        for(int i = 0; i < interestingStreams.length(); i++){
            if(Streamnumber == interestingStreams[i]){
                if(CheckvertexIndex()){
                vec4 fluxColor3 = getTransferFunctionColor(colorTable);
                vs_color = vec4(fluxColor3.xyz, 1*fluxColorAlpha);
                }
            }
        }
    }

    if(usingParticles && isParticle() && rValue > 0.f){
        int modulusResult = int(double(particleSpeed) * time + gl_VertexID) 
        % particleSpacing;

        if(modulusResult >= particleSize - 30){
            if(flowColoring){
                vec4 fluxColor3 = getTransferFunctionColor(colorTable);
                vs_color = vec4(fluxColor3.xyz, flowColor.a * 0.8);
                //vs_color = vec4(1,1,1,1);
            }
            else{
                vs_color = vec4(0.9,0.9,0.9,0.5);
                //vs_color = flowColor;
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
        //vs_closeToEarth = 0;
            float distScale = 1 - smoothstep(0, maxDistance, distanceVec);
            //float distMinScale = 1 - smoothstep(0, nodeDistanceThreshold, distanceVec);
            float factorS = 1.f;
            if(usingRadiusPerspective){
                factorS = pow(distScale, 9) * 500.f * pow(rtemp, 2);
            }
            else{
                factorS = pow(distScale, 9) * 500.f;
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

//------------ OLD CODE, MAYBE USEFUL FOR CAMERAPERSPECTIVE
 /*
    if(distance(in_position, cameraPos) < 100000000000.f){
        gl_PointSize = nodeSize * 5;
     }
    else{
    gl_PointSize = nodeSize;
    }
    */
    //test for camera perspective:: 
    /*
    dvec4 dpos = dvec4(in_position, 1.0);
    dpos = modelMatrix * dpos;

    float scaleMultiply = exp(scaleFactor * 0.10f);

    //vec3 scaledRight    = vec3(0.f);
    //vec3 scaledUp       = vec3(0.f);

    /////vec3 normal   = vec3(normalize(cameraPos - dpos.xyz));
    /////vec3 newRight = normalize(cross(cameraLookUp, normal));
    /////vec3 newUp    = cross(normal, newRight);

     double distCamera = length(cameraPos - dpos.xyz);
     float expVar = float(-distCamera) / pow(10.f, correctionSizeEndDistance);
     float factorVar = pow(10.f, correctionSizeFactor);
     scaleMultiply *= 1.f / (1.f + factorVar * exp(expVar));
     */
    //vec2 halfViewSize = vec2(screenSize.x, screenSize.y) * 0.5f;
      //  vec2 topRight = crossCorner.xy/crossCorner.w;
       // vec2 bottomLeft = initialPosition.xy/initialPosition.w;
        
        // width and height
        //vec2 sizes = abs(halfViewSize * (topRight - bottomLeft));
        //float ta = 1.0f;
    /*
    if (sizes.x < 2.0f * minNodeDistanceSize) {
                float maxVar = 2.0f * minNodeDistanceSize;
                float minVar = minNodeDistanceSize;
                float var    = (sizes.y + sizes.x);
                ta = ( (var - minVar)/(maxVar - minVar) );
               
                if (ta == 0.0f)
                    return;
            }
             gl_PointSize = ta;
        }
        */
      /* 
    vec3 scaledRight = scaleMultiply * right * 0.5f;
    vec3 scaledUp    = scaleMultiply * up * 0.5f;

    vec4 dposClip = cameraViewProjectionMatrix * vec4(dpos);
    vec4 scaledRightClip = cameraViewProjectionMatrix * vec4(scaledRight, 0.0);
    vec4 scaledUpClip = cameraViewProjectionMatrix * vec4(scaledUp, 0.0);

    vec4 initialPosition = z_normalization(dposClip - scaledRightClip - scaledUpClip);
    gl_Position = initialPosition;
    vs_depth = initialPosition.w;
    */
