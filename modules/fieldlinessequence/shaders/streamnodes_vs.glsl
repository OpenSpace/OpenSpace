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
// General Uniforms that's always needed
uniform vec4      lineColor;
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
uniform float nodeSize;
uniform float nodeSizeLargerFlux;
uniform vec4 streamColor;
uniform float thresholdFlux;
uniform float filterRadius;
uniform float filterUpper;
uniform int ScalingMode;
uniform int NodeskipMethod;
uniform int Nodeskip;
uniform int Nodeskipdefault;
uniform float NodeskipFluxThreshold;
uniform float NodeskipRadiusThreshold;
uniform float fluxColorAlpha;
uniform vec3 earthPos;
uniform float DistanceThreshold;
uniform int DistanceMethod;
uniform int activestreamnumber;
uniform bool firstrender;
uniform int EnhanceMethod;
uniform double    time;
//uniform vec3 camerapos;
//uniform float interestingStreams[4];

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

// These should correspond to the enum 'ColorMode' in renderablestreamnodes.cpp
const int uniformColor     = 0;
const int colorByFluxValue  = 1;

const int uniformskip = 0;
const int Fluxskip = 1;
const int Radiusskip = 2;
const int Streamnumberskip = 3;


const int Fluxmode = 0;
const int RFlux = 1;
const int R2Flux = 2;
const int log10RFlux = 3;
const int lnRFlux = 4;
out vec4 vs_color;
out float vs_depth;
out vec2 vs_st;
//out vec4 vs_gPosition;

vec4 getTransferFunctionColor(sampler1D InColorTable) {
    // Remap the color scalar to a [0,1] range
    float scalevalue = 0;
    if(ScalingMode == Fluxmode){
        scalevalue = fluxValue;
    }
    else if(ScalingMode == RFlux){
       scalevalue = rValue * fluxValue;
    }
    else if(ScalingMode == log10RFlux){
        //conversion from logbase e to log10 since glsl does not support log10. 
        float logtoTen = log(rValue) / log(10);
        scalevalue = logtoTen * fluxValue;
    }
    else if(ScalingMode == lnRFlux){
        scalevalue = log(rValue) * fluxValue;
    }
    else if(ScalingMode == R2Flux){
        scalevalue = rValue * rValue * fluxValue;
    }

    float lookUpVal = (scalevalue - colorTableRange.x)
        /(colorTableRange.y - colorTableRange.x);
    return texture(InColorTable, lookUpVal);
}


bool CheckvertexIndex(){
    
    int nodeIndex = gl_VertexID;
   // nodeIndex = gl_VertexIndex;
    //if(EnhanceMethod == 3) return false;
    if(NodeskipMethod == uniformskip){
        if(mod(nodeIndex, Nodeskip) == 0){
            return true;
        }
    }
    else if(NodeskipMethod == Fluxskip){
        if(fluxValue > NodeskipFluxThreshold && mod(nodeIndex, Nodeskip) == 0){
            return true;
        }
        if(fluxValue < NodeskipFluxThreshold && mod(nodeIndex, Nodeskipdefault) == 0){
            return true;
        }
    }
    else if(NodeskipMethod == Radiusskip){
        if(rValue < NodeskipRadiusThreshold && mod(nodeIndex, Nodeskip) == 0){
            return true;
        }
        if(rValue > NodeskipRadiusThreshold && mod(nodeIndex, Nodeskipdefault) == 0){
            return true;
        }
    }
    else if(NodeskipMethod == Streamnumberskip){
        
    if(Streamnumber == activestreamnumber){
        //vs_color = vec4(0);
        return true;
        }
    }
    return false;
}
//todo fix gl_VertexID

//is Particle?: 
bool isParticle(){
    int modulusResult = int(double(particleSpeed) * time + gl_VertexID) % particleSpacing;
    return modulusResult > 0 && modulusResult <= particleSize;
return false;
}

//function for showing nodes different depending on distance to earth
void DecidehowtoshowClosetoEarth(){
        //Sizescaling
     if(EnhanceMethod == 0){
            float tempR = rValue + 0.4; 
            if(tempR > 1.5){
                tempR = 1.5;
            }
            gl_PointSize = tempR * tempR * tempR * gl_PointSize * 5;
            return;
        }
        //Colortables
      if(EnhanceMethod == 1){
             vec4 fluxColor = getTransferFunctionColor(colorTable);
             vs_color = vec4(fluxColor.xyz, fluxColor.a);
             return;
        }
        //Outline
      if(EnhanceMethod == 2){
            if(!firstrender && vs_color.x != 0 && vs_color.y != 0){
                 gl_PointSize = gl_PointSize + 1;
                 vs_color = vec4(streamColor.xyz, fluxColorAlpha);
            }
            return;
        }
        //lines
      if(EnhanceMethod == 3){
      // Draw every other line grey
      vs_color = vec4(0.18, 0.18, 0.18, 1*fluxColorAlpha);

      float interestingStreams[4] = float[](154, 156, 153, 163);
       // vs_color = vec4(0);
      //float interestingStreams[26] =  float[](135, 138, 145, 146, 147, 149, 153, 154, 155, 156, 157, 158, 159, 160, 167, 163, 
      //168, 169, 170, 172, 174, 180, 181, 183, 356, 364);
      //float interestingStreams[3] = float[](37, 154, 210);
      
      for(int i = 0; i < interestingStreams.length(); i++){
            if(Streamnumber == interestingStreams[i]){
        
           // if(!firstrender){
               // vs_color = vec4(streamColor.xyz, fluxColorAlpha);
               if(usingParticles && isParticle() && rValue > 0.f){
                   int modulusResult = int(double(particleSpeed) * time + gl_VertexID) % particleSpacing;
                   if(modulusResult >= particleSize - 10){
                        //vs_color = vec4(1,1,1,1);
                        if(flowColoring){
                        vec4 fluxColor3 = getTransferFunctionColor(colorTable);
                        vs_color = vec4(fluxColor3.xyz, flowColor.a * 0.8);
                         //vs_color = vec4(1,1,1,1);
                        }
                        else{
                        //vs_color = vec4(1,1,1,1);
                        vs_color = flowColor;
                        }
                   }
                   else{
                        vec4 fluxColorFlow = getTransferFunctionColor(colorTableFlow);
                        vs_color = vec4(fluxColorFlow.xyz, flowColor.a);
                        }
                        //vs_color = vec4(0.37, 0.37, 0.37, flowColor.a);
               }
               else{
                   vec4 fluxColor3 = getTransferFunctionColor(colorTable);
                   vs_color = vec4(fluxColor3.xyz, fluxColor3.a);
                 // vs_color = vec4(0.37, 0.37, 0.37, flowColor.a);
                }
            }
       }
        //    }
    }
        //SizeandColor
    if(EnhanceMethod == 4){
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
        if(EnhanceMethod == 1 || EnhanceMethod == 4){
             vec4 fluxColor2 = getTransferFunctionColor(colorTableEarth);
             vs_color = vec4(fluxColor2.xyz, fluxColor2.a);
        }
        if(DistanceMethod == 0){
             if(distance(earthPos, in_position) < DistanceThreshold){
                DecidehowtoshowClosetoEarth();
             }
        }
        else if(DistanceMethod == 1){
            if(distance(earthPos.x, in_position.x) < DistanceThreshold){
                DecidehowtoshowClosetoEarth();
            }
        }
        else if(DistanceMethod == 2){
            if(distance(earthPos.y, in_position.y) < DistanceThreshold){
                DecidehowtoshowClosetoEarth();
            }
        }
        else if(DistanceMethod == 3){
            if(distance(earthPos.z, in_position.z) < DistanceThreshold){
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
    if(rValue > filterRadius && rValue < filterUpper){ //if(rValue > filterRadius){
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
    //if(!firstrender){
    //CheckdistanceMethod();
   // }
    
    //temporary things for trying out point sprites. 
      /*  if(!firstrender && vs_color.w != 0){
            vs_st = in_st;
        }
        else{
        vs_st = vec2(-1);
        }
        */
        vec4 position_in_meters = vec4(in_position, 1);
        vec4 positionClipSpace = modelViewProjection * position_in_meters;
        //vs_gPosition = vec4(modelViewTransform * dvec4(in_point_position, 1));
       /* if(distance(position_in_meters.xyz, camerapos) < 3000000000.f){
        vs_color = streamColor;
        }
        else{
        vs_color = vs_color;
        }
        */
        //gl_PointSize = nodeSize;
        gl_Position = vec4(positionClipSpace.xy, 0, positionClipSpace.w);
        vs_depth = gl_Position.w;
        
       // if(distance(positionClipSpace.xyz, camerapos) < 0.f){
       
        
        
}
