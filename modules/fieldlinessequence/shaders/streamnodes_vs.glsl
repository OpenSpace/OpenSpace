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
uniform vec2      colorTableRange;

// Uniforms needed for Particle Flow
uniform vec4      flowColor;
uniform int       particleSize;
uniform int       particleSpeed;
uniform int       particleSpacing;
uniform double    time;
uniform bool      usingParticles;

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
uniform vec4 streamColor;
uniform float thresholdRadius;
uniform float filterRadius;
uniform float filterUpper;

// Inputs
// Should be provided in meters
layout(location = 0) in vec3 in_position;

// The extra value used to color lines. Location must correspond to _VA_COLOR in 
// renderablefieldlinessequence.h
layout(location = 1) in float rTimesFluxValue;

// The extra value used to mask out parts of lines. Location must correspond to 
// _VA_MASKING in renderablefieldlinessequence.h
layout(location = 2)
in float rValue;


// These should correspond to the enum 'ColorMode' in renderablestreamnodes.cpp
const int uniformColor     = 0;
const int colorByFluxValue  = 1;

out vec4 vs_color;
out float vs_depth;
//out vec4 vs_gPosition;

vec4 getTransferFunctionColor() {
    // Remap the color scalar to a [0,1] range
    float lookUpVal = (rTimesFluxValue - colorTableRange.x) /
                            (colorTableRange.y - colorTableRange.x);
    return texture(colorTable, lookUpVal);
}

bool isPartOfParticle(const double time, const int vertexId, const int particleSize,
                      const int particleSpeed, const int particleSpacing) {
    int modulusResult = int(double(particleSpeed) * time + vertexId) % particleSpacing;
    return modulusResult > 0 && modulusResult <= particleSize;
}

void main() {

    //vs_color = streamColor;

    const float largerFlux  = -2;
    if((rValue > filterRadius) && (rValue < filterUpper)){
    //if(rValue > filterRadius){
    if(colorMode == 0){
    vs_color = streamColor;
    }
    else{
    if(rTimesFluxValue > (thresholdRadius + 0.5)){
        vs_color = vec4(0.4, 0.9, 0.2, 1.0);
        
    }
    else if(rTimesFluxValue > thresholdRadius){
        vs_color = vec4(0.9, 0.2, 0.2, 1.0);
    }
    else{
        //vs_color = vec4(0.2, 0.5, 0.5, 0.5);
        //vs_color = vec4(0);
        vs_color = streamColor;
    }
    }
    }
    else{
    vs_color = vec4(0);
    }

    //if (colorMethod == colorByFluxValue) {
    //    vec4 quantityColor = getTransferFunctionColor();
    //    vs_color = vec4(quantityColor.xyz, vs_color.a * quantityColor.a);
    //}

    //if(rValue > thresholdRadius){
    //  vs_color = vec4(0);
    //}

        vec4 position_in_meters = vec4(in_position, 1);
        vec4 positionClipSpace = modelViewProjection * position_in_meters;
        //vs_gPosition = vec4(modelViewTransform * dvec4(in_point_position, 1));
        gl_Position = vec4(positionClipSpace.xy, 0, positionClipSpace.w);

        vs_depth = gl_Position.w;
}
