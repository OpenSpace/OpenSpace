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

#version __CONTEXT__

// General Uniforms that's always needed
uniform vec4      lineColor;
uniform mat4      modelViewProjection;

// Uniforms needed to color by quantity
uniform int       colorMethod;
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

// Inputs
layout(location = 0) in vec3 in_position;        // Should be provided in meters
layout(location = 1) in float in_color_scalar;   // The extra value used to color lines. Location must correspond to _VA_COLOR in renderablefieldlinessequence.h
layout(location = 2) in float in_masking_scalar; // The extra value used to mask out parts of lines. Location must correspond to _VA_MASKING in renderablefieldlinessequence.h

// These should correspond to the enum 'ColorMethod' in renderablefieldlinesequence.cpp
const int uniformColor     = 0;
const int colorByQuantity  = 1;

out vec4 vs_color;
out float vs_depth;
//out vec4 vs_gPosition;

vec4 getTransferFunctionColor() {
    // Remap the color scalar to a [0,1] range
    float lookUpVal = (in_color_scalar - colorTableRange.x) /
                            (colorTableRange.y - colorTableRange.x);
    return texture(colorTable, lookUpVal);
}

bool isPartOfParticle(const double time, const int vertexId, const int particleSize,
                      const int particleSpeed, const int particleSpacing) {
    int modulusResult = int(double(particleSpeed) * time + vertexId) % particleSpacing;
    return modulusResult > 0 && modulusResult <= particleSize;
}

void main() {

    bool hasColor = true;

    if (usingMasking && (in_masking_scalar < maskingRange.x ||
                         in_masking_scalar > maskingRange.y )) {
        hasColor = false;
    }

    if (usingDomain && hasColor) {
        float radius = length(in_position);

        if (in_position.x < domainLimX.x || in_position.x > domainLimX.y ||
            in_position.y < domainLimY.x || in_position.y > domainLimY.y ||
            in_position.z < domainLimZ.x || in_position.z > domainLimZ.y ||
            radius        < domainLimR.x || radius        > domainLimR.y) {

            hasColor = false;
        }
    }

    if (hasColor) {
        bool isParticle = usingParticles && isPartOfParticle(time, gl_VertexID,
                                                                    particleSize,
                                                                    particleSpeed,
                                                                    particleSpacing);

        if (isParticle) {
            vs_color = flowColor;
        } else {
            vs_color = lineColor;
        }

        if (colorMethod == colorByQuantity) {
            vec4 quantityColor = getTransferFunctionColor();
            vs_color = vec4(quantityColor.xyz, vs_color.a * quantityColor.a);
        }
    } else {
        vs_color = vec4(0);
    }

    vec4 position_in_meters = vec4(in_position, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    //vs_gPosition = vec4(modelViewTransform * dvec4(in_point_position, 1));
    gl_Position = vec4(positionClipSpace.xy, 0, positionClipSpace.w);

    vs_depth = gl_Position.w;
}
