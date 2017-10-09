/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2017                                                                    *
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
const int UNIFORM_COLOR     = 0;
const int COLOR_BY_QUANTITY = 1;

out vec4 vs_color;
out float vs_depth;


vec4 getTransferFunctionColor() {
    // Remap the color scalar to a [0,1] range
    const float LOOK_UP_VAL = (in_color_scalar - colorTableRange.x) /
                              (colorTableRange.y - colorTableRange.x);
    return texture(colorTable, LOOK_UP_VAL);
}

bool isPartOfParticle(const double TIME, const int VERTEX_ID, const int PARTICLE_SIZE,
                      const int PARTICLE_SPEED, const int PARTICLE_SPACING) {
    const int MODULUS_RESULT = int(double(PARTICLE_SPEED) * TIME + VERTEX_ID) % PARTICLE_SPACING;
    return MODULUS_RESULT > 0 && MODULUS_RESULT <= PARTICLE_SIZE;
}

void main() {

    bool hasColor = true;

    if (usingMasking && (in_masking_scalar < maskingRange.x ||
                         in_masking_scalar > maskingRange.y )) {
        hasColor = false;
    }

    if (usingDomain && hasColor) {
        const float RADIUS = length(in_position);

        if (in_position.x < domainLimX.x || in_position.x > domainLimX.y ||
            in_position.y < domainLimY.x || in_position.y > domainLimY.y ||
            in_position.z < domainLimZ.x || in_position.z > domainLimZ.y ||
            RADIUS        < domainLimR.x || RADIUS        > domainLimR.y) {

            hasColor = false;
        }
    }

    if (hasColor) {
        const bool IS_PARTICLE = usingParticles && isPartOfParticle(time, gl_VertexID,
                                                                    particleSize,
                                                                    particleSpeed,
                                                                    particleSpacing);

        if (IS_PARTICLE) {
            vs_color = flowColor;
        } else {
            vs_color = lineColor;
        }

        if (colorMethod == COLOR_BY_QUANTITY) {
            const vec4 QUANTITY_COLOR = getTransferFunctionColor();
            vs_color = vec4(QUANTITY_COLOR.xyz, vs_color.a * QUANTITY_COLOR.a);
        }
    } else {
        vs_color = vec4(0);
    }

    vec4 position_in_meters = vec4(in_position, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    gl_Position = vec4(positionClipSpace.xy, 0, positionClipSpace.w);
    vs_depth = gl_Position.w;
}
