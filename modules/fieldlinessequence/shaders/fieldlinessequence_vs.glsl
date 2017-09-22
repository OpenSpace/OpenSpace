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

#include "PowerScaling/powerScaling_vs.hglsl"

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

layout(location = 0) in vec3 in_position;      // Should be provided in meters
layout(location = 1) in float in_color_scalar; // The extra value used to color lines. Location must correspond to _VA_COLOR in renderablefieldlinessequence.h

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

    vec4 position_in_meters = vec4(in_position, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    gl_Position = z_normalization(positionClipSpace);
    vs_depth = gl_Position.w;
}
