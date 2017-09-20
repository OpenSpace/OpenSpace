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

uniform mat4   modelViewProjection;
uniform bool   usingParticles;
uniform double time;
uniform int    particleSize;
uniform int    particleSpeed;
uniform int    particleSpacing;
uniform vec4   flowColor;
uniform vec4   lineColor;

layout(location = 0) in vec3 in_position; // in meters

out vec4 vs_color;
out float vs_depth;

#include "PowerScaling/powerScaling_vs.hglsl"

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

    vec4 position_in_meters = vec4(in_position, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    gl_Position = z_normalization(positionClipSpace);
    vs_depth = gl_Position.w;
}
