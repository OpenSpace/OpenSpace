/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 1.0), 
    vec2(0.0, 0.0), 
    vec2(1.0, 1.0), 
    vec2(1.0, 0.0) 
);

#include "PowerScaling/powerScalingMath.hglsl"
#include <${SHADERS_GENERATED}/constants.hglsl>:notrack

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

layout(location = 0) in vec4 psc_position[];
layout(location = 1) in vec3 vs_brightness[];
layout(location = 2) in vec3 vs_velocity[];
layout(location = 3) in float vs_speed[];
layout(location = 4) in vec4 cam_position[];

layout(location = 0) out vec4 vs_position;
layout(location = 1) out vec3 ge_brightness;
layout(location = 2) out vec3 ge_velocity;
layout(location = 3) out float ge_speed;
layout(location = 4) out vec2 texCoord;
layout(location = 5) out float billboardSize;

uniform mat4 projection;

uniform float scaleFactor;

void main() {
    ge_brightness = vs_brightness[0];
    ge_velocity = vs_velocity[0];
    ge_speed = vs_speed[0];

    float absoluteMagnitude = vs_brightness[0].z;
    // float modifiedSpriteSize = exp((-exponentialOffset - absoluteMagnitude) * exponentialDampening) * scaleFactor * 50;
    float modifiedSpriteSize = exp((-30.623 - absoluteMagnitude) * 0.462) * scaleFactor * 2000;

    vec4 projPos[4];
    for (int i = 0; i < 4; ++i) {
        vec4 p1     = gl_in[0].gl_Position;
        p1.xy      += vec2(modifiedSpriteSize * (corners[i] - vec2(0.5))); 
        projPos[i] = projection * p1;
    }

    // Calculate the positions of the lower left and upper right corners of the
    // billboard in screen-space
    const vec2 screenSize = vec2(SCREEN_WIDTH, SCREEN_HEIGHT);
    vec2 ll = (((projPos[1].xy / projPos[1].w) + 1) / 2) * screenSize;
    vec2 ur = (((projPos[2].xy / projPos[2].w) + 1) / 2) * screenSize;

    // The billboard is smaller than one pixel, we can discard it
    vec2 distance = abs(ll - ur);
    float sizeInPixels = length(distance);
    if (sizeInPixels < 5)
        return;

    for(int i = 0; i < 4; i++){
        vs_position = gl_in[0].gl_Position;
        gl_Position = projPos[i];
        // gl_Position = z_normalization(projPos[i]);
        texCoord    = corners[i];
        billboardSize = sizeInPixels;
      EmitVertex();
    }
    EndPrimitive();
}
