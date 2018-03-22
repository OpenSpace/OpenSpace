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

#include "floatoperations.glsl"

layout(points) in;

in vec3 vs_brightness[];
in vec3 vs_velocity[];
in vec4 vs_gPosition[];
in float vs_speed[];
in vec4 vs_worldPosition[];

layout(triangle_strip, max_vertices = 4) out;

out vec4 vs_position;
out vec4 ge_gPosition;               
out vec3 ge_brightness;
out vec3 ge_velocity;
out float ge_speed;
out vec2 texCoord;
out float ge_observationDistance;
out vec4 ge_worldPosition;

uniform float viewScaling;
uniform float scaleFactor;
uniform float billboardSize;
uniform vec2 screenSize;
uniform vec3 eyePosition;

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 1.0), 
    vec2(0.0, 0.0), 
    vec2(1.0, 1.0), 
    vec2(1.0, 0.0) 
);

void main() {

    if ((vs_worldPosition[0].x == 0.0) &&
        (vs_worldPosition[0].y == 0.0) &&
        (vs_worldPosition[0].z == 0.0))
    {
        return;
    }

    ge_brightness = vs_brightness[0];
    ge_velocity = vs_velocity[0];
    ge_speed = vs_speed[0];
    ge_worldPosition = vs_worldPosition[0];

    vec4 projectedPoint = gl_in[0].gl_Position;
    
    float distanceToStarInParsecs = length(ge_worldPosition.xyz / 3.0856776E16 - eyePosition / 3.0856776E16);

    float luminosity = ge_brightness.y;
      
    // Working like Partiview
    float pSize = 3.0E5;
    float slum = 1.0;
    float samplingFactor = 1.0;
    float apparentBrightness = (pSize * slum * samplingFactor * luminosity) / (distanceToStarInParsecs * distanceToStarInParsecs);
    
    vec2 multiplier = vec2(apparentBrightness * projectedPoint.w);
   
    // Max Star Sizes:
    // Fragment Coords:
    vec2 bottomLeft = screenSize * ((projectedPoint.xy + vec2(multiplier) * corners[1])/projectedPoint.w + vec2(1.0)) - vec2(0.5);
    vec2 topRight   = screenSize * ((projectedPoint.xy + vec2(multiplier) * corners[2])/projectedPoint.w + vec2(1.0)) - vec2(0.5);

    float height = abs(topRight.y - bottomLeft.y);
    float width  = abs(topRight.x - bottomLeft.x);    
    float var    = (height + width);

    float maxBillboardSize = billboardSize;
    float minBillboardSize = 1.0;

    if ((height > maxBillboardSize) ||
        (width > maxBillboardSize)) {
    //if (height > maxBillboardSize) {        
        float correctionScale = height > maxBillboardSize ? maxBillboardSize / (topRight.y - bottomLeft.y) :
                                                            maxBillboardSize / (topRight.x - bottomLeft.x);
        multiplier *= correctionScale;
    } else {            
        if (width < 2.0f * minBillboardSize) {
            float maxVar = 2.0f * minBillboardSize;
            float minVar = minBillboardSize;
            float ta = ( (var - minVar)/(maxVar - minVar) );
            if (ta == 0.0f)
                return;
        }        
    } 

    vec2 starSize = multiplier;
    
    for (int i = 0; i < 4; i++) {
        vs_position = gl_in[0].gl_Position;
        gl_Position = projectedPoint + vec4(starSize * (corners[i] - 0.5), 0.0, 0.0);
        gl_Position.z = 0.0;

        texCoord    = corners[i];

        // G-Buffer
        ge_gPosition  = vs_gPosition[0];
        ge_observationDistance = safeLength(vs_gPosition[0] / viewScaling);

        EmitVertex();
    }

    EndPrimitive();
}
