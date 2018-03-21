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
    vec2 starSize = vec2(billboardSize) / screenSize * projectedPoint.w;
    
    float distanceToStar = length(ge_worldPosition.xyz - eyePosition);
    float distanceToStarInParsecs = length(ge_worldPosition.xyz / 3.0856776E16 - eyePosition / 3.0856776E16);

    float luminosity = ge_brightness.y;
    //starSize *= luminosity * 100;
    
    float absoluteMagnitude = ge_brightness.z;
    //starSize *= (absoluteMagnitude + 9.0)/4.0;
    //vec2 starSize = vec2((absoluteMagnitude + 9.0)*4.0) / screenSize * projectedPoint.w;
    
    float apparentMag = 5.0 * (log(distanceToStarInParsecs) - 1.0) + absoluteMagnitude;
    //starSize *= (apparentMag + 40.0)/30.0;
    //vec2 starSize = vec2(apparentMag + 40.0) / screenSize * projectedPoint.w;

    float apparentBrightnessSphere = (luminosity) / (4 * 3.14159265359 * distanceToStarInParsecs * distanceToStarInParsecs);
    //starSize = vec2(apparentBrightnessSphere * 100.0);

    float modifiedSpriteSize =
         exp((-30.623 - absoluteMagnitude) * 0.462);// * scaleFactor * 2000;
    //starSize = vec2(modifiedSpriteSize * 1E10);
    //vec2 starSize = vec2(billboardSize) / screenSize * modifiedSpriteSize * projectedPoint.w;
    //vec2 starSize = vec2(modifiedSpriteSize * 5E7) / screenSize * projectedPoint.w;

    // Working like Partiview
    float pSize = 100.0;
    float slum = 1.0;
    float samplingFactor = 1.0;
    float apparentBrightness = (pSize * slum * samplingFactor * luminosity) / distanceToStarInParsecs;
    //starSize *= apparentBrightness * 1E4;
    //starSize = vec2(apparentBrightness); 

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
