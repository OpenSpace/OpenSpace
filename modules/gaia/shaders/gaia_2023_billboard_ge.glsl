/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include "floatoperations.glsl"

layout(points) in;
in vec2 vs_brightness[];
in vec4 vs_gPosition[];
in float vs_starDistFromSun[];
in float vs_cameraDistFromSun[];
in float vs_otherData[];

layout(triangle_strip, max_vertices = 4) out;
out vec2 ge_brightness;
out vec4 ge_gPosition;               
out vec2 texCoord;
out float ge_starDistFromSun;
out float ge_cameraDistFromSun;
out float ge_observedDist;
out float ge_otherData;
out float gs_screenSpaceDepth;

uniform dmat4 view;
uniform dmat4 projection;
uniform dvec3 cameraPos;
uniform dvec3 cameraLookUp;
uniform float viewScaling;
uniform float cutOffThreshold;
uniform float closeUpBoostDist;
uniform float billboardSize;
uniform int renderOption;
uniform float magnitudeBoost;
const float EPS = 1e-5;

double scaleForDistanceModulus(float absMag) {
  return exp((-30.623 - absMag) * 0.462) * pow(10.0, magnitudeBoost + 12.5) * 2000;
}

void main() {
    // float f = magnitudeBoost;
    // f = closeUpBoostDist;
    // f = billboardSize;
    // f = viewScaling;
    // f = cutOffThreshold;

    dmat4 cameraViewProjectionMatrix = projection * view; 

    dvec4 dpos = vs_gPosition[0]; //model * object position
    ge_brightness = vs_brightness[0];

    //Scale the billboard according to a distance modulus, this is taken from renderablestars because Brian thought it looked the best. 
    double scaleMultiply = scaleForDistanceModulus(vs_brightness[0].x);
    // scaleMultiply = 6.3e16;


    dvec3 normal = normalize(cameraPos - dpos.xyz);
    dvec3 newRight = normalize(cross(cameraLookUp, normal));
    dvec3 newUp = normalize(cross(normal, newRight));
    dvec3 scaledRight = scaleMultiply * newRight;
    dvec3 scaledUp = scaleMultiply * newUp;

    vec4 lowerLeft = z_normalization(
        vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w))
    );
  
    vec4 upperRight = z_normalization(
        vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w))
    ); 

    vec4 lowerRight = z_normalization(
        vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w))
    );
    
    vec4 upperLeft = z_normalization(
        vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w))
    );
    
    vec4 viewPosition = vec4(view * vs_gPosition[0]);
    ge_observedDist = safeLength(viewPosition / viewScaling);
    float distThreshold = cutOffThreshold - log(ge_observedDist) / log(4.0);

    if (length(gl_in[0].gl_Position) < EPS || distThreshold <= 0) {
        return;
    }
    gs_screenSpaceDepth = lowerLeft.w;


      // Build primitive    
    gl_Position = lowerLeft;
    texCoord = vec2(0.0, 0.0);
    ge_otherData = vs_otherData[0];
    EmitVertex();
    
    gl_Position = lowerRight;
    texCoord = vec2(1.0,0.0);
    ge_otherData = vs_otherData[0];
    EmitVertex();

    gl_Position = upperLeft;
    texCoord = vec2(0.0, 1.0);
    ge_otherData = vs_otherData[0];
    EmitVertex();
    
    gl_Position = upperRight;
    texCoord = vec2(1.0, 1.0);
    ge_otherData = vs_otherData[0];
    EmitVertex();
    
    EndPrimitive();
}