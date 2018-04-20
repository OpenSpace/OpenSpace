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
#include "PowerScaling/powerScalingMath.hglsl"

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in vec3 vs_brightness[];
in vec3 vs_velocity[];
in vec4 vs_gPosition[];
in float vs_speed[];
//in vec4 vs_worldPosition[];

out vec4 vs_position;
out vec3 ge_brightness;
out vec3 ge_velocity;
out float ge_speed;
out vec2 psfCoords;
out float ge_observationDistance;
out float gs_screenSpaceDepth;

//uniform float viewScaling;
uniform float scaleFactor;
uniform float billboardSize;
uniform float magnitudeExponent;
uniform vec2 screenSize;
uniform dvec3 eyePosition;
uniform dvec3 cameraUp;

uniform dmat4 cameraViewProjectionMatrix;
uniform dmat4 modelMatrix;

uniform dmat4 cameraViewMatrix;
uniform dmat4 projectionMatrix;

const double PARSEC = 0.308567756e17LF;

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 0.0),
    vec2(1.0, 0.0), 
    vec2(1.0, 1.0),
    vec2(0.0, 1.0)     
);

void main() {
    vs_position = gl_in[0].gl_Position; // in object space
    
    // JCC: Don't display the Sun for now.
    if ((vs_position.x == 0.0) &&
        (vs_position.y == 0.0) &&
        (vs_position.z == 0.0))
    {
        return;
    }
    
    vs_position = gl_in[0].gl_Position; // in object space
    dvec4 dpos  = modelMatrix * dvec4(vs_position); 

    dvec4 clipTestPos = cameraViewProjectionMatrix * dpos;
    clipTestPos /= clipTestPos.w;
    if ((clipTestPos.x < -1.0 || clipTestPos.x > 1.0) ||
        (clipTestPos.y < -1.0 || clipTestPos.y > 1.0)) {
        return;
    }

    ge_brightness    = vs_brightness[0];
    ge_velocity      = vs_velocity[0];
    ge_speed         = vs_speed[0];
    
    double distanceToStarInParsecs = length(dpos.xyz - eyePosition) / PARSEC;
    double luminosity = double(ge_brightness.y);
    float absMag = ge_brightness.x;
    float appMag = absMag + 5 * (log(float(distanceToStarInParsecs))-1.0);
    
    // Working like Partiview
    double pSize              = pow(10, magnitudeExponent + 14.0);
    // double apparentBrightness = (pSize * luminosity) /
    //  (distanceToStarInParsecs * distanceToStarInParsecs);
    double apparentBrightness = pSize * luminosity / distanceToStarInParsecs;
    double scaleMultiply = apparentBrightness;  

    dvec3 scaledRight    = dvec3(0.0);
    dvec3 scaledUp       = dvec3(0.0);
    vec4 bottomLeftVertex, bottomRightVertex, topLeftVertex, topRightVertex;
  
    dvec3 normal   = normalize(eyePosition - dpos.xyz);
    dvec3 newRight = normalize(cross(cameraUp, normal));
    dvec3 newUp    = cross(normal, newRight);
    scaledRight    = scaleMultiply * newRight * 0.5f;
    scaledUp       = scaleMultiply * newUp * 0.5f;

    bottomLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
    gs_screenSpaceDepth  = bottomLeftVertex.w;
    
    topRightVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                            dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));        

    // Testing size:
    vec2 halfViewSize = vec2(screenSize.x, screenSize.y) * 0.5f;
    vec2 topRight = topRightVertex.xy/topRightVertex.w;
    vec2 bottomLeft = bottomLeftVertex.xy/bottomLeftVertex.w;
    
    // Complete algebra
    // topRight =  ((topRight + vec2(1.0)) * halfViewSize) - vec2(0.5);
    // bottomLeft = ((bottomLeft + vec2(1.0)) * halfViewSize) - vec2(0.5);
    //vec2 sizes = abs(topRight - bottomLeft);
    
    // Optimized version
    vec2 sizes = abs(halfViewSize * (topRight - bottomLeft));
    
    float height = sizes.y;
    float width  = sizes.x;

    if ((height > billboardSize) ||
        (width > billboardSize)) { 
        float correctionScale = height > billboardSize ? billboardSize / height :
                                                         billboardSize / width;

        scaledRight *= correctionScale;
        scaledUp    *= correctionScale;
        bottomLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix *
                                dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
        gs_screenSpaceDepth = bottomLeftVertex.w;
        topRightVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                            dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w))); 

    
        bottomRightVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                    dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
        
        topLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));
    } else {
        // if (width < 2.0f) {
        //     float maxVar = 2.0f;
        //     float minVar = 1.0f;
        //     float var    = (height + width);    
        //     float ta = ( (var - minVar)/(maxVar - minVar) );
        //     if (ta == 0.0f)
        //         return;
        // }  
        // float minSize = 30.f; 
        // if ((width < minSize) || (height < minSize))
        //     return;         
        bottomRightVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                    dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
        topLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));
    } 

    // Build primitive
    gl_Position = topLeftVertex;
    psfCoords   = vec2(-1.0, 1.0);
    EmitVertex();
    gl_Position = bottomLeftVertex;
    psfCoords   = vec2(-1.0, -1.0);
    EmitVertex();
    gl_Position = topRightVertex;
    psfCoords   = vec2(1.0, 1.0);
    EmitVertex();
    gl_Position = bottomRightVertex;
    psfCoords   = vec2(1.0, -1.0);
    EmitVertex();
    EndPrimitive();     
}
