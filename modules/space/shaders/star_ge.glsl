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
<<<<<<< HEAD
in vec4 vs_correctedPositionViewSpace[];

layout(triangle_strip, max_vertices = 4) out;
=======
//in vec4 vs_worldPosition[];
>>>>>>> 02a910bc5d14de80267c24fbc83de961e9159085

out vec4 vs_position;
out vec3 ge_brightness;
out vec3 ge_velocity;
out float ge_speed;
out vec2 texCoord;
<<<<<<< HEAD
out float billboardSize;
out float gs_clipSpaceDepth;

uniform mat4 projectionMatrix;
uniform mat4 viewMatrix;
=======
out float ge_observationDistance;
out float gs_screenSpaceDepth;
out float ta;
>>>>>>> 02a910bc5d14de80267c24fbc83de961e9159085

//uniform float viewScaling;
uniform float scaleFactor;
uniform float billboardSize;
uniform float magnitudeExponent;
uniform vec2 screenSize;
uniform dvec3 eyePosition;
uniform dvec3 cameraUp;

//uniform dmat4 modelViewMatrix;
//uniform dmat4 projectionMatrix;
uniform dmat4 cameraViewProjectionMatrix;
uniform dmat4 modelMatrix;

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
    
    //ta          = 1.0f;
    vs_position = gl_in[0].gl_Position; // in object space
    vec4 pos    = vs_position; 
    dvec4 dpos  = dvec4(dvec3(modelMatrix * pos), 1.0); 

    ge_brightness    = vs_brightness[0];
    ge_velocity      = vs_velocity[0];
    ge_speed         = vs_speed[0];
    
    vec4 projectedPoint = gl_in[0].gl_Position;
    
    dvec3 starPositionInParsecs = dpos.xyz / PARSEC;
    dvec3 eyePositionInParsecs  = eyePosition / PARSEC;
    double distanceToStarInParsecs = length(starPositionInParsecs - eyePositionInParsecs);
    double luminosity = double(ge_brightness.y);
    //float absMag = ge_brightness.x;
    //float appMag = absMag + 5 * (log(distanceToStarInParsecs)-1.0);
    
    // Working like Partiview
    double pSize              = pow(10, magnitudeExponent + 14.0);
    double apparentBrightness = (pSize * luminosity) /
     (distanceToStarInParsecs * distanceToStarInParsecs);
    
    //vec2 multiplier = vec2(apparentBrightness/screenSize * projectedPoint.w);
    double scaleMultiply = apparentBrightness;  

    dvec3 scaledRight    = dvec3(0.0);
    dvec3 scaledUp       = dvec3(0.0);
    vec4 initialPosition, secondPosition, thirdPosition, crossCorner;
  
    dvec3 normal   = normalize(eyePosition - dpos.xyz);
    dvec3 newRight = normalize(cross(cameraUp, normal));
    dvec3 newUp    = cross(normal, newRight);
    scaledRight    = scaleMultiply * newRight * 0.5f;
    scaledUp       = scaleMultiply * newUp * 0.5f;

    initialPosition = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
    gs_screenSpaceDepth  = initialPosition.w;
    
    crossCorner = z_normalization(vec4(cameraViewProjectionMatrix * 
                            dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));        

    // Testing size:
    vec2 halfViewSize = vec2(screenSize.x, screenSize.y) / 2.0f;
    vec2 topRight = crossCorner.xy/crossCorner.w;
    topRight =  ((topRight + vec2(1.0)) * halfViewSize) - vec2(0.5);
    vec2 bottomLeft = initialPosition.xy/initialPosition.w;
    bottomLeft = ((bottomLeft + vec2(1.0)) * halfViewSize) - vec2(0.5);

    float height = abs(topRight.y - bottomLeft.y);
    float width  = abs(topRight.x - bottomLeft.x);    
    
    // JCC: Change this (horrible code :-))
    // while((height > billboardSize) ||
    //       (width > billboardSize)) {
    //     scaledRight *= 0.90;
    //     scaledUp    *= 0.90;
    //     crossCorner = z_normalization(vec4(cameraViewProjectionMatrix * 
    //         dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));
    //     initialPosition = z_normalization(vec4(cameraViewProjectionMatrix *
    //         dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
    //     topRight = crossCorner.xy/crossCorner.w;
    //     topRight =  ((topRight + vec2(1.0)) * halfViewSize) - vec2(0.5);
    //     bottomLeft = initialPosition.xy/initialPosition.w;
    //     bottomLeft = ((bottomLeft + vec2(1.0)) * halfViewSize) - vec2(0.5);
    //     height = abs(topRight.y - bottomLeft.y);
    //     width  = abs(topRight.x - bottomLeft.x);  
    // }

    // if ((height > billboardSize) ||
    //     (width > billboardSize)) {    


    // }

    // if ((height > billboardSize) ||
    //     (width > billboardSize)) {        
    //     // Set maximum size as Carter's instructions
    //     float correctionScale = height > billboardSize ? billboardSize / (topRight.y - bottomLeft.y) :
    //                                                      billboardSize / (topRight.x - bottomLeft.x);
    //     scaledRight *= correctionScale/2.0;
    //     scaledUp    *= correctionScale/2.0;
    //     initialPosition = z_normalization(vec4(cameraViewProjectionMatrix *
    //                             dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
    //     gs_screenSpaceDepth = initialPosition.w;
    //     secondPosition = z_normalization(vec4(cameraViewProjectionMatrix * 
    //                     dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
    //     crossCorner = z_normalization(vec4(cameraViewProjectionMatrix * 
    //                         dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));
    //     thirdPosition = z_normalization(vec4(cameraViewProjectionMatrix *
    //                     dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));
        
    // } else {            
        // if (width < 2.0f * minBillboardSize) {
        //     float maxVar = 2.0f * minBillboardSize;
        //     float minVar = minBillboardSize;
        //     float var    = (height + width);    
        //     ta = ( (var - minVar)/(maxVar - minVar) );
        //     if (ta == 0.0f)
        //         return;
        // }

        secondPosition = z_normalization(vec4(cameraViewProjectionMatrix * 
                    dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
        
        thirdPosition = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));
   // } 

    // Build primitive
    texCoord    = corners[3];
    gl_Position = thirdPosition;
    EmitVertex();
    texCoord    = corners[0];
    gl_Position = initialPosition;
    EmitVertex();
    texCoord    = corners[2];
    gl_Position = crossCorner;
    EmitVertex();
    texCoord    = corners[1];
    gl_Position = secondPosition;
    EmitVertex();
    EndPrimitive();     
}
