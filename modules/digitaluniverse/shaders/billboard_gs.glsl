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

#include "PowerScaling/powerScalingMath.hglsl"

layout(points) in;
//layout(triangle_strip, max_vertices = 6) out;
layout(triangle_strip, max_vertices = 4) out;

//uniform dmat4 transformMatrix;
uniform dmat4 modelViewProjectionTransform;
uniform float scaleFactor;
uniform dvec3 up;
uniform dvec3 right;
uniform dvec3 cameraPosition;
uniform dvec3 cameraLookUp;
uniform dvec4 centerScreenInWorldPosition;
uniform int renderOption;
uniform vec2 screenSize;
uniform float maxBillboardSize;
uniform float minBillboardSize;

uniform dmat4 modelViewMatrix;
uniform dmat4 projectionMatrix;

in vec4 colorMap[];

out vec4 gs_colorMap;
out vec2 texCoord;
out float vs_screenSpaceDepth;
out float ta;

const double PARSEC = 0.308567756e17LF;

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 0.0),
    vec2(1.0, 0.0), 
    vec2(1.0, 1.0),
    vec2(0.0, 1.0)     
);


void main() {
    ta  = 1.0f;
    vec4 pos    = gl_in[0].gl_Position;
    gs_colorMap = colorMap[0];
    
    double scaleMultiply = exp(scaleFactor/10);
    dvec3 scaledRight    = dvec3(0.0);
    dvec3 scaledUp       = dvec3(0.0);

    if (renderOption == 0) {
        scaledRight = scaleMultiply * right/2.0f;
        scaledUp    = scaleMultiply * up/2.0f;
    } else if (renderOption == 1) {
        dvec3 normal   = normalize(cameraPosition - dvec3(pos.xyz));
        dvec3 newRight = normalize(cross(cameraLookUp, normal));
        dvec3 newUp    = cross(normal, newRight);
        scaledRight    = scaleMultiply * newRight/2.0f;
        scaledUp       = scaleMultiply * newUp/2.0f;
    } else if (renderOption == 2) {
        dvec3 normal   = normalize(centerScreenInWorldPosition.xyz - dvec3(pos.xyz));
        dvec3 newRight = normalize(cross(cameraLookUp, normal));
        dvec3 newUp    = cross(normal, newRight);
        scaledRight    = scaleMultiply * newRight/2.0f;
        scaledUp       = scaleMultiply * newUp/2.0f;
    }

    double unit = PARSEC;

    // Must be the same as the enum in RenderableBillboardsCloud.h
    if (pos.w == 1.f) {
        unit = 1E3;
    } else if (pos.w == 2.f) {
        unit = PARSEC;
    } else if (pos.w == 3.f) {
        unit = 1E3 * PARSEC;
    } else if (pos.w == 4.f) {
        unit = 1E6 * PARSEC;
    } else if (pos.w == 5.f) {
        unit = 1E9 * PARSEC;
    } else if (pos.w == 6.f) {
        unit = 306391534.73091 * PARSEC;
    }
    
    dvec4 dpos = dvec4(dvec3(pos.xyz) * unit, 1.0); 

    vec4 initialPosition, secondPosition, thirdPosition, crossCorner;
     // Testing new alingment
    if (renderOption == 2) {
        dvec4 dposCameraSpace = modelViewMatrix * dpos;

        if (dposCameraSpace.z > 0.0) {
            return;
        }
        dvec3 xAxis = dvec3(0.5, 0.0, 0.0);
        dvec3 yAxis = dvec3(0.0, 0.5, 0.0);

        xAxis *= scaleMultiply;
        yAxis *= scaleMultiply;

        //xAxis *= unit * 0.80;
        //yAxis *= unit * 0.80;

        initialPosition = z_normalization(
            vec4(projectionMatrix * dvec4(dposCameraSpace.xyz - xAxis - yAxis, dposCameraSpace.w)
            ));
        vs_screenSpaceDepth = initialPosition.w;
        
        secondPosition = z_normalization(
            vec4(projectionMatrix * dvec4(dposCameraSpace.xyz + xAxis - yAxis, dposCameraSpace.w)));
        
        crossCorner = z_normalization(
            vec4(projectionMatrix * dvec4(dposCameraSpace.xyz + yAxis + xAxis, dposCameraSpace.w)));
        
        thirdPosition = z_normalization(
            vec4(projectionMatrix * dvec4(dposCameraSpace.xyz + yAxis - xAxis, dposCameraSpace.w)));
    } else {
        initialPosition = z_normalization(vec4(modelViewProjectionTransform * 
                            dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
        vs_screenSpaceDepth  = initialPosition.w;
        
        secondPosition = z_normalization(vec4(modelViewProjectionTransform * 
                        dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
        
        crossCorner = z_normalization(vec4(modelViewProjectionTransform * 
                            dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));
        
        thirdPosition = z_normalization(vec4(modelViewProjectionTransform * 
                        dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));
    }

    // Testing size:
    vec4 topRight = secondPosition/secondPosition.w;
    topRight =  ((topRight + vec4(1.0)) / vec4(2.0)) * vec4(screenSize.x, screenSize.y, 1.0, 1.0);
    vec4 bottomLeft = initialPosition/initialPosition.w;
    bottomLeft = ((bottomLeft + vec4(1.0)) / vec4(2.0)) * vec4(screenSize.x, screenSize.y, 1.0, 1.0);

    float height = abs(topRight.y - bottomLeft.y);
    float width  = abs(topRight.x - bottomLeft.x);    
    float var    = (height + width);    
    
    if ((height > maxBillboardSize) ||
        (width > maxBillboardSize)) {
        
        // Set maximum size as Carter's instructions
        float correctionScale = height > maxBillboardSize ? maxBillboardSize / (topRight.y - bottomLeft.y) :
                                                            maxBillboardSize / (topRight.x - bottomLeft.x);
        if (renderOption == 2) {
        } else {
            scaledRight = correctionScale * scaleMultiply * right/2.0f;
            scaledUp    = correctionScale * scaleMultiply * up/2.0f;
            initialPosition = z_normalization(vec4(modelViewProjectionTransform *
                                    dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
            vs_screenSpaceDepth  = initialPosition.w;
            secondPosition = z_normalization(vec4(modelViewProjectionTransform * 
                            dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
            crossCorner = z_normalization(vec4(modelViewProjectionTransform * 
                                dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));
            thirdPosition = z_normalization(vec4(modelViewProjectionTransform *
                            dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));
        }
    } 
    else if (width < 2.0f * minBillboardSize) {
        //return;
        float maxVar = 2.0f * minBillboardSize;
        float minVar = minBillboardSize;
        ta = ( (var - minVar)/(maxVar - minVar) );
        if (ta == 0.0f)
            return;
    } 

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
