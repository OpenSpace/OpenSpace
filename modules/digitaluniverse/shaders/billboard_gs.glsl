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
layout(triangle_strip, max_vertices = 4) out;

uniform float scaleFactor;
uniform dvec3 up;
uniform dvec3 right;
uniform dvec3 cameraPosition; // in world space (no SGCT View was considered)
uniform dvec3 cameraLookUp;   // in world space (no SGCT View was considered)
uniform int renderOption;
uniform vec2 screenSize;
uniform float maxBillboardSize;
uniform float minBillboardSize;

uniform dmat4 cameraViewProjectionMatrix;
uniform dmat4 modelMatrix;

uniform float correctionSizeFactor;
uniform float correctionSizeEndDistance;

uniform bool enabledRectSizeControl;

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
    ta          = 1.0f;
    vec4 pos    = gl_in[0].gl_Position; // in object space
    gs_colorMap = colorMap[0];

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
        // Convertion factor from Parsecs to GigalightYears
        unit = 306391534.73091 * PARSEC;
    }

    dvec4 dpos = dvec4(dvec3(pos.xyz) * unit, 1.0); 
    dpos = modelMatrix * dpos;

    double scaleMultiply = exp(scaleFactor * 0.10);
    dvec3 scaledRight    = dvec3(0.0);
    dvec3 scaledUp       = dvec3(0.0);
    vec4 initialPosition, secondPosition, thirdPosition, crossCorner;
  
    if (renderOption == 0) {
        scaledRight = scaleMultiply * right * 0.5f;
        scaledUp    = scaleMultiply * up * 0.5f;
    } else if (renderOption == 1) {
        dvec3 normal   = normalize(cameraPosition - dpos.xyz);
        dvec3 newRight = normalize(cross(cameraLookUp, normal));
        dvec3 newUp    = cross(normal, newRight);

        if (!enabledRectSizeControl) {
            double distCamera = length(cameraPosition - dpos.xyz);
            float expVar = float(-distCamera) / pow(10.f, correctionSizeEndDistance);
            double factorVar = double(pow(10, correctionSizeFactor));
            scaleMultiply *= 1.0 / (1.0 + factorVar * double(exp(expVar)));
        }

        scaledRight    = scaleMultiply * newRight * 0.5f;
        scaledUp       = scaleMultiply * newUp * 0.5f;
    }
    
    if (enabledRectSizeControl) {
        initialPosition = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
        vs_screenSpaceDepth = initialPosition.w;
        crossCorner = z_normalization(vec4(cameraViewProjectionMatrix * 
                            dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));
        
        // Testing size for rectangular viewport:
        vec2 halfViewSize = vec2(screenSize.x, screenSize.y) * 0.5f;
        vec2 topRight = crossCorner.xy/crossCorner.w;
        vec2 bottomLeft = initialPosition.xy/initialPosition.w;
        
        // width and height
        vec2 sizes = abs(halfViewSize * (topRight - bottomLeft));
        
        if (enabledRectSizeControl && ((sizes.y > maxBillboardSize) ||
            (sizes.x > maxBillboardSize))) {        
            //Set maximum size as Carter's instructions
            float correctionScale = 
                sizes.y > maxBillboardSize ? maxBillboardSize / sizes.y :
                                             maxBillboardSize / sizes.x;
            
            scaledRight *= correctionScale;
            scaledUp    *= correctionScale;
        
        } else {            
            if (sizes.x < 2.0f * minBillboardSize) {
                float maxVar = 2.0f * minBillboardSize;
                float minVar = minBillboardSize;
                float var    = (sizes.y + sizes.x);    
                ta = ( (var - minVar)/(maxVar - minVar) );
                if (ta == 0.0f)
                    return;
            }
        }
    }
    
    initialPosition = z_normalization(vec4(cameraViewProjectionMatrix *
                        dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));
    vs_screenSpaceDepth = initialPosition.w;                        
    secondPosition = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
    crossCorner = z_normalization(vec4(cameraViewProjectionMatrix * 
                        dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));
    thirdPosition = z_normalization(vec4(cameraViewProjectionMatrix *
                        dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));


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