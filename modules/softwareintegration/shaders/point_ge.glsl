/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

in float vs_colormapAttributeScalar[];
flat in float vs_linearSizeAttributeScalar[];

flat out float ge_screenSpaceDepth;
out float ta;

out vec2 coords;
out float ge_colormapAttributeScalar;

uniform dvec3 cameraPosition;
uniform vec3 cameraUp;
uniform float size;
uniform int sizeOption;
uniform vec2 screenSize;

uniform dmat4 modelMatrix;
uniform mat4 cameraViewProjectionMatrix;

uniform float linearSizeMin;
uniform float linearSizeMax;
uniform bool linearSizeEnabled;

// FRAGILE
// All of these values have to be synchronized with the values in the optionproperty
const int SizeCompositionOptionUniform = 0;
const int SizeCompositionOptionNonUniform = 1;

const float minBillboardSize = 0.0;
const float maxBillboardSize = 100.0;

void main() {
    ge_colormapAttributeScalar = vs_colormapAttributeScalar[0];

    ta = 1.0;

    vec3 pos = gl_in[0].gl_Position.xyz;

    dvec4 dpos = modelMatrix * dvec4(dvec3(pos.xyz), 1.0);

    float scaleMultiply = 1.0e15 * size;
    
    if (linearSizeEnabled) {
        float interpolatedSizeAtt = 1.0;
        float colormapAttributeScalar = vs_linearSizeAttributeScalar[0];
        if (colormapAttributeScalar < linearSizeMin) {
            interpolatedSizeAtt = 0.0;
        }
        else if (colormapAttributeScalar > linearSizeMax) {
            interpolatedSizeAtt = 1.0;
        }
        else {
            // Linear interpolation
            interpolatedSizeAtt = (colormapAttributeScalar - linearSizeMin) / (linearSizeMax - linearSizeMin);
        }
        interpolatedSizeAtt = mix(1.0, 5.0, interpolatedSizeAtt);

        scaleMultiply *= interpolatedSizeAtt;
    }
    
    vec3 normal = vec3(normalize(cameraPosition - dpos.xyz));
    vec3 newRight = normalize(cross(cameraUp, normal));
    vec3 newUp = cross(normal, newRight);

    if (sizeOption == SizeCompositionOptionUniform) {
        double distCamera = length(cameraPosition - dpos.xyz);
        scaleMultiply *= (float(distCamera) / 1.0e19);
    }

    vec3 scaledRight = scaleMultiply * newRight * 0.5;
    vec3 scaledUp = scaleMultiply * newUp * 0.5;

    {
        scaleMultiply *= exp(size * size);
        
        vec4 initialPosition = z_normalization(
            cameraViewProjectionMatrix * 
            vec4(
                vec3(dpos.xyz) - scaledRight - scaledUp, dpos.w
            )
        );

        ge_screenSpaceDepth = initialPosition.w;
        
        vec4 crossCorner = z_normalization(
            cameraViewProjectionMatrix * 
            vec4(
                vec3(dpos.xyz) + scaledUp + scaledRight, dpos.w
            )
        );
        
        // Testing size for rectangular viewport:
        vec2 halfViewSize = screenSize * 0.5;
        vec2 topRight = crossCorner.xy / crossCorner.w;
        vec2 bottomLeft = initialPosition.xy / initialPosition.w;
        
        // width and height
        vec2 sizes = abs(halfViewSize * (topRight - bottomLeft));
        
        if (length(sizes) > maxBillboardSize) {
            float correctionScale = maxBillboardSize / length(sizes);
            
            scaledRight *= correctionScale;
            scaledUp *= correctionScale;
        }
        else {
            // linear alpha decay
            if (sizes.x < 2.0 * minBillboardSize) {
                float maxVar = 2.0 * minBillboardSize;
                float minVar = minBillboardSize;
                float var = sizes.y + sizes.x;
                ta = (var - minVar) / (maxVar - minVar);
                if (ta == 0.0) {
                    return;
                }
            }
        }
    }

    // Saving one matrix multiplication:
    vec4 dposClip = cameraViewProjectionMatrix * vec4(dpos);
    vec4 scaledRightClip = cameraViewProjectionMatrix * vec4(scaledRight, 0.0);
    vec4 scaledUpClip = cameraViewProjectionMatrix * vec4(scaledUp, 0.0);

    vec4 initialPosition = z_normalization(dposClip - scaledRightClip - scaledUpClip);
    vec4 secondPosition = z_normalization(dposClip + scaledRightClip - scaledUpClip);
    vec4 crossCorner = z_normalization(dposClip + scaledUpClip + scaledRightClip);
    vec4 thirdPosition = z_normalization(dposClip + scaledUpClip - scaledRightClip);

    ge_screenSpaceDepth = initialPosition.w;

    // Build primitive    
    gl_Position = initialPosition;
    coords = vec2(0.0, 0.0);
    EmitVertex();

    gl_Position = secondPosition;
    coords = vec2(1.0, 0.0);
    EmitVertex();

    gl_Position = thirdPosition;
    coords = vec2(0.0, 1.0);
    EmitVertex();

    gl_Position = crossCorner;
    coords = vec2(1.0, 1.0);
    EmitVertex();

    EndPrimitive();
}
