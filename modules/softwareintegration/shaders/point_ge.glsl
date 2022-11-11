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

in vec3 vs_velocity[];

in float vs_colormapAttributeScalar[];
flat in float vs_linearSizeAttributeScalar[];

flat out float ge_screenSpaceDepth;
out float ta;
out vec4 ge_positionViewSpace;

out vec2 coords;
out float ge_colormapAttributeScalar;

out vec3 ge_velocity;

uniform dvec3 eyePosition;
uniform dvec3 cameraUp;
uniform float size;
uniform int sizeOption;
uniform ivec2 screenSize;

uniform dmat4 modelMatrix;
uniform dmat4 cameraViewProjectionMatrix;

uniform float linearSizeMin;
uniform float linearSizeMax;
uniform bool linearSizeEnabled;

uniform bool motionEnabled;

// FRAGILE
// All of these values have to be synchronized with the values in the optionproperty
const int SizeCompositionOptionUniform = 0;
const int SizeCompositionOptionNonUniform = 1;

const float maxSize = 1000.0;

dvec4 z_normalization_d(dvec4 v_in) {
    dvec4 v_out = v_in;
    v_out.z = 0;
    return v_out;
}

dvec4[4] getCorners(dvec4 center, dvec3 right, dvec3 up) {
    // Saving matrix multiplication:
    dvec4 centerClip = cameraViewProjectionMatrix * center;
    dvec4 rightClip = cameraViewProjectionMatrix * dvec4(right, 0.0);
    dvec4 upClip = cameraViewProjectionMatrix * dvec4(up, 0.0);

    dvec4 lowerLeft = z_normalization_d(centerClip - rightClip - upClip);
    dvec4 lowerRight = z_normalization_d(centerClip + rightClip - upClip);
    dvec4 upperLeft = z_normalization_d(centerClip + upClip - rightClip);
    dvec4 upperRight = z_normalization_d(centerClip + upClip + rightClip);

    return dvec4[4](lowerLeft, lowerRight, upperLeft, upperRight);
}

dvec4[2] getTwoCorners(dvec4 center, dvec3 right, dvec3 up) {
    // Saving matrix multiplication:
    dvec4 centerClip = cameraViewProjectionMatrix * center;
    dvec4 rightClip = cameraViewProjectionMatrix * dvec4(right, 0.0);
    dvec4 upClip = cameraViewProjectionMatrix * dvec4(up, 0.0);

    dvec4 lowerLeft = z_normalization_d(centerClip - rightClip - upClip);
    dvec4 upperRight = z_normalization_d(centerClip + upClip + rightClip);

    return dvec4[2](lowerLeft, upperRight);
}

void main() {
    ge_colormapAttributeScalar = vs_colormapAttributeScalar[0];
    ge_velocity = vs_velocity[0];
    
    ta = 1.0;

    vec3 pos = gl_in[0].gl_Position.xyz;

    dvec4 modelPos = modelMatrix * dvec4(pos, 1.0);

    double scaleMultiply = 1.0e17 * size;

    if (linearSizeEnabled) {
        float interpolatedSizeAtt = 1.0;
        float linearSizeAttributeScalar = vs_linearSizeAttributeScalar[0];
        if (linearSizeAttributeScalar < linearSizeMin) {
            interpolatedSizeAtt = 0.0;
        }
        else if (linearSizeAttributeScalar > linearSizeMax) {
            interpolatedSizeAtt = 1.0;
        }
        else {
            // Linear interpolation
            interpolatedSizeAtt = (linearSizeAttributeScalar - linearSizeMin) / (linearSizeMax - linearSizeMin);
        }
        interpolatedSizeAtt = mix(1.0, 5.0, interpolatedSizeAtt);

        scaleMultiply *= interpolatedSizeAtt;
    }

    dvec3 normal = normalize(eyePosition - modelPos.xyz);
    dvec3 newRight = normalize(cross(cameraUp, normal));
    dvec3 newUp = cross(normal, newRight);

    if (sizeOption == SizeCompositionOptionUniform) {
        double distanceToPoint = length(eyePosition - modelPos.xyz);
        scaleMultiply *= distanceToPoint / 1.0e20;
    }

    dvec3 scaledRight = scaleMultiply * newRight * 0.5;
    dvec3 scaledUp = scaleMultiply * newUp * 0.5;

    ge_screenSpaceDepth = float(modelPos.w);
    ge_positionViewSpace = vec4(modelPos);
    
    {
        dvec4 corners[2] = getTwoCorners(modelPos, scaledRight, scaledUp);

        // Testing size for rectangular viewport:
        dvec2 halfViewSize = screenSize * 0.5;
        dvec2 topRight = corners[1].xy / corners[1].w;
        dvec2 bottomLeft = corners[0].xy / corners[0].w;
        
        // width and height
        dvec2 sizes = abs(halfViewSize * (topRight - bottomLeft));
        
        if (length(sizes) > maxSize) {
            double correctionScale = maxSize / length(sizes);
            scaledRight *= correctionScale;
            scaledUp *= correctionScale;
        }
    }

    dvec4 corners[4] = getCorners(modelPos, scaledRight, scaledUp);

    // Build primitive    
    gl_Position = vec4(corners[0]);
    coords = vec2(0.0, 0.0);
    EmitVertex();

    gl_Position = vec4(corners[1]);
    coords = vec2(1.0, 0.0);
    EmitVertex();

    gl_Position = vec4(corners[2]);
    coords = vec2(0.0, 1.0);
    EmitVertex();

    gl_Position = vec4(corners[3]);
    coords = vec2(1.0, 1.0);
    EmitVertex();

    EndPrimitive();
}

// float scaleForNonUniform() {
//     return (size * 500.0) * pow(10.0, 12.5);
// }

// float scaleForUniform(vec3 pos) {
//     float distanceToPoint = length(eyePosition - pos);
//     return (distanceToPoint * size) / (1500.0);
// }

// void main() {
//     ge_colormapAttributeScalar = vs_colormapAttributeScalar[0];

//     vec3 pos = gl_in[0].gl_Position.xyz;

//     float scaleMultiply = 1.0;
//     if (sizeOption == SizeCompositionOptionUniform) {
//         scaleMultiply = scaleForUniform(pos);
//     }
//     else if (sizeOption == SizeCompositionOptionNonUniform) {
//         scaleMultiply = scaleForNonUniform();
//     }

//     vec3 normal = normalize(eyePosition - pos);
//     vec3 newRight = normalize(cross(cameraUp, normal));
//     vec3 newUp = normalize(cross(normal, newRight));
//     vec3 scaledRight = scaleMultiply * newRight;
//     vec3 scaledUp = scaleMultiply * newUp;

//     vec4 lowerLeft = z_normalization(
//         vec4(cameraViewProjectionMatrix * modelMatrix * vec4(pos - scaledRight - scaledUp, 1.0))
//     );
    
//     vec4 upperRight = z_normalization(
//         vec4(cameraViewProjectionMatrix * modelMatrix * vec4(pos + scaledUp + scaledRight, 1.0))
//     ); 

//     vec4 lowerRight = z_normalization(
//         vec4(cameraViewProjectionMatrix * modelMatrix * vec4(pos + scaledRight - scaledUp, 1.0))
//     );
    
//     vec4 upperLeft = z_normalization(
//         vec4(cameraViewProjectionMatrix * modelMatrix * vec4(pos + scaledUp - scaledRight, 1.0))
//     );

//     ge_screenSpaceDepth = lowerLeft.w;
//     ge_positionViewSpace = lowerLeft;

//     // Build primitive    
//     gl_Position = lowerLeft;
//     coords = vec2(0.0, 0.0);
//     EmitVertex();

//     gl_Position = lowerRight;
//     coords = vec2(1.0, 0.0);
//     EmitVertex();

//     gl_Position = upperLeft;
//     coords = vec2(0.0, 1.0);
//     EmitVertex();

//     gl_Position = upperRight;
//     coords = vec2(1.0, 1.0);
//     EmitVertex();

//     EndPrimitive();
// }
