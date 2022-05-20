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

flat out float ge_screenSpaceDepth;
out vec4 ge_positionViewSpace;

out vec2 coords;
out float ge_colormapAttributeScalar;

uniform dvec3 eyePosition;
uniform dvec3 cameraUp;
uniform float size;
uniform int sizeOption;

uniform dmat4 modelMatrix;
uniform dmat4 modelViewTransform;
uniform dmat4 cameraViewProjectionMatrix;

// FRAGILE
// All of these values have to be synchronized with the values in the optionproperty
const int SizeCompositionOptionUniform = 0;
const int SizeCompositionOptionNonUniform = 1;

double scaleForNonUniform() {
    return (size * 500.0) * pow(10.0, 12.5);
}

double scaleForUniform(vec3 pos) {
    double distanceToPoint = length(eyePosition - pos);

    return (distanceToPoint * size) / (1500.0);
}

void main() {
    ge_colormapAttributeScalar = vs_colormapAttributeScalar[0];

    vec3 pos = gl_in[0].gl_Position.xyz;

    double scaleMultiply = 1.0;
    if (sizeOption == SizeCompositionOptionUniform) {
        scaleMultiply = scaleForUniform(pos);
    }
    else if (sizeOption == SizeCompositionOptionNonUniform) {
        scaleMultiply = scaleForNonUniform();
    }

    dvec3 normal = normalize(eyePosition - pos);
    dvec3 newRight = normalize(cross(cameraUp, normal));
    dvec3 newUp = normalize(cross(normal, newRight));
    dvec3 scaledRight = scaleMultiply * newRight;
    dvec3 scaledUp = scaleMultiply * newUp;

    vec4 lowerLeft = z_normalization(
        vec4(cameraViewProjectionMatrix * modelMatrix * dvec4(pos - scaledRight - scaledUp, 1.0))
    );
    
    vec4 upperRight = z_normalization(
        vec4(cameraViewProjectionMatrix * modelMatrix * dvec4(pos + scaledUp + scaledRight, 1.0))
    ); 

    vec4 lowerRight = z_normalization(
        vec4(cameraViewProjectionMatrix * modelMatrix * dvec4(pos + scaledRight - scaledUp, 1.0))
    );
    
    vec4 upperLeft = z_normalization(
        vec4(cameraViewProjectionMatrix * modelMatrix * dvec4(pos + scaledUp - scaledRight, 1.0))
    );

    ge_screenSpaceDepth = lowerLeft.w;
    ge_positionViewSpace = lowerLeft;

    // Build primitive    
    gl_Position = lowerLeft;
    coords = vec2(0.0, 0.0);
    EmitVertex();

    gl_Position = lowerRight;
    coords = vec2(1.0, 0.0);
    EmitVertex();

    gl_Position = upperLeft;
    coords = vec2(0.0, 1.0);
    EmitVertex();

    gl_Position = upperRight;
    coords = vec2(1.0, 1.0);
    EmitVertex();

    EndPrimitive();
}
