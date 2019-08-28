/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

uniform dvec3 eyePosition;
uniform dvec3 cameraUp;
uniform dmat4 cameraViewProjectionMatrix;
uniform dmat4 modelMatrix;

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in vec4 vs_gPosition[];
in vec3 vs_color[];

out vec4 vs_position;
out vec2 psfCoords;
flat out vec3 ge_color;
flat out float ge_screenSpaceDepth;

const double PARSEC = 3.08567756E16;

void main() {
    vs_position = gl_in[0].gl_Position; // in object space
    ge_color = vs_color[0];

    double scaleMultiply = 8.0;

    dvec4 dpos = dvec4(vs_position);
    dpos.xyz *= scaleMultiply;
    dpos = modelMatrix * dpos;
    dpos /= PARSEC;
    //It lies about 8 kpc from the center on what is known as the Orion Arm of the Milky Way
    dpos.x += 8000;

    scaleMultiply *= 4.0;
    dvec3 scaledRight    = dvec3(0.0);
    dvec3 scaledUp       = dvec3(0.0);
    vec4 bottomLeftVertex, bottomRightVertex, topLeftVertex, topRightVertex;

    dvec3 normal   = normalize(eyePosition - dpos.xyz);
    dvec3 newRight = normalize(cross(cameraUp, normal));
    dvec3 newUp    = cross(normal, newRight);
    scaledRight    = scaleMultiply * newRight;
    scaledUp       = scaleMultiply * newUp;

    bottomLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix *
                        dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w)));

    //dvec4 dposCamera = cameraViewProjectionMatrix * dpos;
    //ge_screenSpaceDepth = float(length(eyePosition - dposCamera.xyz)/PARSEC);
    ge_screenSpaceDepth  = bottomLeftVertex.w;

    topRightVertex = z_normalization(vec4(cameraViewProjectionMatrix *
                            dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w)));

    bottomRightVertex = z_normalization(vec4(cameraViewProjectionMatrix *
                    dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w)));
    topLeftVertex = z_normalization(vec4(cameraViewProjectionMatrix *
                        dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w)));

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
