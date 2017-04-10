/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

uniform mat4 modelViewProjection;
uniform sampler3D volumeTexture;
uniform vec3 domainMins;
uniform vec3 domainDiffs;
// uniform vec3 bMins;
// uniform vec3 bMaxs;
// uniform vec3 bDiff;

in vec4 vs_color[];

out vec4 gs_color;
out float gs_depth;

layout(points) in;
layout(line_strip, max_vertices = 8) out;

#include "PowerScaling/powerScaling_vs.hglsl"


void main() {
    vec3 normCoords = (gl_in[0].gl_Position.xyz - domainMins) / (domainDiffs);
    vec4 interpTexVal = texture(volumeTexture, normCoords);

    vec4 position_meters = vec4(gl_in[0].gl_Position.xyz + vec3(0,0,999999.0)/* interpTexVal.xyz */, 1.0);
    position_meters.xyz = position_meters.xyz * 6371000.0;
    gl_Position = z_normalization(modelViewProjection * position_meters);
    gs_depth = gl_Position.w;
    gs_color = vec4(0.0,1.0,1.0,1.0);
    EmitVertex();

    // normCoords = (gl_in[0].gl_Position.xyz - domainMins) / (domainDiffs);
    // interpTexVal = texture(volumeTexture, normCoords);
    position_meters = vec4(gl_in[0].gl_Position.xyz,1.0);// + interpTexVal;
    position_meters.xyz = position_meters.xyz * 6371000.0;
    gl_Position = z_normalization(modelViewProjection * position_meters);
    // gl_Position = z_normalization(modelViewProjection * (gl_in[0].gl_Position *  + vec4(0.0, 0.0, 0.0, 0.0)));
    gs_depth = gl_Position.w;
    gs_color = vs_color[0];
    EmitVertex();

    position_meters = vec4(gl_in[0].gl_Position.xyz + vec3(999999.0,0,0)/* interpTexVal.xyz */, 1.0);
    position_meters.xyz = position_meters.xyz * 6371000.0;
    gl_Position = z_normalization(modelViewProjection * position_meters);
    gs_depth = gl_Position.w;
    gs_color = vec4(1.0,1.0,0.0,1.0);
    EmitVertex();

        position_meters = vec4(gl_in[0].gl_Position.xyz,1.0);// + interpTexVal;
    position_meters.xyz = position_meters.xyz * 6371000.0;
    gl_Position = z_normalization(modelViewProjection * position_meters);
    // gl_Position = z_normalization(modelViewProjection * (gl_in[0].gl_Position *  + vec4(0.0, 0.0, 0.0, 0.0)));
    gs_depth = gl_Position.w;
    gs_color = vs_color[0];
    EmitVertex();

    position_meters = vec4(gl_in[0].gl_Position.xyz + vec3(999999.0,0,0)/* interpTexVal.xyz */, 1.0);
    position_meters.xyz = position_meters.xyz * 6371000.0;
    gl_Position = z_normalization(modelViewProjection * position_meters);
    gs_depth = gl_Position.w;
    gs_color = vec4(1.0,1.0,0.0,1.0);
    EmitVertex();


    EndPrimitive();
}
