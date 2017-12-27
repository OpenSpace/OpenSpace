/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2017                                                             *
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

#include "PowerScaling/powerScaling_vs.hglsl"

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

out vec2 vs_st;
out vec4 vs_normal;
out vec4 vs_position;
out vec4 vs_posWorld;
out vec4 vs_gPosition;
out vec3 vs_gNormal;

uniform mat4 ViewProjection;
uniform mat4 ModelTransform;


void main() {
    vs_st = in_st;
    vs_position = in_position;
    vec4 tmp = in_position;

    // this is wrong for the normal. The normal transform is the transposed inverse of the model transform
    vs_normal = normalize(ModelTransform * vec4(in_normal,0));

    // G-Buffer
    vs_gNormal = in_normal;

    // The tmp is now in the OS eye space (camera rig space)
    // and wrtitten using PSC coords.
    // position is in the same space, i.e., OS eye space but 
    // in meters (no PSC coords).    
    vec4 position = pscTransform(tmp, ModelTransform);

    // G-Buffer
    vs_gPosition = position;

    vec3 local_vertex_pos = mat3(ModelTransform) * in_position.xyz;
    vec4 vP = psc_addition(vec4(local_vertex_pos, in_position.w), objpos);
    vs_posWorld = vec4(vP.xyz * pow(10, vP.w), 1.0);
    
    vs_position = tmp;
    // Now is transforming from view position to SGCT projection
    // coordinates.
    position = ViewProjection * position;
    gl_Position =  z_normalization(position);
}
