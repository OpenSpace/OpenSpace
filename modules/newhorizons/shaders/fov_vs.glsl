/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

//uniform mat4 ViewProjection;
//uniform mat4 ModelTransform;
//uniform vec4 etColor;
//uniform vec4 objectVelocity;

layout(location = 0) in vec4 in_point_position;
layout(location = 1) in vec4 in_point_velocity;
layout(location = 2) in vec2 in_point_timeindex;


//out vec4 vs_point_position;
out vec4 vs_point_velocity;

// Uniforms
uniform mat4 modelViewProjectionTransform;

// Outputs
out vec4 vs_positionScreenSpace;

#include "PowerScaling/powerScaling_vs.hglsl"

void main()
{
    vec4 position = vec4(in_point_position.xyz * pow(10, in_point_position.w), 1);
    vec4 positionClipSpace = modelViewProjectionTransform * position;

    // Write output
    vs_positionScreenSpace = z_normalization(positionClipSpace);
    gl_Position = vs_positionScreenSpace;

    vs_point_velocity = in_point_velocity;

/*
    //vs_point_position = objpos;

    // rotate and scale vertex with model transform and add the translation
    vec3 local_vertex_pos = mat3(ModelTransform) * in_point_position.xyz;
    //vec4 lvp = ModelTransform * in_point_position;

    // PSC addition; local vertex position and the object power scaled world position
    vs_point_position = psc_addition(vec4(local_vertex_pos,in_point_position.w),objpos);
    //vs_point_position = psc_addition(lvp,objpos);
    
    // PSC addition; rotated and viewscaled vertex and the cmaeras negative position
    vs_point_position = psc_addition(vs_point_position,vec4(-campos.xyz,campos.w));
    
    // rotate the camera
    local_vertex_pos =  mat3(camrot) * vs_point_position.xyz;
    vs_point_position = vec4(local_vertex_pos, vs_point_position.w);
    //vs_point_position =  camrot* vs_point_position;

    // project using the rescaled coordinates,
    //vec4 vs_point_position_rescaled = psc_scaling(vs_point_position, scaling);
    vec4 vs_point_position_rescaled = psc_to_meter(vs_point_position, scaling);
    //vs_point_position = vs_point_position_rescaled;

    // project the position to view space
    gl_Position =  ViewProjection * vs_point_position_rescaled;
    */
}