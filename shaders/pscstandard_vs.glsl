/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#version 430

uniform mat4 ViewProjection;
uniform mat4 ModelTransform;

uniform vec4 camdir;

layout(location = 0) in vec4 in_position;
//in vec3 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

out vec2 vs_st;
out vec3 vs_stp;
out vec4 vs_normal;
out vec3 vs_position;
out float s;

#include "PowerScaling/powerScaling_vs.hglsl"

void main()
{
	// set variables
	vs_st = in_st;
	//vs_stp = in_position.xyz;

	// this is wrong for the normal. The normal transform is the transposed inverse of the model transform
	vs_normal = normalize(ModelTransform * vec4(in_normal,0));
	
	vec4 position = pscTransform(in_position, ModelTransform);
	vs_position = position.xyz;
	s = position.w;

	// project the position to view space
	gl_Position =  ViewProjection * position;
}