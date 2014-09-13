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

#version 430 core

layout(location = 0) in vec4 vertPosition;

uniform mat4 modelViewProjection;
uniform mat4 modelTransform;
uniform vec4 campos;
uniform mat4 camrot;
uniform vec2 scaling;
uniform vec4 objpos;

out vec3 vPosition;
out vec3 worldPosition;
out float s;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {

	//vs_st = in_st;
	//vs_stp = in_position.xyz;

	vPosition = vertPosition.xyz;

	// this is wrong for the normal. The normal transform is the transposed inverse of the model transform
	//vs_normal = normalize(modelTransform * vec4(in_normal,0));
	
	vec4 position = pscTransform(vertPosition, campos, scaling, modelTransform);
	worldPosition = position.xyz;
	s = position.w;

	// project the position to view space
	gl_Position =  modelViewProjection * position;

	// vPosition = vertPosition.xyz;
	// worldPosition = (modelTransform *vec4(vPosition, 1.0)).xyz;
	// gl_Position = modelViewProjection  *vec4(worldPosition, 1.0);
}