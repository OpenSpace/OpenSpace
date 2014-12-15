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

#version __CONTEXT__

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec3 in_brightness;
layout(location = 2) in vec3 in_velocity;
layout(location = 3) in float in_speed;

layout(location = 0) out vec4 psc_position;
layout(location = 1) out vec3 vs_brightness;
layout(location = 2) out vec3 vs_velocity;
layout(location = 3) out float vs_speed;
layout(location = 4) out vec4 cam_position;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() { 
	psc_position  = in_position;
	vs_brightness = in_brightness;
	vs_velocity = in_velocity;
	vs_speed = in_speed;
	cam_position  = campos;

	vec4 tmp = in_position;
	vec4 position = pscTransform(tmp, mat4(1.0));
	// psc_position = tmp;
	position = model * view * position;
	// position = ViewProjection * ModelTransform * position;
	// gl_Position =  z_normalization(position);
	gl_Position =  position;
}