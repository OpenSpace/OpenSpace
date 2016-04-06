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

uniform mat4 Projection;

uniform vec3 p00;
uniform vec3 p10;
uniform vec3 p01;
uniform vec3 p11;

layout(location = 1) in vec2 in_UV;

out vec4 vs_position;
out vec2 vs_uv;

#include "PowerScaling/powerScaling_vs.hglsl"

void main()
{
	vs_uv = in_UV;

	// Bilinear interpolation
	vec3 p0 = (1 - in_UV.x) * p00 + in_UV.x * p10;
	vec3 p1 = (1 - in_UV.x) * p01 + in_UV.x * p11;
	vec3 p = (1 - in_UV.y) * p0 + in_UV.y * p1;

	vec4 position = Projection * vec4(p, 1);
	gl_Position = z_normalization(position);
}