/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
THE SOFTWARE.
*/
#version 430

uniform mat4 ViewProjection;
uniform mat4 ModelTransform;
uniform vec4 campos;
uniform mat4 camrot;
uniform vec2 scaling;
uniform vec4 objpos;
uniform float time;
uniform sampler2D texture1;
uniform sampler2D texture2;
uniform sampler2D texture3;
uniform float TessLevel;
uniform bool Wireframe;
uniform bool Lightsource;
uniform bool UseTexture;

layout(location = 0) in vec4 in_position;
//in vec3 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

out vec2 vs_st;
out vec3 vs_stp;
out vec4 vs_normal;
out vec4 vs_position;

const float k = 10.0;
const float dgr_to_rad = 0.0174532925;

vec4 psc_addition(vec4 v1, vec4 v2) {
	float ds = v2.w - v1.w;
	if(ds >= 0) {
		float p = pow(k,-ds);
		return vec4(v1.x*p + v2.x, v1.y*p + v2.y, v1.z*p + v2.z, v2.w);
	} else {
		float p = pow(k,ds);
		return vec4(v1.x + v2.x*p, v1.y + v2.y*p, v1.z + v2.z*p, v1.w);
	}
}

vec4 psc_to_meter(vec4 v1, vec2 v2) {
	float factor = v2.x * pow(k,v2.y + v1.w);
	return vec4(v1.xyz * factor, 1.0);
}

vec4 psc_scaling(vec4 v1, vec2 v2) {
	float ds = v2.y - v1.w;
	if(ds >= 0) {
		return vec4(v1.xyz * v2.x * pow(k,v1.w), v2.y);
	} else {
		return vec4(v1.xyz * v2.x * pow(k,v2.y), v1.w);
	}
}

void main()
{
	// set variables
	vs_st = in_st;
	//vs_stp = in_position.xyz;
	vs_normal = normalize(ModelTransform * vec4(in_normal,0));
	
	// fetch model and view translation
	//vec4 vertex_translate = ModelTransform[3];
	
	// rotate and scale vertex with model transform and add the translation
	vec3 local_vertex_pos = mat3(ModelTransform) * in_position.xyz;
	//vec4 lvp = ModelTransform * in_position;
	
	// PSC addition; local vertex position and the object power scaled world position
	vs_position = psc_addition(vec4(local_vertex_pos,in_position.w),objpos);
	//vs_position = psc_addition(lvp,objpos);
	
	// PSC addition; rotated and viewscaled vertex and the cmaeras negative position
	vs_position = psc_addition(vs_position,vec4(-campos.xyz,campos.w));
	
	// rotate the camera
	local_vertex_pos =  mat3(camrot) * vs_position.xyz;
	vs_position = vec4(local_vertex_pos, vs_position.w);
	//vs_position =  camrot* vs_position;

	// rescales the scene to fit inside the view frustum
	// is set from the main program, but these are decent values
	// scaling = vec2(1.0, -8.0);

	// project using the rescaled coordinates,
	//vec4 vs_position_rescaled = psc_scaling(vs_position, scaling);
	vec4 vs_position_rescaled = psc_to_meter(vs_position, scaling);
	//vs_position = vs_position_rescaled;


	// project the position to view space
	gl_Position =  ViewProjection * vs_position_rescaled;
}