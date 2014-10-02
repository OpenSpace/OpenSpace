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
#version 400 core

uniform mat4 ViewProjection;
uniform mat4 ModelTransform;
uniform vec4 campos;
uniform mat4 camrot;
uniform vec2 scaling;
uniform vec4 objpos;
//uniform vec4 etColor;
uniform vec4 objectVelocity;

layout(location = 0) in vec4 in_point_position;
layout(location = 1) in vec4 in_point_velocity;
layout(location = 2) in vec2 in_point_timeindex;


out vec4 vs_point_position;
out vec4 vs_point_velocity;

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
	//vs_stp = in_point_position.xyz;
	//vs_normal = normalize(ModelTransform*vec4(1)); // <-- not really using right now. change later.

	// add life back to the thing.
	/*
	vec3 vel_1 = psc_to_meter(in_point_velocity, vec2(3)).xyz;
	vec3 vel_2 = psc_to_meter(objectVelocity, vec2(3)).xyz;
	float a = 0;
	
	vec3 dist = (objpos-in_point_position).xyz; 
	
	if( dot(dist, vel_2) > 0.f){
		 a = 2.f*dot(vel_1,vel_2)/(length(vel_1)*length(vel_2));
	}*/
	vs_point_velocity = in_point_velocity;

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
}