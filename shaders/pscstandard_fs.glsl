/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#version 430

uniform mat4 ViewProjection;
uniform mat4 ModelTransform;
uniform vec4 campos;
uniform vec4 objpos;
uniform float time;
uniform sampler2D texture1;

vec3 light_position = vec3(40.0, 20.0, 100.0);

in vec2 vs_st;
//in vec3 vs_stp;
in vec4 vs_normal;
in vec4 vs_position;

out vec4 diffuse;

const float k = 10.0;

#include "ABuffer/abufferStruct.hglsl"
#include "ABuffer/abufferAddToBuffer.hglsl"


vec4 psc_normlization(vec4 invec) {
	
	float xymax = max(invec.x,invec.y);

	if(invec.z > 0.0f || invec.z < 0.0f) {
		return invec / abs(invec.z);
	} else if (xymax != 0.0f) {
		return invec / xymax;
	} else {
		return invec / -.0;
	}
}

void main()
{

	// Observable universe is 10^27m, setting the far value to extremely high, aka 30!! ERMAHGERD!
	float s_far			= 27.0; //= gl_DepthRange.far;	// 40
	float s_farcutoff	= 12.0;
	float s_nearcutoff	= 7.0;
	float s_near		= 0.0f;// gl_DepthRange.near;	// 0.1
	float depth;

	// the value can be normalized to 1
	
	vec4 p = vs_position;
	if(vs_position.w <= 0.5) {
		//depth = abs(vs_position.z * pow(10, vs_position.w)) / pow(k,s_far);
		depth = (vs_position.w+log(abs(vs_position.z)))/pow(k, vs_position.w);
	} else if(vs_position.w < 3.0) {
		depth = vs_position.w+log(abs(vs_position.z))/pow(k, vs_position.w);
	} else {
		depth = vs_position.w+log(abs(vs_position.z));
	}
	

	// DEBUG
	float depth_orig = depth;
	float x = 0.0f;
	float cutoffs = 0.0;
	float orig_z = vs_position.z;
	
	// calculate a normalized depth [0.0 1.0]
	if((depth > s_near && depth <= s_nearcutoff) || (depth > s_farcutoff && depth < s_far)) {

		// completely linear interpolation [s_near .. depth .. s_far]
		depth = (depth - s_near) / (s_far - s_near);

	} else if(depth > s_nearcutoff && depth < s_farcutoff) {

		// DEBUG
		cutoffs = 1.0;

		// interpolate [10^s_nearcutoff .. 10^depth .. 10^s_farcutoff]
		// calculate between 0..1 where the depth is
		x = (pow(10,depth) - pow(10, s_nearcutoff)) / (pow(10,s_farcutoff) - pow(10, s_nearcutoff));

		// remap the depth to the 0..1 depth buffer
		depth = s_nearcutoff + x * (s_farcutoff - s_nearcutoff);
		depth = (depth - s_near) / (s_far - s_near);

	} else {
		// where am I?
		// do I need to be discarded?
		// discard;
	}


	
	
	// set the depth
	gl_FragDepth = depth;
	//gl_FragDepth = 0.5;

	// color 
	diffuse = texture(texture1, vs_st);
    //diffuse = vec4(vs_position.z,0.0, 0.0, 1.0);
    // diffuse = vec4(vs_position.xyz * pow(10, vs_position.w), 1.0);
	//diffuse = vec4(vs_st, 0.0, 1.0);
    //diffuse = vec4(1.0,1.0, 0.0, 1.0);
    //diffuse = vec4(depth*5,0.0, 0.0, 1.0);
    //diffuse = vec4(vs_position.w,0.0, 0.0, 1.0);
	//diffuse = vec4(1.0,0.0,0.0,1.0);

	ABufferStruct_t frag;
	_col_(frag, diffuse);
	_z_(frag, depth);
	_type_(frag, 0);
	addToBuffer(frag);

	/*
	uint index = atomicCounterIncrement(atomicCounterBuffer);
    uint old_head = imageAtomicExchange(anchorPointerTexture, ivec2(gl_FragCoord.xy), index);
	uvec4 item;
	item.x = old_head;
	item.y = packUnorm4x8(diffuse);
	item.z = floatBitsToUint(gl_FragCoord.z / gl_FragCoord.w);
	item.w = 0;
	imageStore(fragmentTexture, int(index), item);
*/
	discard;
}