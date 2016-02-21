#version 430 core

in vec4 fColor;
in vec3 vs_position;
in float s;
 
#include "../../../shaders/ABuffer/abufferStruct.hglsl"
#include "../../../shaders/ABuffer/abufferAddToBuffer.hglsl"

const float k 				= 10.0;
const float s_far			= 27.0f; //= gl_DepthRange.far;	// 40
const float s_farcutoff		= 12.0f;
const float s_nearcutoff	= 7.00f;
const float s_near			= 0.00f;// gl_DepthRange.near;	// 0.1

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

float pscDepth(vec4 position) {
	float depth = 0.0f;
	if(position.w <= 0.5) {
		//depth = abs(position.z * pow(10, position.w)) / pow(k,s_far);
		depth = (position.w+log(abs(position.z)))/pow(k, position.w);
	} else if(position.w < 3.0) {
		depth = position.w+log(abs(position.z))/pow(k, position.w);
	} else {
		depth = position.w+log(abs(position.z));
	}
	

	// DEBUG
	float depth_orig = depth;
	float x = 0.0f;
	float cutoffs = 0.0;
	float orig_z = position.z;
	
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

	}
	return depth;
}

void main() {
	vec4 fragColor = fColor;

	//float val = 1.0;
	//fragColor = vec4(val, 0.0, val, val);
	vec4 position = vec4(vs_position,s);
	float depth = pscDepth(position);

	ABufferStruct_t frag = createGeometryFragment(fragColor, position, depth);
	addToBuffer(frag);

	discard;
}