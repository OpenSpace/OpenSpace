#version 440
uniform sampler2D texture1;
uniform vec3 Color;

in vec4 vs_position;
in vec2 texCoord;

out vec4 diffuse;

const float k = 10.0;

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

void main(void)
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

	diffuse = vec4(Color, 1); //<--- works, obviously
   // diffuse = texture(texture1, texCoord); //<--- SHOULD work, but doesn't. agh!!
    
}