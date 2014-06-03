#version 430

// uniforms

in vec2 texCoord;
out vec4 color;

// settings
#define ALPHA_LIMIT 0.51
#define MAX_FRAGMENTS 16
#define MAX_VOLUMES 4

#include "abufferStruct.hglsl"
ABufferStruct_t fragments[MAX_FRAGMENTS];

vec3 volume_direction[MAX_VOLUMES];
vec3 volume_position[MAX_VOLUMES];

#include "abufferSort.hglsl"


vec4 blend(vec4 src, vec4 dst) {
	vec4 o;
	o.a = src.a + dst.a * (1.0f - src.a);
	o.rgb = (src.rgb*src.a + dst.rgb*dst.a* (1.0f - src.a));
	return o;
	//return mix(src, dst, dst.a*(1.0f - src.a));
}

vec3 direction(vec4 v1, vec4 v2) {
	const float k = 10.0;
	float ds = v2.w - v1.w;
	if(ds >= 0) {
		float p = pow(k,-ds);
		return vec4(v1.x*p - v2.x, v1.y*p - v2.y, v1.z*p - v2.z, v2.w).xyz;
	} else {
		float p = pow(k,ds);
		return vec4(v1.x - v2.x*p, v1.y - v2.y*p, v1.z - v2.z*p, v1.w).xyz;
	}
}

vec4 calculate_final_color(uint frag_count) {
	
	int currentVolumeBitmask = 0;
	vec4 final_color = vec4(0);
	for(uint i = 0; i < frag_count && final_color.a < ALPHA_LIMIT; i++) {
		ABufferStruct_t item = fragments[i];
		vec4 frag_color = _col_(item);
		vec4 position = _pos_(item);
		uint type = _type_(item);


		if(currentVolumeBitmask != 0) {
			currentVolumeBitmask = currentVolumeBitmask ^ (1 << type);
		}
		/*
		if(_type_(item) == 1)
*/
			final_color = blend(final_color, frag_color);
			//final_color = blend(frag_color, final_color);

	}

	return final_color;

}


void main() {
    color = vec4(texCoord,0.0,1.0);
    int frag_count = build_local_fragments_list();
    sort_fragments_list(frag_count);



    color = calculate_final_color(frag_count);

    //color = vec4(float(frag_count) / 5.0, 0.0, 0.0, 1.0);
}