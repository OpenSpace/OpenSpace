#version 430

// uniforms
uniform sampler3D volume;
uniform sampler1D transferFunction;

in vec2 texCoord;
out vec4 color;

// settings
#define ALPHA_LIMIT 0.98
#define LOOP_LIMIT 200
#define MAX_FRAGMENTS 16
#define MAX_VOLUMES 4

#include "abufferStruct.hglsl"
ABufferStruct_t fragments[MAX_FRAGMENTS];
vec3 volume_direction[MAX_VOLUMES];
float volume_length[MAX_VOLUMES];
vec2 volume_zlength[MAX_VOLUMES];
vec3 volume_position[MAX_VOLUMES];

#include "abufferSort.hglsl"


vec4 blend(vec4 src, vec4 dst) {
	vec4 o;
	o.a = src.a + dst.a * (1.0f - src.a);
	o.rgb = (src.rgb*src.a + dst.rgb*dst.a* (1.0f - src.a));
	return o;
	//return mix(src, dst, dst.a*(1.0f - src.a));
}

void blendStep(inout vec4 dst, in vec4 src, in float stepSize) {
    src.a = 1.0 - pow(1.0 - src.a, stepSize );
    dst.rgb = dst.rgb + (1.0 - dst.a) * src.a * src.rgb;
    dst.a = dst.a + (1.0 -dst.a) * src.a;
}
/*
//Color geometry
void blendGeometry(inout vec4 color, ABufferStruct_t frag) {
	//vec4 c;
	//c.rg = unpackHalf2x16(frag.rg);
	//c.ba = unpackHalf2x16(frag.ba);
	vec4 c = _col_(frag);
	c.a = clamp(c.a, 0.0,1.0);
	blend(color, c, 0.01);
	return;
}
*/

vec4 calculate_final_color(uint frag_count) {
	
	int currentVolumeBitmask = 0;
	vec4 final_color = vec4(0);

	if(frag_count == 1 && _type_(fragments[0]) == 0) {
		final_color = blend(final_color, _col_(fragments[0]));
		return final_color;
	}

	int frag_count_1 = int(frag_count)-1;
	for(int i = 0; i < frag_count_1 && final_color.a < ALPHA_LIMIT; i++) {

	//int maxFrags = int(frag_count)-1;
	//for(int i = 0; i < frag_count; i++) {
		ABufferStruct_t startFrag = fragments[i];
		ABufferStruct_t endFrag = fragments[i+1];
		//vec4 frag_color = _col_(startFrag);
		//vec4 position = _pos_(startFrag);
		int type = int(_type_(startFrag));
		currentVolumeBitmask = currentVolumeBitmask ^ (type);
		
		if(type == 0)
			final_color = blend(final_color, _col_(startFrag));

		if(bool(currentVolumeBitmask)) {
			int volID = type -1;
			float p = 0.0f;

			const float stepSize = 0.01;
			//const float l = volume_length[volID];
			const float S1 = volume_zlength[volID].x;
			const float S2 = volume_zlength[volID].y;
			const float L = S1 - S2;
			const float l = (_z_(startFrag) - S1) / L - (_z_(endFrag) - S1) / L;
			const vec3 direction = volume_direction[volID];
			int iterations = 0;
			vec3 position;
			
			// MIP
			// vec4 tmp, color = vec4(0);
			// while(p < l && iterations < LOOP_LIMIT) {
			// 	tmp = texture(volume, volume_position[volID]);
			// 	color = max(color, tmp);
			// 	p+= stepSize;
			// 	volume_position[volID] += direction*stepSize;
			// 	++iterations;
			// }
			// vec4 volume_color = vec4(color.r,color.r,color.r,color.r*2.0);
			// if(volume_color.a < 0.1) volume_color = vec4(0.0,0.0,0.0,0.0);
			// final_color = blend(final_color, volume_color);
			

			// TransferFunction
			vec4 color = vec4(0);
			float intensity;
			while(p < l && iterations < LOOP_LIMIT) {
				intensity = length(texture(volume, volume_position[volID]));
				color = 	texture(transferFunction, intensity);
				blendStep(final_color, color, stepSize);
				//final_color = blend(final_color, color*stepSize);
				p+= stepSize;
				volume_position[volID] += direction*stepSize;
				++iterations;
			}
			
		}

		

		
		//blendGeometry(final_color, startFrag);
		//if(i == maxFrags -1 && _type_(endFrag) == 0)
		//	blendGeometry(final_color, endFrag);
	
		// final_color = blend(final_color, frag_color);
		if(i == frag_count_1 -1 && _type_(endFrag) == 0)
			final_color = blend(final_color, _col_(endFrag));

	}
	// final_color = vec4(0);
	// int id =3;
	// if(id < frag_count)final_color = blend(final_color, _col_(fragments[id]));

	return final_color;

}


void main() {
    color = vec4(texCoord,0.0,1.0);
    int frag_count = build_local_fragments_list();
    sort_fragments_list(frag_count);



    color = calculate_final_color(frag_count);

    //color = vec4(float(frag_count) / 5.0, 0.0, 0.0, 1.0);
}