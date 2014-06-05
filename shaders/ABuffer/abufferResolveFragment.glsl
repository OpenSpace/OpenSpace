#version 430

// uniforms
uniform sampler3D volume;

in vec2 texCoord;
out vec4 color;

// settings
#define ALPHA_LIMIT 1.0
#define MAX_FRAGMENTS 16
#define MAX_VOLUMES 4

#include "abufferStruct.hglsl"
ABufferStruct_t fragments[MAX_FRAGMENTS];
vec3 volume_direction[MAX_VOLUMES];
float volume_length[MAX_VOLUMES];
vec3 volume_position[MAX_VOLUMES];

#include "abufferSort.hglsl"


vec4 blend(vec4 src, vec4 dst) {
	vec4 o;
	o.a = src.a + dst.a * (1.0f - src.a);
	o.rgb = (src.rgb*src.a + dst.rgb*dst.a* (1.0f - src.a));
	return o;
	//return mix(src, dst, dst.a*(1.0f - src.a));
}
/*
void blend(inout vec4 dst, in vec4 src, in float stepSize) {
    src.a = 1.0 - pow(1.0 - src.a, stepSize );
    dst.rgb = dst.rgb + (1.0 - dst.a) * src.a * src.rgb;
    dst.a = dst.a + (1.0 -dst.a) * src.a;
}

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

	// if(frag_count == 1 && _type_(fragments[0]) == 0) {
	// 	blendGeometry(final_color, fragments[0]);
	// 	return final_color;
	// }

	int frag_count_1 = int(frag_count);
	for(int i = 0; i < frag_count_1 && final_color.a < ALPHA_LIMIT; i++) {

	//int maxFrags = int(frag_count)-1;
	//for(int i = 0; i < frag_count; i++) {
		ABufferStruct_t startFrag = fragments[i];
		//ABufferStruct_t endFrag = fragments[i+1];
		//vec4 frag_color = _col_(startFrag);
		//vec4 position = _pos_(startFrag);
		int type = int(_type_(startFrag));


		if(type != 0) {
			currentVolumeBitmask = currentVolumeBitmask ^ (1 << type);
			if(currentVolumeBitmask != 0) {
				float p = 0.0f;
				const float stepSize = 0.01;
				float l = volume_length[type-1];
				vec3 direction = volume_direction[type-1];
				vec3 position;
				vec4 tmp, color = vec4(0);
				while(p < l) {
					position = volume_position[type-1] + direction*p;
					tmp = texture(volume, position);
					color = max(color, tmp);
					p+= stepSize; 
				}
				vec4 volume_color = vec4(color.r,color.r,color.r,color.r*2.0);
				final_color = blend(final_color, volume_color);
			}
			/*
			const vec3 direction = volume_direction[type-1];
			const float l = pscLength(_pos_(fragments[i+1]), _pos_(fragments[i]));
			const float stepSize = 0.01;
			const float iterationsStop = l / volume_length[type-1];
			//const int iterations = 100;
			float p = 0.0f;
			int maxit = 500;
			int it = 0;
			while(p < iterationsStop && it < maxit) {
				vec4 sample_color = vec4(0.1, 0.0, 0.0, 0.1);
				blend(final_color, sample_color, stepSize);

				p+=stepSize;
				++it;
			} 
			*/
		} else {
			final_color = blend(final_color, _col_(startFrag));
			//blendGeometry(final_color, startFrag);
		}

		
		//blendGeometry(final_color, startFrag);
		//if(i == maxFrags -1 && _type_(endFrag) == 0)
		//	blendGeometry(final_color, endFrag);
	
		// final_color = blend(final_color, frag_color);
		//if(i == frag_count_1 && _type_(endFrag) == 0)
		//	final_color = blend(final_color, _col_(endFrag));

	}

	//int id =2;
	//if(id < frag_count)final_color = blend(final_color, _col_(fragments[id]));

	return final_color;

}


void main() {
    color = vec4(texCoord,0.0,1.0);
    int frag_count = build_local_fragments_list();
    sort_fragments_list(frag_count);



    color = calculate_final_color(frag_count);

    //color = vec4(float(frag_count) / 5.0, 0.0, 0.0, 1.0);
}