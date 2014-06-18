#version 430

uniform int SCREEN_WIDTH;
uniform int SCREEN_HEIGHT;
uniform float ALPHA_LIMIT = 0.95;

in vec2 texCoord;
out vec4 color;

// settings
#define LOOP_LIMIT 800
#define MAX_FRAGMENTS 16
#define SHOWFUNC

// GENERATED CONTENT
#pragma openspace insert HEADERS
// END GENERATED CONTENT


const float stepSize = 0.01;
const float samplingRate = 1.0;
float volumeStepSize[] = {
	stepSize
};


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

float globz(float z) {
	return z;
	// return log(2.0*z-1.0);
	// return exp(2.0*z-1.0);
	// const float zNear = 0.1f;
	// const float zFar = 1.0f;
	// //float z_b = texture2D(depthBuffTex, vTexCoord).x;
	// float z_b = z;
 //    float z_n = 2.0 * z_b - 1.0;
 //    float z_e = 2.0 * zNear * zFar / (zFar + zNear - z_n * (zFar - zNear));
	// return z_e;
	//return (2.0 * z - near - far) / (far - near);
}

vec4 calculate_final_color(uint frag_count) {
	// volumeStepSize[volID] = 0.01;
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
			//blendStep(final_color, _col_(startFrag), stepSize);
			final_color = blend(final_color, _col_(startFrag));

		if(bool(currentVolumeBitmask)) {
			int volID = type -1;
			float p = 0.0f;

			//const float l = volume_length[volID];
			const float S1 = volume_zlength[volID].x;
			const float S2 = volume_zlength[volID].y;
			const float L = S1 - S2;
			const float z1 = globz(_z_(startFrag));
			const float z2 = globz(_z_(endFrag));
			// const float z1 = _z_(startFrag);
			// const float z2 = _z_(endFrag);
			const float l = ((z1 - S1) / L - (z2 - S1) / L) * volume_length[volID];
			int max_iterations = int(l / volumeStepSize[volID]);
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
			for(int k = 0; k < max_iterations && k < LOOP_LIMIT; ++k) {
			//while(p < l && iterations < LOOP_LIMIT) {

// GENERATED CONTENT
#pragma openspace insert SAMPLERCALLS
// END GENERATED CONTENT

				//final_color = blend(final_color, color*stepSize);

				volume_position[volID] += volume_direction[volID]*volumeStepSize[volID];
				//p+= stepSize;
				//++iterations;
			}
			
		}

		

		
		//blendGeometry(final_color, startFrag);
		//if(i == maxFrags -1 && _type_(endFrag) == 0)
		//	blendGeometry(final_color, endFrag);
	
		// final_color = blend(final_color, frag_color);
		 if(i == frag_count_1 -1 && _type_(endFrag) == 0)
		 // if(i == frag_count_1 -1)
			final_color = blend(final_color, _col_(endFrag));

	}
	// final_color = vec4(0);
	// int id =3;
	// if(id < frag_count)final_color = blend(final_color, _col_(fragments[id]));

	// if(frag_count > 0)
	// 	final_color = _col_(fragments[0]);
#ifdef SHOWFUNC  
  float showfunc_size = 20.0;
  if(gl_FragCoord.y > float(SCREEN_HEIGHT) - showfunc_size) {
    float normalizedIntensity = gl_FragCoord.x / float(SCREEN_WIDTH) ;
    vec4 tfc = texture(transferFunction, normalizedIntensity);
    final_color = tfc;
  } else if(ceil(gl_FragCoord.y) == float(SCREEN_HEIGHT) - showfunc_size) {
  	const float intensity = 0.4;
  	final_color = vec4(intensity,intensity,intensity,1.0);
  }
#endif

	return final_color;

}


void main() {
    color = vec4(texCoord,0.0,1.0);
    int frag_count = build_local_fragments_list();
    sort_fragments_list(frag_count);



    color = calculate_final_color(frag_count);

    //color = vec4(float(frag_count) / 5.0, 0.0, 0.0, 1.0);
}

// GENERATED CONTENT
#pragma openspace insert SAMPLERS
// END GENERATED CONTENT




