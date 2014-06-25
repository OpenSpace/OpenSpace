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

#version 430

// ================================================================================
// Settings
// ================================================================================
#pragma openspace insert SETTINGS
#define LOOP_LIMIT 800
#define MAX_FRAGMENTS 16
#define SHOWFUNC
const float stepSize = 0.01;
const float samplingRate = 1.0;

uniform int SCREEN_WIDTH;
uniform int SCREEN_HEIGHT;
uniform float ALPHA_LIMIT = 0.98;

in vec2 texCoord;
out vec4 color;

// ================================================================================ 
// Headers, 
// volume and transferfunctions uniforms
// ================================================================================
#pragma openspace insert HEADERS

// ================================================================================
// The ABuffer specific includes and definitions
// ================================================================================
#include "abufferStruct.hglsl"
ABufferStruct_t fragments[MAX_FRAGMENTS];
vec3 volume_direction[MAX_VOLUMES];
float volume_length[MAX_VOLUMES];
vec2 volume_zlength[MAX_VOLUMES];
// float volume_zlength[MAX_VOLUMES];
vec3 volume_position[MAX_VOLUMES];

#include "abufferSort.hglsl"

// ================================================================================
// Blend functions
// ================================================================================
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

			// const float l = volume_length[volID];
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


			// const float S1 = volume_zlength[volID].x;
			// const float S2 = volume_zlength[volID].y;

			// const float L = volume_zlength[volID];
			// const vec4 p1 = _pos_(startFrag);
			// const vec4 p2 = _pos_(endFrag);
			// const float dist = pscLength(p1, p2);
			// // const float z1 = _z_(startFrag);
			// // const float z2 = _z_(endFrag);
			// const float l = (dist / L) * volume_length[volID];
			// int max_iterations = int(l / volumeStepSize[volID]);
			// int iterations = 0;
			// vec3 position;
			
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

#pragma openspace insert SAMPLERCALLS

				//final_color = blend(final_color, color*stepSize);

				//volume_position[volID] += volume_direction[volID]*volumeStepSize[volID];
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

// ================================================================================
// Transferfunction visualizer
// ================================================================================
#ifdef SHOWFUNC  
	float showfunc_size = 20.0;
	if(gl_FragCoord.y > float(SCREEN_HEIGHT) - showfunc_size) {
		float normalizedIntensity = gl_FragCoord.x / float(SCREEN_WIDTH) ;
		vec4 tfc = texture(transferFunction1, normalizedIntensity);
		final_color = tfc;
	} else if(ceil(gl_FragCoord.y) == float(SCREEN_HEIGHT) - showfunc_size) {
		const float intensity = 0.4;
		final_color = vec4(intensity,intensity,intensity,1.0);
	}
#endif

	// if(frag_count == 1) {
	// 	final_color = vec4(0.0,0.0,1.0,1.0);
	// } else if(frag_count == 2) {
	// 	final_color = vec4(volume_direction[0],1.0);
	// } else {
	// 	final_color = vec4(1.0,1.0,1.0,1.0);
	// }

	return final_color;

}

// ================================================================================
// Main function
// ================================================================================
void main() {
    color = vec4(texCoord,0.0,1.0);
    int frag_count = build_local_fragments_list();
    sort_fragments_list(frag_count);
    color = calculate_final_color(frag_count);
}

// ================================================================================
// 	The samplers implementations
// ================================================================================
#pragma openspace insert SAMPLERS




