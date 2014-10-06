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

// Select type of depth calculations
#define 	PSCDEPTH 		1
#define 	ZDEPTH 			2
#define 	ZTYPE 			ZDEPTH

// Maximum number of fragments
#ifdef MAX_LAYERS
#define 	MAX_FRAGMENTS 	MAX_LAYERS
#else
#define 	MAX_FRAGMENTS 	16 				// The size of the local fragment list
#endif
// #define 	VISUALIZE_TRANSFERFUNCTIONS		// 
#define 	USE_JITTERING					//
// #define 	USE_COLORNORMALIZATION			//

// If you need to render a volume box but not sample the volume (debug purpose)
// #define 	SKIP_VOLUME_0
// #define 	SKIP_VOLUME_1
// #define 	SKIP_VOLUME_2
// #define 	SKIP_VOLUME_3

// constants
const float stepSize = 	0.01;
const float samplingRate = 1.0;
uniform float ALPHA_LIMIT = 0.99;


// Math defintions
#define 	M_E   		2.7182818284590452354
#define 	M_LOG2E   	1.4426950408889634074 	/* log_2 e */
#define 	M_LOG10E   	0.43429448190325182765 	/* log_10 e */
#define 	M_LN2   	0.69314718055994530942 	/* log_e 2 */
#define 	M_LN10   	2.30258509299404568402 	/* log_e 10 */
#define 	M_PI   		3.14159265358979323846 	/* pi */
#define 	M_PI_2   	1.57079632679489661923 	/* pi/2 */
#define 	M_PI_4   	0.78539816339744830962 	/* pi/4 */
#define 	M_1_PI   	0.31830988618379067154 	/* 1/pi */
#define 	M_2_PI   	0.63661977236758134308 	/* 2/pi */
#define 	M_2_SQRTPI	1.12837916709551257390 	/* 2/sqrt(pi) */
#define 	M_SQRT2 	1.41421356237309504880 	/* sqrt(2) */
#define 	M_SQRT1_2   0.70710678118654752440 	/* 1/sqrt(2) */
#define		M_SQRT1_3	0.57735026919			/* 1/sqrt(3) */

in vec2 texCoord;
out vec4 out_color;

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

#if MAX_VOLUMES > 0
	vec3 volume_direction[MAX_VOLUMES];
	float volume_length[MAX_VOLUMES];
	vec3 volume_position[MAX_VOLUMES];
	int volumes_in_fragment[MAX_VOLUMES];
	int volume_count = 0;

	#if ZTYPE == ZDEPTH
		vec2 volume_zlength[MAX_VOLUMES];
	#elif ZTYPE == PSCDEPTH
		float volume_zlength[MAX_VOLUMES];
	#endif
#endif
#include "abufferSort.hglsl"

// ================================================================================
// Helper functions functions
// ================================================================================
vec3 CartesianToSpherical(vec3 _cartesian) {
  // Put cartesian in [-1..1] range first
  vec3 cartesian = vec3(-1.0,-1.0,-1.0) + _cartesian * 2.0f;
  
  float r = length(cartesian);
  float theta, phi;

  if (r == 0.0) {
    theta = phi = 0.0;
  } else {
    theta = acos(cartesian.z/r) / M_PI;
    phi = (M_PI + atan(cartesian.y, cartesian.x)) / (2.0*M_PI );
  }
  r *= M_SQRT1_3;
  // r = r / sqrt(3.0f);
  
  // Sampler ignores w component
  return vec3(r, theta, phi);
}

vec4 blend(vec4 src, vec4 dst) {
	vec4 o;
	o.a = src.a + dst.a * (1.0f - src.a);
	o.rgb = (src.rgb*src.a + dst.rgb*dst.a* (1.0f - src.a));
	return o;
	//return mix(src, dst, dst.a*(1.0f - src.a));
}

void blendStep(inout vec4 dst, in vec4 src, in float stepSize) {
    src.a = 1.0 - pow(1.0 - src.a, stepSize );
    // src.a = 1.0 -(1.0 - src.a*stepSize);
    dst.rgb = dst.rgb + (1.0 - dst.a) * src.a * src.rgb;
    dst.a = dst.a + (1.0 -dst.a) * src.a;
}

float volumeRaycastingDistance(in int id, in ABufferStruct_t startFrag, in ABufferStruct_t endFrag) {
#if MAX_VOLUMES > 0
#if ZTYPE == ZDEPTH
	const float S1 = volume_zlength[id].x;
	const float S2 = volume_zlength[id].y;
	const float L = S1 - S2;
	// const float z1 = globz(_z_(startFrag));
	// const float z2 = globz(_z_(endFrag));
	const float z1 = _z_(startFrag);
	const float z2 = _z_(endFrag);
	return ((z1 - S1) / L - (z2 - S1) / L) * volume_length[id];
#elif ZTYPE == PSCDEPTH
	const float L = volume_zlength[id];
	const vec4 p1 = _pos_(startFrag);
	const vec4 p2 = _pos_(endFrag);
	const float dist = pscLength(p1, p2);
	// const float z1 = _z_(startFrag);
	// const float z2 = _z_(endFrag);
	return (dist / L) * volume_length[id];
#endif
#else
	return 0.f;
#endif
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

		ABufferStruct_t startFrag = fragments[i];
		ABufferStruct_t endFrag = fragments[i+1];
		int type = int(_type_(startFrag));
		
		if(type == 0) {
			//blendStep(final_color, _col_(startFrag), stepSize);
			final_color = blend(final_color, _col_(startFrag));
		} else {
			currentVolumeBitmask = currentVolumeBitmask ^ (1 << (type-1));
		}


#if MAX_VOLUMES > 0
		if(currentVolumeBitmask > 0) {


			int volID;
			float myMaxSteps = 0.0000001;
			if(volume_count > 1) {
				for(int v = 0; v < volume_count; ++v) {
					int vol = volumes_in_fragment[v];
					float l = volumeRaycastingDistance(vol, startFrag, endFrag);
					myMaxSteps = max(myMaxSteps, l/volumeStepSizeOriginal[vol]);
				}

				for(int v = 0; v < volume_count; ++v) {
					int vol = volumes_in_fragment[v];
					float l = volumeRaycastingDistance(vol, startFrag, endFrag);
					float aaa = l/myMaxSteps;
					volumeStepSize[vol] = aaa;
					volID = vol;
				}
			} else {
				volID = type -1;
			}

			float l = volumeRaycastingDistance(volID, startFrag, endFrag);
			int max_iterations = int(l / volumeStepSize[volID]);

			// TransferFunction
			vec4 color = vec4(0);
			for(int k = 0; k < max_iterations && final_color.a < ALPHA_LIMIT && k < LOOP_LIMIT; ++k) {

#pragma openspace insert SAMPLERCALLS


			}
			
		}
#endif

		

		
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
#ifdef VISUALIZE_TRANSFERFUNCTIONS
#pragma openspace insert TRANSFERFUNC
#endif

	// if(frag_count == 0) {
	// 	final_color = vec4(0.5,0.5,0.5,1.0);
	// } else if(frag_count == 1) {
	// 	final_color = vec4(1.0,0.0,0.0,1.0);
	// } else if(frag_count == 2) {
	// 	final_color = vec4(0.0,1.0,0.0,1.0);
	// 	// final_color = vec4(volume_direction[0],1.0);
	// } else if(frag_count == 3) {
	// 	final_color = vec4(0.0,0.0,1.0,1.0);
	// 	// final_color = vec4(volume_direction[0],1.0);
	// } else if(frag_count == 4) {
	// 	final_color = vec4(1.0,1.0,0.0,1.0);
	// 	// final_color = vec4(volume_direction[0],1.0);
	// } else {
	// 	final_color = vec4(1.0,1.0,1.0,1.0);
	// }

	// if(frag_count > 12) {
	// 	final_color = vec4(1.0,0.0,1.0,1.0);
	// }


#ifdef USE_COLORNORMALIZATION
	final_color.rgb = final_color.rgb * final_color.a;
	final_color.a = 1.0;
#endif

	return final_color;

}

// ================================================================================
// Main function
// ================================================================================
void main() {
    out_color = vec4(texCoord,0.0,1.0);
    int frag_count = build_local_fragments_list();
    sort_fragments_list(frag_count);
    out_color = calculate_final_color(frag_count);
}

// ================================================================================
// 	The samplers implementations
// ================================================================================
#pragma openspace insert SAMPLERS



