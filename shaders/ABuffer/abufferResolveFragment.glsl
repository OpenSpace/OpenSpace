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
#define MAX_FRAGMENTS 16

// #define SHOWFUNC
// #define JITTERING
#define SAMPLEFIRSTVOLUME
// #define COLORNORMALIZATION

#define PSCDEPTH 1
#define ZDEPTH 2
#define ZTYPE ZDEPTH

// Math defintions
#define 	M_E   		2.7182818284590452354
#define 	M_LOG2E   	1.4426950408889634074 	/* log_2 e */
#define 	M_LOG10E   	0.43429448190325182765 	/* log_10 e */
#define 	M_LN2   	0.69314718055994530942 	/* log_e 2 */
#define 	M_LN10   	2.30258509299404568402 	/* log_e 10 */
#define 	M_PI   		3.14159265358979323846 	/* pi */
// #define     M_PI 		3.141592653589793238462643383279 /* pi */
#define 	M_PI_2   	1.57079632679489661923 	/* pi/2 */
#define 	M_PI_4   	0.78539816339744830962 	/* pi/4 */
#define 	M_1_PI   	0.31830988618379067154 	/* 1/pi */
#define 	M_2_PI   	0.63661977236758134308 	/* 2/pi */
#define 	M_2_SQRTPI	1.12837916709551257390 	/* 2/sqrt(pi) */
#define 	M_SQRT2 	1.41421356237309504880 	/* sqrt(2) */
#define 	M_SQRT1_2   0.70710678118654752440 	/* 1/sqrt(2) */

const float stepSize = 	0.01;
const float samplingRate = 1.0;

// uniform int SCREEN_WIDTH;
// uniform int SCREEN_HEIGHT;
uniform float ALPHA_LIMIT = 0.99;
uniform float EPSILON = 0.01;

in vec2 texCoord;
out vec4 color;

// ================================================================================ 
// Headers, 
// volume and transferfunctions uniforms
// ================================================================================
#pragma openspace insert HEADERS

vec3 CartesianToSpherical(vec3 _cartesian) {
  // Put cartesian in [-1..1] range first
  vec3 cartesian = vec3(-1.0,-1.0,-1.0) + _cartesian * 2.0f;
  
  float r = length(cartesian);
  float theta, phi;

  if (r == 0.0) {
    theta = phi = 0.0;
  } else {
    theta = acos(cartesian.z/r) / M_PI;
    // float diff = -.0f;
    phi = (M_PI + atan(cartesian.y, cartesian.x)) / (2.0*M_PI );
    // phi = (M_PI+radians(diff)  + atan(cartesian.y, cartesian.x)) / (2.0*M_PI+radians(diff) );
    // phi = (M_PI + atan(cartesian.x, cartesian.y)) / (2.0*M_PI);
  }
  // r *= 0.57735026919;
  r = r / sqrt(3.0f);
  
  // Sampler ignores w component
  // return vec3(r, phi, theta);
  return vec3(r, theta, phi);
  // return vec3(r, phi*0.7, theta);
}

// ================================================================================
// The ABuffer specific includes and definitions
// ================================================================================
#include "abufferStruct.hglsl"
ABufferStruct_t fragments[MAX_FRAGMENTS];

#if MAX_VOLUMES > 0
vec3 volume_direction[MAX_VOLUMES];
float volume_length[MAX_VOLUMES];
vec3 volume_position[MAX_VOLUMES];

#if ZTYPE == ZDEPTH
	vec2 volume_zlength[MAX_VOLUMES];
#elif ZTYPE == PSCDEPTH
	float volume_zlength[MAX_VOLUMES];
#endif
#endif
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
    // src.a = 1.0 -(1.0 - src.a*stepSize);
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

float volumeRaycastingDistance(in int id, in ABufferStruct_t startFrag, in ABufferStruct_t endFrag) {
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
}

float globz(float z) {
	return z;
	// return log(2.0*z-1.0);
	// return exp(2.0*z-1.0);
	// const float zNear = 0.1f;
	// const float zFar = 100.0f;
	// float z_b = z;
 //    float z_n = 2.0 * z_b - 1.0;
 //    float z_e = 2.0 * zNear * zFar / (zFar + zNear - z_n * (zFar - zNear));
	// return z_e;
	// return (2.0 * z - near - far) / (far - near);
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

			int volbit = currentVolumeBitmask;
			int endbittype = int(_type_(endFrag));
			if(endbittype > 0)
				volbit = volbit ^ (1 << (endbittype-1));

			// int volID = type -1;
			// int volID = type-1;
			int volID;
			
			// TODO: Fix this non-working normalization for volumes with different stepsize
			float myMaxSteps = 0.0000001;
			float vlength;
			for(int v = 0; v < MAX_VOLUMES; ++v) {
				if((currentVolumeBitmask & (1 << v)) > 0) {

					// float l = volume_length[v]/volumeStepSize[v];
					// if(myMaxSteps < l) {
					// 	myMaxSteps = l;
					// 	volID = v;
					// }
					float l = volumeRaycastingDistance(v, startFrag, endFrag);
					// myMaxSteps = max(myMaxSteps, L/volumeStepSizeOriginal[v]);
					myMaxSteps = max(myMaxSteps, l/volumeStepSizeOriginal[v]);

					// if((volbit & (1 << v)) > 0)
					// 	volID = v;
					// if(volbit == 0)
					// 	volID = v;
					// if(type - 1 != v &&  type == endbittype)
					// 	volID = v;

					// if(volID < 0) volID = v;
					// volID = v;
				}
			}
			// for(int v = 0; v < MAX_VOLUMES; ++v) {
			// 	if((currentVolumeBitmask & (1 << v)) > 0) {
			// 		float aaa = volume_length[v]/myMaxSteps;
			// 		volumeStepSize[v] = volumeStepSizeOriginal[v]*vec3(aaa);
			// 	}
			// }
			// float myMaxSteps = stepSize;
			// for(int v = 0; v < MAX_VOLUMES; ++v) {
			// 	if((currentVolumeBitmask & (1 << v)) > 0) {
			// 		myMaxSteps = min(myMaxSteps, volumeStepSize[v]);
			// 	}
			// }

			for(int v = 0; v < MAX_VOLUMES; ++v) {
				if((currentVolumeBitmask & (1 << v)) > 0) {
					// float aaa = myMaxSteps;
					float l = volumeRaycastingDistance(v, startFrag, endFrag);
					float aaa = l/myMaxSteps;
					// float aaa = volume_length[v]/myMaxSteps;
					// volumeStepSize[v] = volumeStepSizeOriginal[v]*vec3(aaa);
					// float aaa = volume_length[v]/volumeStepSizeOriginal[v];
					volumeStepSize[v] = aaa;

					// if(vlength >= volume_length[v]) {
					// 	// vlength = volume_length[v];
					// 	const float S1 = volume_zlength[v].x;
					// 	const float S2 = volume_zlength[v].y;
					// 	const float L = S1 - S2;
					// 	vlength = volume_length[v];
					// 	vlength = L;
					// 	volID = v;
					// }
					// if(volID < 0) volID = v;
					volID = v;
				}
			}


			float l = volumeRaycastingDistance(volID, startFrag, endFrag);
			int max_iterations = int(l / volumeStepSize[volID]);

			// TransferFunction
			vec4 color = vec4(0);
			for(int k = 0; k < max_iterations && final_color.a < ALPHA_LIMIT && k < LOOP_LIMIT; ++k) {
			//while(p < l && iterations < LOOP_LIMIT) {

#pragma openspace insert SAMPLERCALLS

				//final_color = blend(final_color, color*stepSize);

				//volume_position[volID] += volume_direction[volID]*volumeStepSize[volID];
				//p+= stepSize;
				//++iterations;
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
#ifdef SHOWFUNC
#pragma openspace insert TRANSFERFUNC
#endif

	// if(frag_count == 1) {
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

#ifdef COLORNORMALIZATION
	final_color.rgb = final_color.rgb * final_color.a;
	final_color.a = 1.0;
#endif

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



