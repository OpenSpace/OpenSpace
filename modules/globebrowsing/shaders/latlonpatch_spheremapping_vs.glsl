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

#version __CONTEXT__

uniform mat4 Projection;
uniform vec3[9] p;
uniform float interpolationWeight;

layout(location = 1) in vec2 in_UV;

out vec4 vs_position;
out vec2 vs_uv;

#include "PowerScaling/powerScaling_vs.hglsl"

// Nurbs basis function for third order and three conrol points 
// This function is not general. It only works for third order with three control points.
void nurbsBasis(float t, inout float n[3])
{
	#define ORDER 3
	#define NPTS 3
	
	#define ORDER_2 2
	#define ORDER_3 3

	int i,k;
	float d, e;
	float temp[5];

	int knots[6] = {0,0,0,1,1,1};

	// calculate the first order basis functions n[i][1]
	for (i = 0; i < 5; i++){ // Loop to NPTS + ORDER -1
    	if (( t >= knots[i]) && (t < knots[i+1]))
			temp[i] = 1;
	    else
			temp[i] = 0;
	}

	// calculate the higher order basis functions
	// Second order
	for (i = 0; i < 4; i++){ // Loop to NPTS + ORDER - 2 = 4
		// if the lower order basis function is zero skip the calculation
		d = (temp[i] != 0) 		? ((t-knots[i])*temp[i])/(knots[i+ORDER_2-1]-knots[i]) 				: 0;
		e = (temp[i+1] != 0) 	? ((knots[i+ORDER_2]-t)*temp[i+1])/(knots[i+ORDER_2]-knots[i+1])	: 0;
	    temp[i] = d + e;
	}

	// Third order
	for (i = 0; i < 3; i++){ // Loop to NPTS + ORDER - 3 = 3
		// if the lower order basis function is zero skip the calculation
		d = (temp[i] != 0) 		? ((t-knots[i])*temp[i])/(knots[i+ORDER_3-1]-knots[i]) 				: 0;
		e = (temp[i+1] != 0) 	? ((knots[i+ORDER_3]-t)*temp[i+1])/(knots[i+ORDER_3]-knots[i+1])	: 0;
	    temp[i] = d + e;
	}

	// Last point
	temp[NPTS - 1] = (t == knots[NPTS + ORDER - 1]) ? 1 : temp[NPTS - 1];
	
	// Copy to array
	for (i = 0; i < 3; i++) {
    	n[i] = temp[i];
	}
}

// These are the physical positions of the control points for the patch:
//
// y  p[6] p[7] p[8]     p02 p12 p22
// ^  p[3] p[4] p[5] <=> p01 p11 p21
// |  p[0] p[1] p[2]     p00 p10 p20
// |
//  -----> x

vec3 bilinearInterpolation() {
	// Bilinear interpolation
	vec3 p0 = (1 - in_UV.y) * p[0] + in_UV.y * p[2];
	vec3 p2 = (1 - in_UV.y) * p[6] + in_UV.y * p[8];
	return (1 - in_UV.x) * p0 + in_UV.x * p2;
}

vec3 nurbsInterpolation() {
	float basisFunctionValues[3];
	// Interpolate in u direction
	nurbsBasis(in_UV.y, basisFunctionValues);	
	vec3 p0 =
		basisFunctionValues[0] * p[0] +
		basisFunctionValues[1] * p[1] * interpolationWeight + 
		basisFunctionValues[2] * p[2];
	vec3 p1 =
		basisFunctionValues[0] * p[3] +
		basisFunctionValues[1] * p[4] * interpolationWeight + 
		basisFunctionValues[2] * p[5];
	vec3 p2 =
		basisFunctionValues[0] * p[6] +
		basisFunctionValues[1] * p[7] * interpolationWeight + 
		basisFunctionValues[2] * p[8];

	// Calculate the last interpolation weight
	float w1 = dot(normalize(p0), normalize(p1));

	// Interpolate in v direction
	nurbsBasis(in_UV.x, basisFunctionValues);	
	return
		basisFunctionValues[0] * p0 +
		basisFunctionValues[1] * p1 * w1 * interpolationWeight + 
		basisFunctionValues[2] * p2;
}

void main()
{
	vs_uv = in_UV;
	//vec3 p = bilinearInterpolation();
	vec3 p = nurbsInterpolation();

	vec4 position = Projection * vec4(p, 1);
	gl_Position = z_normalization(position);
}