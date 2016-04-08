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

uniform vec3 p00;
uniform vec3 p10;
uniform vec3 p20;
uniform vec3 p01;
uniform vec3 p11;
uniform vec3 p21;
uniform vec3 p02;
uniform vec3 p12;
uniform vec3 p22;

uniform float w10;
uniform float w11;
uniform float w12;

/*
uniform vec3 n00;
uniform vec3 n10;
uniform vec3 n20;
uniform vec3 n01;
uniform vec3 n11;
uniform vec3 n21;
uniform vec3 n02;
uniform vec3 n12;
uniform vec3 n22;
*/
layout(location = 1) in vec2 in_UV;

out vec4 vs_position;
out vec2 vs_uv;

#include "PowerScaling/powerScaling_vs.hglsl"

void nurbsBasis(int c, float t, int npts, int x[7], inout float n[4])
{
	int nplusc;
	int i,k;
	float d, e;
	float temp[36];

	nplusc = npts + c;

/* calculate the first order basis functions n[i][1]	*/

	for (i = 1; i<= nplusc-1; i++){
    	if (( t >= x[i]) && (t < x[i+1]))
			temp[i] = 1;
	    else
			temp[i] = 0;
	}

/* calculate the higher order basis functions */

	for (k = 2; k <= c; k++){
    	for (i = 1; i <= nplusc-k; i++){
        	if (temp[i] != 0)    /* if the lower order basis function is zero skip the calculation */
           		d = ((t-x[i])*temp[i])/(x[i+k-1]-x[i]);
	        else
				d = 0;

    	    if (temp[i+1] != 0)     /* if the lower order basis function is zero skip the calculation */
        		e = ((x[i+k]-t)*temp[i+1])/(x[i+k]-x[i+1]);
	        else
    			e = 0;

    	    temp[i] = d + e;
		}
	}

	float floatXnplusc = x[nplusc];
	if (t == floatXnplusc){		/*    pick up last point	*/
 		temp[npts] = 1;
	}

/* put in n array	*/

	for (i = 1; i <= npts; i++) {
    	n[i] = temp[i];
	}
}


void getKnots(int n, int c, inout int x[7])
{
	int nplusc, nplus2, i;

	nplusc = n + c;
	nplus2 = n + 2;

	x[1] = 0;
	for (i = 2; i <= nplusc; i++){
	    if ( (i > c) && (i < nplus2) )
			x[i] = x[i-1] + 1;
    	else
			x[i] = x[i-1];
	}
}

void main()
{
	vs_uv = in_UV;

	// Bilinear interpolation
	
	/*
	vec3 p0 = (1 - in_UV.x) * p00 + in_UV.x * p20;
	vec3 p2 = (1 - in_UV.x) * p02 + in_UV.x * p22;
	vec3 p = (1 - in_UV.y) * p0 + in_UV.y * p2;
	*/
	// Calculate three weights
	// These values of the weights will make the curves in to circle segments

	//float w10 = dot(n00, normalize(n00 + n20));
	//float w11 = dot(n01, normalize(n01 + n21));
	//float w12 = dot(n02, normalize(n02 + n22));

	float basisFunctionValues[4];
	int order = 3; // Order of the NURBS curve
	int npts = 3; // Number of control points
	int knotVector[7] = {0,0,0,0,1,1,1};

	// Interpolate in u direction
	// getKnots(npts, order, knotVector);
	nurbsBasis(order, in_UV.y, npts, knotVector, basisFunctionValues);	
	vec3 p0 =
		basisFunctionValues[1] * p00 +
		basisFunctionValues[2] * p10 * w10+ 
		basisFunctionValues[3] * p20;
	vec3 p1 =
		basisFunctionValues[1] * p01 +
		basisFunctionValues[2] * p11 * w11+ 
		basisFunctionValues[3] * p21;
	vec3 p2 =
		basisFunctionValues[1] * p02 +
		basisFunctionValues[2] * p12 * w12+ 
		basisFunctionValues[3] * p22;

	// Calculate the last weight
	float w1 = dot(normalize(p0), normalize(p1));

	// Interpolate in v direction
	nurbsBasis(order, in_UV.x, npts, knotVector, basisFunctionValues);	
	vec3 p =
		basisFunctionValues[1] * p0 +
		basisFunctionValues[2] * p1 * w1 * w11+ 
		basisFunctionValues[3] * p2;
	
	//p = (1 - in_UV.x) * p0 + in_UV.x * p2;


	vec4 position = Projection * vec4(p, 1);
	gl_Position = z_normalization(position);
}