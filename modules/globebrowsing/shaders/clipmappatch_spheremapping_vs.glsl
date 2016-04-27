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

uniform mat4 modelViewProjectionTransform;
uniform vec3 radiiSquared;

uniform vec2 minLatLon;
uniform vec2 lonLatScalingFactor;
uniform ivec2 contraction; // [-1, 1]

layout(location = 1) in vec2 in_uv;

out vec4 vs_position;
out vec2 vs_uv;

#include "PowerScaling/powerScaling_vs.hglsl"

vec3 geodeticSurfaceNormal(float latitude, float longitude)
{
	float cosLat = cos(latitude);
	return vec3(
		cosLat * cos(longitude),
		cosLat * sin(longitude), 
		sin(latitude));
}

vec3 geodetic3ToCartesian(
	float latitude,
	float longitude,
	float height,
	vec3 radiiSquared)
{
	vec3 normal = geodeticSurfaceNormal(latitude, longitude);
	vec3 k = radiiSquared * normal;
	float gamma = sqrt(dot(k, normal));
	vec3 rSurface = k / gamma;
	return rSurface + height * normal;
}

vec3 geodetic2ToCartesian(float latitude, float longitude, vec3 radiiSquared)
{
	// Position on surface : height = 0
	return geodetic3ToCartesian(latitude, longitude, 0, radiiSquared);
}

vec3 latLonToCartesian(float latitude, float longitude, float radius) {
	return radius * vec3(
		cos(latitude) * cos(longitude),
		cos(latitude) * sin(longitude),
		sin(latitude));
}

vec3 globalInterpolation(vec2 uv) {
	vec2 lonLatInput;
	lonLatInput.y = minLatLon.y + lonLatScalingFactor.y * uv.y; // Lat
	lonLatInput.x = minLatLon.x + lonLatScalingFactor.x * uv.x; // Lon
	vec3 positionModelSpace = geodetic2ToCartesian(lonLatInput.y, lonLatInput.x, radiiSquared);// latLonToCartesian(lonLatInput.y, lonLatInput.x, globeRadius);
	return positionModelSpace;
}

void main()
{
	vs_uv = in_uv;

	vec2 scaledContraction = contraction / 32.0f;
	vs_uv += scaledContraction;
	vs_uv = clamp(vs_uv, 0, 1);
	vs_uv -= scaledContraction;

	vec3 p = globalInterpolation(vs_uv);

	vec4 position = modelViewProjectionTransform * vec4(p, 1);
	vs_position = z_normalization(position);
	gl_Position = vs_position;
}