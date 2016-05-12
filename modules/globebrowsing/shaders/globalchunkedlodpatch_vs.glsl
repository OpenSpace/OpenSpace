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

#include "PowerScaling/powerScaling_vs.hglsl"
#include <${MODULE_GLOBEBROWSING}/shaders/ellipsoid.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/texturetile.hglsl>

uniform mat4 modelViewProjectionTransform;
uniform vec3 radiiSquared;

uniform vec2 minLatLon;
uniform vec2 lonLatScalingFactor;

uniform TextureTile heightTile;

layout(location = 1) in vec2 in_uv;

out vec2 fs_uv;
out vec4 fs_position;

PositionNormalPair globalInterpolation() {
	vec2 lonLatInput;
	lonLatInput.y = minLatLon.y + lonLatScalingFactor.y * in_uv.y; // Lat
	lonLatInput.x = minLatLon.x + lonLatScalingFactor.x * in_uv.x; // Lon
	PositionNormalPair positionPairModelSpace = geodetic2ToCartesian(lonLatInput.y, lonLatInput.x, radiiSquared);
	return positionPairModelSpace;
}

void main()
{
	PositionNormalPair pair = globalInterpolation();

	vec2 samplePos =
		heightTile.uvTransform.uvScale * in_uv +
		heightTile.uvTransform.uvOffset;

	float sampledHeight = texture(heightTile.textureSampler, samplePos).r;

	pair.position +=
		pair.normal *
		(sampledHeight *
			heightTile.depthTransform.depthScale +
			heightTile.depthTransform.depthOffset);

	vec4 position = modelViewProjectionTransform * vec4(pair.position, 1);

	// Write output
	fs_uv = in_uv;
	fs_position = z_normalization(position);
	gl_Position = fs_position;
}