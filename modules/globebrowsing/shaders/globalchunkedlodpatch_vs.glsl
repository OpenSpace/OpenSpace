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

#define NUMLAYERS_COLORTEXTURE #{numLayersColor}
#define NUMLAYERS_HEIGHTMAP #{numLayersHeight}

uniform mat4 modelViewProjectionTransform;
uniform vec3 radiiSquared;

uniform vec2 minLatLon;
uniform vec2 lonLatScalingFactor;

uniform int xSegments;
uniform float skirtLength;

uniform TextureTile heightTiles[NUMLAYERS_HEIGHTMAP];
uniform TextureTile heightTilesParent1[NUMLAYERS_HEIGHTMAP];
uniform TextureTile heightTilesParent2[NUMLAYERS_HEIGHTMAP];

uniform vec3 cameraPosition;
uniform float distanceScaleFactor;
uniform int chunkLevel;

layout(location = 1) in vec2 in_uv;

out vec2 fs_uv;
out vec4 fs_position;
// tileInterpolationParameter is used to interpolate between a tile and its parent tiles
// The value increases with the distance from the vertex (or fragment) to the camera
out float tileInterpolationParameter;

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

	float height = 0;

    // Calculate desired level based on distance to the vertex on the ellipsoid
    // Before any heightmapping is done
	float distToVertexOnEllipsoid = length(pair.position - cameraPosition);
    float projectedScaleFactor = distanceScaleFactor / distToVertexOnEllipsoid;
	float desiredLevel = log2(projectedScaleFactor);

	tileInterpolationParameter = chunkLevel - desiredLevel;
	float w1 = clamp(1 - tileInterpolationParameter, 0 , 1);
	float w2 = (clamp(tileInterpolationParameter, 0 , 1) - clamp(tileInterpolationParameter - 1, 0 , 1));
	float w3 = clamp(tileInterpolationParameter - 1, 0 , 1);

	#for j in 1..#{numLayersHeight}
	{
		int i = #{j} - 1;
		vec2 samplePos =
			heightTiles[i].uvTransform.uvScale * in_uv +
			heightTiles[i].uvTransform.uvOffset;
		vec2 samplePosParent1 =
			heightTilesParent1[i].uvTransform.uvScale * in_uv +
			heightTilesParent1[i].uvTransform.uvOffset;
		vec2 samplePosParent2 =
			heightTilesParent2[i].uvTransform.uvScale * in_uv +
			heightTilesParent2[i].uvTransform.uvOffset;

		float sampledValue =
			w1 * texture(heightTiles[i].textureSampler, samplePos).r +
			w2 * texture(heightTilesParent1[i].textureSampler, samplePosParent1).r +
			w3 * texture(heightTilesParent2[i].textureSampler, samplePosParent2).r;
		
		// TODO : Some kind of blending here. Now it just writes over
		height = (sampledValue *
			heightTiles[i].depthTransform.depthScale +
			heightTiles[i].depthTransform.depthOffset);

		// Skirts
		int vertexIDx = gl_VertexID % (xSegments + 3);
		int vertexIDy = gl_VertexID / (xSegments + 3);
		if (vertexIDx == 0 ||
			vertexIDy == 0 ||
			vertexIDx == (xSegments + 2) ||
			vertexIDy == (xSegments + 2) ) {
			height -= skirtLength;
		}
	}
	#endfor

	// Add the height in the direction of the normal
	pair.position += pair.normal * height;

	vec4 positionClippingSpace = modelViewProjectionTransform * vec4(pair.position, 1);

	// Write output
	fs_uv = in_uv;
	fs_position = z_normalization(positionClippingSpace);
	gl_Position = fs_position;
}