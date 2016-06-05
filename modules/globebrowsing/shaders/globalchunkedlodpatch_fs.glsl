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

#include <${MODULE_GLOBEBROWSING}/shaders/texturetile.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/blending.hglsl>
#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

#define NUMLAYERS_COLORTEXTURE #{lastLayerIndexColor} + 1
#define NUMLAYERS_HEIGHTMAP #{lastLayerIndexHeight} + 1

uniform TextureTile colorTiles[NUMLAYERS_COLORTEXTURE];
uniform TextureTile colorTilesParent1[NUMLAYERS_COLORTEXTURE];
uniform TextureTile colorTilesParent2[NUMLAYERS_COLORTEXTURE];

in vec4 fs_position;
in vec2 fs_uv;

in float tileInterpolationParameter;

Fragment getFragment() {
	Fragment frag;

	// tileInterpolationParameter increases with distance
	float w1 = clamp(1 - tileInterpolationParameter, 0 , 1);
	float w2 =  (clamp(tileInterpolationParameter, 0 , 1) - clamp(tileInterpolationParameter - 1, 0 , 1));
	float w3 = clamp(tileInterpolationParameter - 1, 0 , 1);

	#for i in 0..#{lastLayerIndexColor}
	{
		vec2 samplePos =
			colorTiles[#{i}].uvTransform.uvScale * fs_uv +
			colorTiles[#{i}].uvTransform.uvOffset;
		vec2 samplePosParent1 =
			colorTilesParent1[#{i}].uvTransform.uvScale * fs_uv +
			colorTilesParent1[#{i}].uvTransform.uvOffset;
		vec2 samplePosParent2 =
			colorTilesParent2[#{i}].uvTransform.uvScale * fs_uv +
			colorTilesParent2[#{i}].uvTransform.uvOffset;
		

/*
		vec4 colorSample =
			w1 * textureLod(colorTiles[#{i}].textureSampler, samplePos, 0) +
			w2 * textureLod(colorTilesParent1[#{i}].textureSampler, samplePosParent1, 0) +
			w3 * textureLod(colorTilesParent2[#{i}].textureSampler, samplePosParent2, 0);
*/

		/*
		vec4 colorSample =
			w1 * textureGrad(colorTiles[#{i}].textureSampler, samplePos, vec2(0), vec2(0)) +
			w2 * textureGrad(colorTilesParent1[#{i}].textureSampler, samplePosParent1, vec2(0), vec2(0)) +
			w3 * textureGrad(colorTilesParent2[#{i}].textureSampler, samplePosParent2, vec2(0), vec2(0));
		*/
		
		vec4 colorSample =
			w1 * texture(colorTiles[#{i}].textureSampler, samplePos) +
			w2 * texture(colorTilesParent1[#{i}].textureSampler, samplePosParent1) +
			w3 * texture(colorTilesParent2[#{i}].textureSampler, samplePosParent2);



		frag.color = blendOver(frag.color, colorSample);
	}
	#endfor

	frag.depth = fs_position.w;

	return frag;
}

