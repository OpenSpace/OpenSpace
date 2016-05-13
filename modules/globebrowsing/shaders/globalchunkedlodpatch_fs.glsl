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
#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

#define NUMLAYERS_COLORTEXTURE 1
#define NUMLAYERS_HEIGHTMAP 1

uniform TextureTile colorTiles[NUMLAYERS_COLORTEXTURE];

in vec4 fs_position;
in vec2 fs_uv;

vec4 blendOver(vec4 oldColor, vec4 newColor)
{
	vec4 toReturn;
	toReturn.rgb =
		(newColor.rgb * newColor.a + oldColor.rgb * oldColor.a * (1 - newColor.a)) /
		(newColor.a + oldColor.a * (1 - newColor.a));
	toReturn.a = newColor.a + oldColor.a * (1 - newColor.a);
	return toReturn;
}

Fragment getFragment() {
	Fragment frag;

	for (int i = 0; i < NUMLAYERS_COLORTEXTURE; ++i)
	{
		vec2 samplePos =
		colorTiles[i].uvTransform.uvScale * fs_uv +
		colorTiles[i].uvTransform.uvOffset;
		vec4 colorSample = texture(colorTiles[i].textureSampler, samplePos);
		frag.color = blendOver(frag.color, colorSample);
	}
	
	//frag.color.rgb *= 10;

	// Sample position overlay
	//frag.color = frag.color * 0.9 + 0.2*vec4(samplePos, 0, 1);

	// Border overlay
	//frag.color = frag.color + patchBorderOverlay(fs_uv, vec3(0.5, 0.5, 0.5), 0.02);

	frag.depth = fs_position.w;

	return frag;
}

