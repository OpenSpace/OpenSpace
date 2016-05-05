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

/*
// Heightmap coverage
uniform sampler2D textureSamplerHeight00;
uniform sampler2D textureSamplerHeight10;
uniform sampler2D textureSamplerHeight01;
uniform sampler2D textureSamplerHeight11;
uniform mat3 uvTransformPatchToTileHeight00;
uniform mat3 uvTransformPatchToTileHeight10;
uniform mat3 uvTransformPatchToTileHeight01;
uniform mat3 uvTransformPatchToTileHeight11;
*/

// Colortexture coverage
uniform sampler2D textureSamplerColor00;
uniform sampler2D textureSamplerColor10;
uniform sampler2D textureSamplerColor01;
uniform sampler2D textureSamplerColor11;
uniform mat3 uvTransformPatchToTileColor00;
uniform mat3 uvTransformPatchToTileColor10;
uniform mat3 uvTransformPatchToTileColor01;
uniform mat3 uvTransformPatchToTileColor11;
uniform uvec2 texture00DimensionsColor;
uniform uvec2 texture10DimensionsColor;
uniform uvec2 texture01DimensionsColor;
uniform uvec2 texture11DimensionsColor;

//uniform int segmentsPerPatch;

in vec4 vs_position;
in vec3 fs_position;
in vec2 fs_uv;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
	Fragment frag;

	frag.color = vec4(0);
	vec4 color00, color10, color01, color11;

	vec2 uv00 = vec2(uvTransformPatchToTileColor00 * vec3(fs_uv.s, fs_uv.t, 1));
	vec2 uv10 = vec2(uvTransformPatchToTileColor10 * vec3(fs_uv.s, fs_uv.t, 1));
	vec2 uv01 = vec2(uvTransformPatchToTileColor01 * vec3(fs_uv.s, fs_uv.t, 1));
	vec2 uv11 = vec2(uvTransformPatchToTileColor11 * vec3(fs_uv.s, fs_uv.t, 1));

	vec2 d00 = 1 / vec2(texture00DimensionsColor);
	vec2 d10 = 1 / vec2(texture10DimensionsColor);
	vec2 d01 = 1 / vec2(texture01DimensionsColor);
	vec2 d11 = 1 / vec2(texture11DimensionsColor);

	if (uv00.x > -d00.x && uv00.x < 1 + d00.x && uv00.y > -d00.y && uv00.y < 1 + d00.y)
		color00 = texture(textureSamplerColor00, uv00);
	if (uv10.x > -d10.x && uv10.x < 1 + d10.x && uv10.y > -d10.y && uv10.y < 1 + d10.y)
		color10 = texture(textureSamplerColor10, uv10);
	if (uv01.x > -d01.x && uv01.x < 1 + d01.x && uv01.y > -d01.y && uv01.y < 1 + d01.y)
		color01 = texture(textureSamplerColor01, uv01);	
	if (uv11.x > -d11.x && uv11.x < 1 + d11.x && uv11.y > -d11.y && uv11.y < 1 + d11.y)
		color11 = texture(textureSamplerColor11, uv11);

	frag.color = max(color00, max(color10, max(color01, color11)));
	//frag.color = vec4(frag.color.r, frag.color.r, frag.color.r, 1);

	frag.depth =  vs_position.w;

	return frag;
}

