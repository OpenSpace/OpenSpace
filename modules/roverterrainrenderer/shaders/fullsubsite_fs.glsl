/*****************************************************************************************
    *                                                                                       *
    * OpenSpace                                                                             *
    *                                                                                       *
    * Copyright (c) 2014-2017                                                               *
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

// Uniforms
uniform float transparency;
uniform float alpha;

uniform vec3 directionToSunViewSpace;

uniform sampler2DArray roverTerrainTextures;
uniform sampler2DArray roverTerrainColoredTextures;

// Input from the vertex shader
in vec4 vs_positionCameraSpace;
in vec4 vs_positionScreenSpace;
flat in int textureIndex;

flat in int coloredTextureIndexOut;
in vec2 vs_stDone[22];
in vec2 vs_st_color[7];


#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
    int blackAndWhiteUvIndex = textureIndex;
    vec4 prevTexture3 = vec4(0,0,0,0);

    if (coloredTextureIndexOut != -1 && vs_st_color[coloredTextureIndexOut].s >= 0.0 && vs_st_color[coloredTextureIndexOut].t >= 0.0 && vs_st_color[coloredTextureIndexOut].s <= 1.0 && vs_st_color[coloredTextureIndexOut].t <= 1.0) {
        prevTexture3 = texture(roverTerrainColoredTextures, vec3(vs_st_color[coloredTextureIndexOut], coloredTextureIndexOut));
    } else {
        prevTexture3 = texture(roverTerrainTextures, vec3(vs_stDone[blackAndWhiteUvIndex], blackAndWhiteUvIndex));
    }

    prevTexture3.a = alpha;

    Fragment frag2;
    frag2.color = prevTexture3;
    frag2.depth = vs_positionScreenSpace.w;

    return frag2;
}
