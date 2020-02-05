/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

in vec2 vs_st;
in float vs_screenSpaceDepth;
in vec4 shadowCoords;

uniform sampler2DShadow shadowMapTexture;
uniform sampler1D ringTexture;
uniform vec2 textureOffset;
uniform float transparency;

uniform vec3 sunPosition;
uniform float _nightFactor;
uniform int nShadowSamples;
uniform float zFightingPercentage;

// temp
in vec4 fragPosInLightSpace;

Fragment getFragment() {
    // Moving the origin to the center
    vec2 st = (vs_st - vec2(0.5)) * 2.0;

    // The length of the texture coordinates vector is our distance from the center
    float radius = length(st);

    // We only want to consider ring-like objects so we need to discard everything else
    if (radius > 1.0) {
        discard;
    }

    // Remapping the texture coordinates
    // Radius \in [0,1],  texCoord \in [textureOffset.x, textureOffset.y]
    // textureOffset.x -> 0
    // textureOffset.y -> 1
    float texCoord = (radius - textureOffset.x) / (textureOffset.y - textureOffset.x);
    if (texCoord < 0.f || texCoord > 1.f) {
        discard;
    }

    vec4 diffuse = texture(ringTexture, texCoord);
    float colorValue = length(diffuse.rgb);
    // times 3 as length of vec3(1.0, 1.0, 1.0) will return 3 and we want
    // to normalize the transparency value to [0,1]
    if (colorValue < 3.0 * transparency) {
        diffuse.a = pow(colorValue / (3.0 * transparency), 1);
        //diffuse.a = (colorValue / 3.0) * transparency;
        if (diffuse.a < 0.65)
            discard;
    }

    // shadow == 1.0 means it is not in shadow
    float shadow = 1.0;
    if ( shadowCoords.z >= 0 ) {
        vec4 normalizedShadowCoords = shadowCoords;
        normalizedShadowCoords.z    = normalizeFloat(zFightingPercentage * normalizedShadowCoords.w);
        normalizedShadowCoords.xy   = normalizedShadowCoords.xy / normalizedShadowCoords.w;
        normalizedShadowCoords.w    = 1.0;
        
        float sum = 0;
        for (int i = 0; i < nShadowSamples; ++i) {
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2(-nShadowSamples + i, -nShadowSamples + i));
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2(-nShadowSamples + i,  0));
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2(-nShadowSamples + i,  nShadowSamples - i));
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2( 0                 , -nShadowSamples + i));
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2( 0                 ,  nShadowSamples - i));
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2( nShadowSamples - i, -nShadowSamples + i));
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2( nShadowSamples - i,  0));
            sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2( nShadowSamples - i,  nShadowSamples - i));
        }
        sum += textureProjOffset(shadowMapTexture, normalizedShadowCoords, ivec2(0, 0));
        shadow = sum / (8.0 * nShadowSamples + 1.f);
    }
    
    // The normal for the one plane depends on whether we are dealing
    // with a front facing or back facing fragment
    vec3 normal;
    // The plane is oriented on the xz plane
    // WARNING: This might not be the case for Uranus
    if (gl_FrontFacing) {
        normal = vec3(-1.0, 0.0, 0.0);
    }
    else {
        normal = vec3(1.0, 0.0, 0.0);
    }

    // Reduce the color of the fragment by the user factor
    // if we are facing away from the Sun
    if (dot(sunPosition, normal) < 0) {
        diffuse.xyz *= _nightFactor;
    }

    Fragment frag;

    frag.color      = (0.65 * diffuse * shadow) + diffuse * 0.35;
    frag.depth      = vs_screenSpaceDepth;
    frag.gPosition  = vec4(1e30, 1e30, 1e30, 1.0);
    frag.gNormal    = vec4(normal, 1.0);

    return frag;
}
