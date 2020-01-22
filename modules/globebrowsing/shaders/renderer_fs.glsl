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

#include "fragment.glsl"

#include <${MODULE_GLOBEBROWSING}/shaders/tile.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/texturetilemapping.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/tileheight.hglsl>
#include "PowerScaling/powerScaling_fs.hglsl"

// Below are all the tiles that are used for contributing the actual fragment color

#if USE_COLORTEXTURE
uniform Layer ColorLayers[NUMLAYERS_COLORTEXTURE];
#endif // USE_COLORTEXTURE

#if USE_NIGHTTEXTURE
uniform Layer NightLayers[NUMLAYERS_NIGHTTEXTURE];
#endif // USE_NIGHTTEXTURE

#if USE_OVERLAY
uniform Layer Overlays[NUMLAYERS_OVERLAY];
#endif // USE_OVERLAY

#if USE_WATERMASK
uniform Layer WaterMasks[NUMLAYERS_WATERMASK];
#endif // USE_WATERMASK

#if SHOW_HEIGHT_RESOLUTION
uniform vec2 vertexResolution;
#endif

#if USE_NIGHTTEXTURE || USE_WATERMASK || PERFORM_SHADING
uniform vec3 lightDirectionCameraSpace;
#endif

#if PERFORM_SHADING
uniform float orenNayarRoughness;
#endif

#if SHADOW_MAPPING_ENABLED
in vec4 shadowCoords;
uniform sampler2DShadow shadowMapTexture;
uniform int nShadowSamples;
uniform float zFightingPercentage;
#endif

#if USE_ECLIPSE_SHADOWS

/*******************************************************************************
 ***** ALL CALCULATIONS FOR ECLIPSE ARE IN METERS AND IN WORLD SPACE SYSTEM ****
 *******************************************************************************/
// JCC: Remove and use dictionary to 
// decides the number of shadows
const uint numberOfShadows = 1;

struct ShadowRenderingStruct {
    double xu, xp;
    double rs, rc;
    dvec3 sourceCasterVec;
    dvec3 casterPositionVec;
    bool isShadowing;
};

// Eclipse shadow data
// JCC: Remove and use dictionary to
// decides the number of shadows
uniform ShadowRenderingStruct shadowDataArray[numberOfShadows];
uniform int shadows;
uniform bool hardShadows;

vec4 calcShadow(const ShadowRenderingStruct shadowInfoArray[numberOfShadows],
                const dvec3 position, const bool ground)
{
    if (shadowInfoArray[0].isShadowing) {
        dvec3 pc = shadowInfoArray[0].casterPositionVec - position;
        dvec3 sc_norm = shadowInfoArray[0].sourceCasterVec;
        dvec3 pc_proj = dot(pc, sc_norm) * sc_norm;
        dvec3 d = pc - pc_proj;

        float length_d = float(length(d));
        double length_pc_proj = length(pc_proj);

        float r_p_pi = float(shadowInfoArray[0].rc * (length_pc_proj + shadowInfoArray[0].xp) / shadowInfoArray[0].xp);
        float r_u_pi = float(shadowInfoArray[0].rc * (shadowInfoArray[0].xu - length_pc_proj) / shadowInfoArray[0].xu);

        if (length_d < r_u_pi) { // umbra
            if (ground) {
#if USE_ECLIPSE_HARD_SHADOWS
                return vec4(0.2, 0.2, 0.2, 1.0);
#else
                // butterworthFunc
                return vec4(vec3(sqrt(r_u_pi / (r_u_pi + pow(length_d, 8.0)))), 1.0);
#endif
            }
            else {
#if USE_ECLIPSE_HARD_SHADOWS
                return vec4(0.5, 0.5, 0.5, 1.0);
#else
                return vec4(vec3(length_d / r_p_pi), 1.0);
#endif
            }
        }
        else if (length_d < r_p_pi) {// penumbra
#if USE_ECLIPSE_HARD_SHADOWS
            return vec4(0.5, 0.5, 0.5, 1.0); 
#else
            return vec4(vec3(length_d / r_p_pi), 1.0);
#endif
        }
    }

    return vec4(1.0);
}
#endif

in vec4 fs_position;
in vec2 fs_uv;
in vec3 ellipsoidNormalCameraSpace;
in vec3 levelWeights;
in vec3 positionCameraSpace;

#if USE_ACCURATE_NORMALS
    in vec3 ellipsoidTangentThetaCameraSpace;
    in vec3 ellipsoidTangentPhiCameraSpace;
#endif // USE_ACCURATE_NORMALS

#if USE_ECLIPSE_SHADOWS
in vec3 positionWorldSpace;
#endif // USE_ECLIPSE_SHADOWS



Fragment getFragment() {
    Fragment frag;
    frag.color = vec4(0.3, 0.3, 0.3, 1.0);

    vec3 normal = normalize(ellipsoidNormalCameraSpace);

#if USE_ACCURATE_NORMALS
    normal = getTileNormal(
        fs_uv,
        levelWeights,
        normalize(ellipsoidNormalCameraSpace),
        normalize(ellipsoidTangentThetaCameraSpace),
        normalize(ellipsoidTangentPhiCameraSpace)
    );
#endif /// USE_ACCURATE_NORMALS

#if USE_COLORTEXTURE
    frag.color = calculateColor(frag.color, fs_uv, levelWeights, ColorLayers);
#endif // USE_COLORTEXTURE

#if USE_WATERMASK
    float waterReflectance = 0.0;
    frag.color = calculateWater(
        frag.color,
        fs_uv,
        levelWeights,
        WaterMasks,
        normal,
        lightDirectionCameraSpace, // Should already be normalized
        positionCameraSpace,
        waterReflectance
    );

#endif // USE_WATERMASK

#if USE_NIGHTTEXTURE
    frag.color = calculateNight(
        frag.color,
        fs_uv,
        levelWeights,
        NightLayers,
        normalize(ellipsoidNormalCameraSpace),
        lightDirectionCameraSpace // Should already be normalized
    );

#endif // USE_NIGHTTEXTURE

#if PERFORM_SHADING
    frag.color = calculateShadedColor(
        frag.color,
        normal,
        lightDirectionCameraSpace,
        normalize(positionCameraSpace),
        orenNayarRoughness
    );
#endif // PERFORM_SHADING

#if USE_ECLIPSE_SHADOWS
    frag.color *= calcShadow(shadowDataArray, dvec3(positionWorldSpace), true);
#endif

#if USE_OVERLAY
    frag.color = calculateOverlay(frag.color, fs_uv, levelWeights, Overlays);
#endif // USE_OVERLAY

#if SHOW_HEIGHT_INTENSITIES
    frag.color.rgb *= vec3(0.1);

    float untransformedHeight = getUntransformedTileVertexHeight(fs_uv, levelWeights);
    float contourLine = fract(10.0 * untransformedHeight) > 0.98 ? 1.0 : 0.0;
    frag.color.r += untransformedHeight;
    frag.color.b = contourLine;
#endif

#if SHOW_HEIGHT_RESOLUTION
    frag.color += 0.0001 * calculateDebugColor(fs_uv, fs_position, vertexResolution);
    #if USE_HEIGHTMAP
        frag.color.r = min(frag.color.r, 0.8);
        frag.color.r += tileResolution(fs_uv, HeightLayers[0].pile.chunkTile0) > 0.9 ? 1 : 0;
    #endif
#endif

    // Other data
#if USE_WATERMASK
    // Water reflectance is added to the G-Buffer.
    frag.gNormal.w = waterReflectance;
#else
    frag.gNormal.w = 0;
#endif
    // Normal is written View Space (Including SGCT View Matrix).
    frag.gNormal.xyz = normal;

    if (dot(positionCameraSpace, vec3(1.0)) != 0.0) {
        frag.gPosition   = vec4(positionCameraSpace, 1.0); // in Camera Rig Space
    }
    else {
        frag.gPosition   = vec4(1.0); // in Camera Rig Space
    }

    frag.depth = fs_position.w;

#if SHOW_CHUNK_EDGES
    const float BorderSize = 0.005;
    const vec3 BorderColor = vec3(1.0, 0.0, 0.0);

    vec2 uvOffset = fs_uv - vec2(0.5);
    float thres = 0.5 - BorderSize * 0.5;
    bool isBorder = abs(uvOffset.x) > thres || abs(uvOffset.y) > thres;
    if (isBorder) {
        frag.color.rgb += BorderColor;
    }
#endif // SHOW_CHUNK_EDGES

#if SHADOW_MAPPING_ENABLED
    float shadow = 1.0;
    if (shadowCoords.w > 1) {
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
    frag.color.xyz *= shadow < 0.99 ? clamp(shadow + 0.3, 0.0, 1.0) : shadow;
#endif

    return frag;
}
