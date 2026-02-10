/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef _GLOBEBROWSING___TEXTURETILEMAPPING___GLSL_
#define _GLOBEBROWSING___TEXTURETILEMAPPING___GLSL_

#include "tile.glsl"
#include "blending.glsl"

// First layer type from LayerShaderManager is height map
#define NUMLAYERS_HEIGHTMAP #{lastLayerIndexHeightLayers} + 1
#define USE_HEIGHTMAP #{useHeightLayers}
#define HEIGHTMAP_BLENDING_ENABLED #{blendHeightLayers}

// Second layer type from LayerShaderManager is color texture
#define NUMLAYERS_COLORTEXTURE #{lastLayerIndexColorLayers} + 1
#define USE_COLORTEXTURE #{useColorLayers}
#define COLORTEXTURE_BLENDING_ENABLED #{blendColorLayers}

 // Third layer type from LayerShaderManager is water mask
#define NUMLAYERS_WATERMASK #{lastLayerIndexWaterMasks} + 1
#define USE_WATERMASK #{useWaterMasks}
#define WATERMASK_BLENDING_ENABLED #{blendWaterMasks}

// Fourth layer type from LayerShaderManager is night texture
#define NUMLAYERS_NIGHTTEXTURE #{lastLayerIndexNightLayers} + 1
#define USE_NIGHTTEXTURE #{useNightLayers}
#define NIGHTTEXTURE_BLENDING_ENABLED #{blendNightLayers}

// Fifth layer type from LayerShaderManager is overlay
#define NUMLAYERS_OVERLAY #{lastLayerIndexOverlays} + 1
#define USE_OVERLAY #{useOverlays}
#define OVERLAY_BLENDING_ENABLED #{blendOverlays}

// Global constants
#define CHUNK_DEFAULT_HEIGHT #{defaultHeight}

// Other key value pairs used for settings
#define USE_ACCURATE_NORMALS #{useAccurateNormals}
#define PERFORM_SHADING #{performShading}
#define USE_ECLIPSE_SHADOWS #{useEclipseShadows}
#define USE_ECLIPSE_HARD_SHADOWS #{useEclipseHardShadows}
#define SHOW_CHUNK_EDGES #{showChunkEdges}
#define SHOW_HEIGHT_RESOLUTION #{showHeightResolution}
#define SHOW_HEIGHT_INTENSITIES #{showHeightIntensities}

// Show shadow from globe onto rings
#define SHADOW_MAPPING_ENABLED #{enableShadowMapping}
// Show shadow from rings onto globe
#define USE_RING_SHADOWS #{useRingShadows}

const vec3 DefaultLevelWeights = vec3(1.0, 0.0, 0.0);


float orenNayarDiffuse(vec3 lightDirection, vec3 viewDirection, vec3 surfaceNormal,
                       float roughness)
{
  // calculate intermediary values
  float NdotL = dot(surfaceNormal, lightDirection);
  float NdotV = dot(surfaceNormal, viewDirection);

  float angleVN = acos(NdotV);
  float angleLN = acos(NdotL);

  float alpha = max(angleVN, angleLN);
  float beta = min(angleVN, angleLN);
  float gamma = dot(
    viewDirection - surfaceNormal * dot(viewDirection, surfaceNormal),
    lightDirection - surfaceNormal * dot(lightDirection, surfaceNormal)
  );

  float roughnessSquared = roughness * roughness;

  // calculate A and B
  float a = 1.0 - 0.5 * (roughnessSquared / (roughnessSquared + 0.57));
  float b = 0.45 * (roughnessSquared / (roughnessSquared + 0.09));
  float c = sin(alpha) * tan(beta);

  // put it all together
  return max(0.0, NdotL) * (a + b * max(0.0, gamma) * c);
}

float performLayerSettings(float value, LayerSettings settings) {
  float v = sign(value) * pow(abs(value), settings.gamma) *
    settings.multiplier + settings.offset;
  return v * settings.opacity;
}

vec4 performLayerSettings(vec4 value, LayerSettings settings) {
  vec3 v = sign(value.rgb) * pow(abs(value.rgb), vec3(settings.gamma)) *
    settings.multiplier + settings.offset;
  return vec4(v, value.a * settings.opacity);
}

vec2 tileUVToTextureSamplePosition(ChunkTile chunkTile, vec2 tileUV) {
  return chunkTile.uvTransform.uvOffset + chunkTile.uvTransform.uvScale * tileUV;
}

vec4 texVal(ChunkTilePile chunkTilePile, vec3 w, vec2 uv) {
  vec4 v1 = texture(
    chunkTilePile.chunkTile0.textureSampler,
    tileUVToTextureSamplePosition(chunkTilePile.chunkTile0, uv)
  );
  vec4 v2 = texture(
    chunkTilePile.chunkTile1.textureSampler,
    tileUVToTextureSamplePosition(chunkTilePile.chunkTile1, uv)
  );
  vec4 v3 = texture(
    chunkTilePile.chunkTile2.textureSampler,
    tileUVToTextureSamplePosition(chunkTilePile.chunkTile2, uv)
  );

  return w.x * v1 + w.y * v2 + w.z * v3;
}

#for id, layerGroup in layerGroups
#for i in 0..#{lastLayerIndex#{layerGroup}}

vec4 sample#{layerGroup}#{i}(vec2 uv, vec3 levelWeights,
                             Layer #{layerGroup}[#{lastLayerIndex#{layerGroup}} + 1])
{
  // All tile layers are the same. Sample from texture
#if (#{#{layerGroup}#{i}LayerType} == 0) // DefaultTileProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 1) // SingleImageProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 2) // ImageSequenceTileProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 3) // SizeReferenceTileProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 4) // TemporalTileProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 5) // TileIndexTileProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 6) // TileProviderByDate
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 7) // TileProviderByIndex
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 8) // TileProviderByLevel
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 9) // SolidColor
  return vec4(#{layerGroup}[#{i}].color, 1.0);
#elif (#{#{layerGroup}#{i}LayerType} == 10) // SpoutImageProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#elif (#{#{layerGroup}#{i}LayerType} == 11) // VideoTileProvider
  return texVal(#{layerGroup}[#{i}].pile, levelWeights, uv);
#endif

  return vec4(0.0, 0.0, 0.0, 1.0);
}

#endfor
#endfor

#define BlendModeDefault 0
#define BlendModeMultiply 1
#define BlendModeMultiplyMix 2
#define BlendModeAdd 3
#define BlendModeSubtract 4
#define BlendModeColor 5

#for id, layerGroup in layerGroups
#for i in 0..#{lastLayerIndex#{layerGroup}}

vec4 blend#{layerGroup}#{i}(vec4 currentColor, vec4 newColor, float blendFactor) {
#if (#{#{layerGroup}#{i}BlendMode} == BlendModeDefault)
  return blendNormal(currentColor, vec4(newColor.rgb, newColor.a * blendFactor));
#elif (#{#{layerGroup}#{i}BlendMode} == BlendModeMultiply)
  return blendMultiply(currentColor, newColor * blendFactor);
#elif (#{#{layerGroup}#{i}BlendMode} == BlendModeMultiplyMix)
  return blendMultiplyMix(currentColor, newColor, blendFactor);
#elif (#{#{layerGroup}#{i}BlendMode} == BlendModeAdd)
  return blendAdd(currentColor, newColor * blendFactor);
#elif (#{#{layerGroup}#{i}BlendMode} == BlendModeSubtract)
  return blendSubtract(currentColor, newColor * blendFactor);
#elif (#{#{layerGroup}#{i}BlendMode} == BlendModeColor)
    // Convert color to grayscale
  float gray = (newColor.r + newColor.g + newColor.b) / 3.0;

  vec3 hsvCurrent = rgb2hsv(currentColor.rgb);
  // Use gray from new color as value in hsv
  vec3 hsvNew = vec3(hsvCurrent.x, hsvCurrent.y, gray);
  vec3 rgbNew = hsv2rgb(hsvNew);

  vec4 color = blendNormal(currentColor, vec4(rgbNew, newColor.a * blendFactor));
  return color;
#endif
}

#endfor
#endfor

const int LayerAdjustmentTypeDefault = 0;
const int LayerAdjustmentTypeChroma = 1;
const int LayerAdjustmentTypeTransferFunction = 1;

#for id, layerGroup in layerGroups
#for i in 0..#{lastLayerIndex#{layerGroup}}

vec4 performAdjustment#{layerGroup}#{i}(vec4 currentColor, LayerAdjustment adjustment) {
#if (#{#{layerGroup}#{i}LayerAdjustmentType} == LayerAdjustmentTypeDefault)
  return currentColor;
#elif (#{#{layerGroup}#{i}LayerAdjustmentType} == LayerAdjustmentTypeChroma)
  if (distance(currentColor.rgb, adjustment.chromaKeyColor) <=
      adjustment.chromaKeyTolerance)
  {
    return vec4(0.0);
  }
  else {
    return currentColor;
  }
#elif (#{#{layerGroup}#{i}LayerAdjustmentType} == LayerAdjustmentTypeTransferFunction)
  return currentColor;
#else
  return currentColor;
#endif
}

#endfor
#endfor

float calculateUntransformedHeight(vec2 uv, vec3 levelWeights,
                                   Layer HeightLayers[NUMLAYERS_HEIGHTMAP])
{

  float height = 0;

  // The shader compiler will remove unused code when variables are multiplied by
  // a constant 0
#if !HEIGHTMAP_BLENDING_ENABLED
  levelWeights = DefaultLevelWeights;
#endif // HEIGHTMAP_BLENDING_ENABLED

  #for i in 0..#{lastLayerIndexHeightLayers}
  {
    vec4 color = sampleHeightLayers#{i}(uv, levelWeights, HeightLayers);
    color = performAdjustmentHeightLayers#{i}(color, HeightLayers[#{i}].adjustment);
    height = performLayerSettings(color.r, HeightLayers[#{i}].settings);
  }
  #endfor
  return height;
}

float calculateHeight(vec2 uv, vec3 levelWeights, Layer HeightLayers[NUMLAYERS_HEIGHTMAP])
{
  float height = 0;

  // The shader compiler will remove unused code when variables are multiplied by
  // a constant 0
#if !HEIGHTMAP_BLENDING_ENABLED
  levelWeights = DefaultLevelWeights;
#endif // HEIGHTMAP_BLENDING_ENABLED

  #for i in 0..#{lastLayerIndexHeightLayers}
  {
    vec4 color = sampleHeightLayers#{i}(uv, levelWeights, HeightLayers);
    color = performAdjustmentHeightLayers#{i}(color, HeightLayers[#{i}].adjustment);
    float untransformedHeight = color.r;

    TileDepthTransform transform = HeightLayers[#{i}].depthTransform;
    float heightSample =
      transform.depthScale * untransformedHeight + transform.depthOffset;
    if (heightSample > -100000) {
      height = performLayerSettings(heightSample, HeightLayers[#{i}].settings);
    }
  }
  #endfor
  return height;
}

vec4 calculateColor(vec4 currentColor, vec2 uv, vec3 levelWeights,
                    Layer ColorLayers[NUMLAYERS_COLORTEXTURE])
{
  vec4 color = currentColor;

  // The shader compiler will remove unused code when variables are multiplied by
  // a constant 0
#if !COLORTEXTURE_BLENDING_ENABLED
  levelWeights = DefaultLevelWeights;
#endif // COLORTEXTURE_BLENDING_ENABLED

  #for i in 0..#{lastLayerIndexColorLayers}
  {
    vec4 colorSample = sampleColorLayers#{i}(uv, levelWeights, ColorLayers);
    colorSample = performAdjustmentColorLayers#{i}(
      colorSample,
      ColorLayers[#{i}].adjustment
    );
    colorSample = performLayerSettings(colorSample, ColorLayers[#{i}].settings);

    color = blendColorLayers#{i}(color, colorSample, 1.0);
  }
  #endfor

  return color;
}

float gridDots(vec2 uv, vec2 gridResolution) {
  vec2 uvVertexSpace = fract((gridResolution) * uv) + 0.5;
  vec2 uvDotSpace = abs(2.0 * (uvVertexSpace - 0.5));
  return 1.0 - length(1.0 - uvDotSpace);
}

vec4 calculateDebugColor(vec2 uv, vec4 fragPos, vec2 vertexResolution) {
  vec2 uvVertexSpace = fract(vertexResolution * uv);
  vec3 colorUv = vec3(0.3 * uv.x, 0.3 * uv.y, 0.0);
  vec3 colorDistance = vec3(0.0, 0.0, min(0.4 * log(fragPos.w) - 3.9, 1.0));
  vec3 colorVertex = (1.0 - length(uvVertexSpace)) * vec3(0.5);
  vec3 colorSum = colorUv + colorDistance + colorVertex;
  return vec4(0.5 * colorSum, 1.0);
}

float tileResolution(vec2 tileUV, ChunkTile chunkTile) {
  vec2 heightResolution = textureSize(chunkTile.textureSampler, 0);
  vec2 uv = tileUVToTextureSamplePosition(chunkTile, tileUV);
  return gridDots(uv, heightResolution);
}

vec4 calculateNight(vec4 currentColor, vec2 uv, vec3 levelWeights,
                    Layer NightLayers[NUMLAYERS_NIGHTTEXTURE],
                    vec3 ellipsoidNormalCameraSpace, vec3 lightDirectionCameraSpace)
{
  vec4 color = currentColor;

  // The shader compiler will remove unused code when variables are multiplied by
  // a constant 0
#if !NIGHTTEXTURE_BLENDING_ENABLED
  levelWeights = DefaultLevelWeights;
#endif // NIGHTTEXTURE_BLENDING_ENABLED

  vec3 n = normalize(ellipsoidNormalCameraSpace);
  vec3 l = lightDirectionCameraSpace;
  float cosineFactor = clamp(dot(l, normalize(n + 0.20 * l)) * 3.0, 0.0, 1.0);

  #for i in 0..#{lastLayerIndexNightLayers}
  {
    vec4 colorSample = sampleNightLayers#{i}(uv, levelWeights, NightLayers);
    colorSample = performAdjustmentNightLayers#{i}(
      colorSample,
      NightLayers[#{i}].adjustment
    );
    colorSample = performLayerSettings(colorSample, NightLayers[#{i}].settings);

    float adjustedAlpha = cosineFactor * colorSample.a;
    // Filter to night side
    vec4 newColor = vec4(cosineFactor * colorSample.xyz, adjustedAlpha);

    color = blendNightLayers#{i}(color, newColor, adjustedAlpha);
  }
  #endfor

  return color;
}

vec4 calculateShadedColor(vec4 currentColor, vec3 ellipsoidNormalCameraSpace,
                          vec3 lightDirectionCameraSpace, vec3 viewDirectionCameraSpace,
                          float roughness, float ambientIntensity)
{
  vec3 shadedColor = currentColor.rgb * ambientIntensity;

  vec3 n = normalize(ellipsoidNormalCameraSpace);

  float power = orenNayarDiffuse(
    -lightDirectionCameraSpace,
    viewDirectionCameraSpace,
    ellipsoidNormalCameraSpace,
    roughness
  );

  vec3 l = lightDirectionCameraSpace;
  power = max(smoothstep(0.0, 0.1, max(dot(-l, n), 0.0)) * power, 0.0);

  vec4 color = vec4(shadedColor + currentColor.rgb * power, currentColor.a);
  return color;
}

vec4 calculateOverlay(vec4 currentColor, vec2 uv, vec3 levelWeights,
                      Layer Overlays[NUMLAYERS_OVERLAY])
{
  vec4 color = currentColor;

  // The shader compiler will remove unused code when variables are multiplied by
  // a constant 0
#if !OVERLAY_BLENDING_ENABLED
  levelWeights = DefaultLevelWeights;
#endif // OVERLAY_BLENDING_ENABLED

  #for i in 0..#{lastLayerIndexOverlays}
  {
    vec4 colorSample = sampleOverlays#{i}(uv, levelWeights, Overlays);
    colorSample = performAdjustmentOverlays#{i}(colorSample, Overlays[#{i}].adjustment);
    colorSample = performLayerSettings(colorSample, Overlays[#{i}].settings);

    color = blendNormal(color, colorSample);
    color = blendOverlays#{i}(color, colorSample, 1.0);
  }
  #endfor

  return color;
}

vec4 calculateWater(vec4 currentColor, vec2 uv, vec3 levelWeights,
                    Layer WaterMasks[NUMLAYERS_WATERMASK],
                    vec3 ellipsoidNormalCameraSpace, vec3 lightDirectionCameraSpace,
                    vec3 positionCameraSpace, out float reflectance)
{
  vec4 waterColor = vec4(0.0);

  // The shader compiler will remove unused code when variables are multiplied by
  // a constant 0
#if !WATERMASK_BLENDING_ENABLED
  levelWeights = DefaultLevelWeights;
#endif // WATERMASK_BLENDING_ENABLED

  #for i in 0..#{lastLayerIndexWaterMasks}
  {
    vec4 colorSample = sampleWaterMasks#{i}(uv, levelWeights, WaterMasks);
    colorSample = performAdjustmentWaterMasks#{i}(
      colorSample,
      WaterMasks[#{i}].adjustment
    );

    colorSample.a = performLayerSettings(colorSample.a, WaterMasks[#{i}].settings);
    waterColor = blendWaterMasks#{i}(waterColor, colorSample, 1.0);
  }
  #endfor

  vec3 directionToFragmentCameraSpace = normalize(positionCameraSpace - vec3(0.0));
  vec3 reflectionDirectionCameraSpace = reflect(
    lightDirectionCameraSpace,
    ellipsoidNormalCameraSpace
  );

  reflectance = waterColor.a;
  return currentColor;
}

#endif // _GLOBEBROWSING___TEXTURETILEMAPPING___GLSL_
