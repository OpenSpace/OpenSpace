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

#include "fragment.glsl"

#include "tile.glsl"
#include "texturetilemapping.glsl"
#include "tileheight.glsl"
#include "powerscaling/powerscaling_fs.glsl"

#define nDepthMaps #{nDepthMaps}

in Data {
  vec4 position;
  vec3 ellipsoidNormalCameraSpace;
  vec3 levelWeights;
  vec3 positionCameraSpace;
  vec3 posObjSpace;
  vec3 normalObjSpace;
  vec2 uv;
#if USE_ACCURATE_NORMALS
  vec3 ellipsoidTangentThetaCameraSpace;
  vec3 ellipsoidTangentPhiCameraSpace;
#endif // USE_ACCURATE_NORMALS
#if USE_ECLIPSE_SHADOWS
  vec3 positionWorldSpace;
#endif // USE_ECLIPSE_SHADOWS
#if SHADOW_MAPPING_ENABLED
  vec4 shadowCoords;
#endif // SHADOW_MAPPING_ENABLED
#if nDepthMaps > 0
  vec4 positionsLightspace[nDepthMaps];
#endif // nDepthMaps > 0
} in_data;

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
#endif // SHOW_HEIGHT_RESOLUTION

uniform vec3 lightDirectionCameraSpace;
uniform vec3 lightDirectionObjSpace;
uniform mat4 modelViewTransform;
uniform float ringSize;

#if PERFORM_SHADING
uniform float orenNayarRoughness;
uniform float ambientIntensity;
#endif // PERFORM_SHADING

#if SHADOW_MAPPING_ENABLED
#if USE_RING_SHADOWS
// Color of the rings
uniform sampler1D ringTextureColor;
// Transparency of the rings
uniform sampler1D ringTextureTransparency;
uniform vec2 textureOffset;
#endif // USE_RING_SHADOWS
#endif // SHADOW_MAPPING_ENABLED

#define USE_DEPTHMAP_SHADOWS #{useDepthmapShadows}

#if USE_ECLIPSE_SHADOWS

#define NSEclipseShadowsMinusOne #{nEclipseShadows}
#define NSEclipseShadows (NSEclipseShadowsMinusOne + 1)

/*******************************************************************************
 ***** ALL CALCULATIONS FOR ECLIPSE ARE IN METERS AND IN WORLD SPACE SYSTEM ****
 *******************************************************************************/
struct ShadowRenderingStruct {
  double xu;
  double xp;
  double rs;
  double rc;
  dvec3 sourceCasterVec;
  dvec3 casterPositionVec;
  bool isShadowing;
};

// Eclipse shadow data
// JCC: Remove and use dictionary to
// decides the number of shadows
uniform ShadowRenderingStruct shadowDataArray[NSEclipseShadows];
uniform int shadows;
uniform bool hardShadows;


vec4 calcShadow(ShadowRenderingStruct shadowInfoArray[NSEclipseShadows], dvec3 position,
                bool ground)
{
  #for i in 0..#{nEclipseShadows}
    if (shadowInfoArray[#{i}].isShadowing) {
      dvec3 pc = shadowInfoArray[#{i}].casterPositionVec - position;
      dvec3 scNorm = shadowInfoArray[#{i}].sourceCasterVec;
      dvec3 pcProj = dot(pc, scNorm) * scNorm;
      dvec3 d = pc - pcProj;

      float lengthD = float(length(d));
      double lengthPcProj = length(pcProj);

      float rPenumbra = float(
        shadowInfoArray[#{i}].rc *
        (lengthPcProj + shadowInfoArray[#{i}].xp) / shadowInfoArray[#{i}].xp
      );
      float rUmbra = float(
        shadowInfoArray[#{i}].rc *
        (shadowInfoArray[#{i}].xu - lengthPcProj) / shadowInfoArray[#{i}].xu
      );

      if (lengthD < rUmbra) {
        // umbra
        if (ground) {
#if USE_ECLIPSE_HARD_SHADOWS
          return vec4(0.2, 0.2, 0.2, 1.0);
#else // USE_ECLIPSE_HARD_SHADOWS
          // butterworthFunc
          return vec4(vec3(sqrt(rUmbra / (rUmbra + pow(lengthD, 2.0)))), 1.0);
#endif // USE_ECLIPSE_HARD_SHADOWS
        }
        else {
#if USE_ECLIPSE_HARD_SHADOWS
          return vec4(0.5, 0.5, 0.5, 1.0);
#else // USE_ECLIPSE_HARD_SHADOWS
          return vec4(vec3(lengthD / rPenumbra), 1.0);
#endif // USE_ECLIPSE_HARD_SHADOWS
        }
      }
      else if (lengthD < rPenumbra) {
        // penumbra
#if USE_ECLIPSE_HARD_SHADOWS
        return vec4(0.5, 0.5, 0.5, 1.0);
#else // USE_ECLIPSE_HARD_SHADOWS
        return vec4(vec3(lengthD / rPenumbra), 1.0);
#endif // USE_ECLIPSE_HARD_SHADOWS
      }
    }
  #endfor
  return vec4(1.0);
}
#endif

float rayPlaneIntersection(vec3 rayOrigin, vec3 rayDirection, vec3 planePoint,
                           vec3 planeNormal)
{
  float denom = dot(planeNormal, rayDirection);

  // Check if ray is parallel to plane (or nearly parallel)
  if (abs(denom) < 1e-6) {
      return -1.0; // No intersection or ray lies in plane
  }

  vec3 p0l0 = planePoint - rayOrigin;
  float t = dot(p0l0, planeNormal) / denom;

  // Return negative if intersection is behind ray origin
  return t >= 0.0  ?  t  :  -1.0;
}

uniform float opacity;

#if USE_DEPTHMAP_SHADOWS
#if nDepthMaps > 0
  uniform sampler2D light_depth_maps[nDepthMaps];
#endif // nDepthMaps > 0
#endif // USE_DEPTHMAP_SHADOWS


Fragment getFragment() {
  Fragment frag;
  frag.color = vec4(0.3, 0.3, 0.3, 1.0);

  vec3 normal = normalize(in_data.ellipsoidNormalCameraSpace);

#if USE_ACCURATE_NORMALS
  normal = getTileNormal(
    in_data.uv,
    in_data.levelWeights,
    normalize(in_data.ellipsoidNormalCameraSpace),
    normalize(in_data.ellipsoidTangentThetaCameraSpace),
    normalize(in_data.ellipsoidTangentPhiCameraSpace)
  );
#endif /// USE_ACCURATE_NORMALS

#if USE_COLORTEXTURE
  frag.color = calculateColor(frag.color, in_data.uv, in_data.levelWeights, ColorLayers);
#endif // USE_COLORTEXTURE

#if USE_WATERMASK
  float waterReflectance = 0.0;
  frag.color = calculateWater(
    frag.color,
    in_data.uv,
    in_data.levelWeights,
    WaterMasks,
    normal,
    lightDirectionCameraSpace, // Should already be normalized
    in_data.positionCameraSpace,
    waterReflectance
  );
#endif // USE_WATERMASK

#if USE_NIGHTTEXTURE
  frag.color = calculateNight(
    frag.color,
    in_data.uv,
    in_data.levelWeights,
    NightLayers,
    normalize(in_data.ellipsoidNormalCameraSpace),
    lightDirectionCameraSpace // Should already be normalized
  );
#endif // USE_NIGHTTEXTURE

#if PERFORM_SHADING
  vec3 preShadedColor = frag.color.rgb;
  frag.color = calculateShadedColor(
    frag.color,
    normal,
    lightDirectionCameraSpace,
    normalize(in_data.positionCameraSpace),
    orenNayarRoughness,
    ambientIntensity
  );
#endif // PERFORM_SHADING

#if USE_ECLIPSE_SHADOWS
  frag.color *= calcShadow(shadowDataArray, dvec3(in_data.positionWorldSpace), true);
#endif // USE_ECLIPSE_SHADOWS

#if USE_OVERLAY
  frag.color = calculateOverlay(frag.color, in_data.uv, in_data.levelWeights, Overlays);
#endif // USE_OVERLAY

#if SHOW_HEIGHT_INTENSITIES
  frag.color.rgb *= vec3(0.1);

  float untransformedHeight = getUntransformedTileVertexHeight(
    in_data.uv,
    in_data.levelWeights
  );
  float contourLine = fract(10.0 * untransformedHeight) > 0.98  ?  1.0  :  0.0;
  frag.color.r += untransformedHeight;
  frag.color.b = contourLine;
#endif // SHOW_HEIGHT_INTENSITIES

#if SHOW_HEIGHT_RESOLUTION
  frag.color +=
    0.0001 * calculateDebugColor(in_data.uv, in_data.position, vertexResolution);
  #if USE_HEIGHTMAP
    frag.color.r = min(frag.color.r, 0.8);
    frag.color.r +=
      tileResolution(in_data.uv, HeightLayers[0].pile.chunkTile0) > 0.9  ?  1.0  :  0.0;
  #endif // USE_HEIGHTMAP
#endif // SHOW_HEIGHT_RESOLUTION

  // Other data
#if USE_WATERMASK
  // Water reflectance is added to the G-Buffer.
  frag.gNormal.w = waterReflectance;
#else
  frag.gNormal.w = 0.0;
#endif
  // Normal is written View Space (Including SGCT View Matrix).
  frag.gNormal.xyz = normal;

  if (dot(in_data.positionCameraSpace, vec3(1.0)) != 0.0) {
    frag.gPosition = vec4(in_data.positionCameraSpace, 1.0); // in Camera Rig Space
  }
  else {
    frag.gPosition = vec4(1.0); // in Camera Rig Space
  }

  frag.depth = in_data.position.w;

#if SHOW_CHUNK_EDGES
  const float BorderSize = 0.005;
  const vec3 BorderColor = vec3(1.0, 0.0, 0.0);

  vec2 uvOffset = in_data.uv - vec2(0.5);
  float thres = 0.5 - BorderSize * 0.5;
  bool isBorder = abs(uvOffset.x) > thres || abs(uvOffset.y) > thres;
  if (isBorder) {
    frag.color.rgb += BorderColor;
  }
#endif // SHOW_CHUNK_EDGES

#if (SHADOW_MAPPING_ENABLED && PERFORM_SHADING && USE_RING_SHADOWS)
  // 0.0 is full shadow, 1.0 is no shadow
  float shadow = 1.0;
  // Light through rings is colored, default full white
  vec3 lightColor = vec3(1.0);
  // Calculate ring shadow by projecting ring texture directly onto surface
  // Assume ring lies in the XZ plane (Y=0) in object space
  vec3 surfaceToSun = -normalize(lightDirectionObjSpace); // Use world coordinates
  vec3 p = in_data.posObjSpace;
  const vec3 RingPlaneNormal = vec3(0.0, 0.0, 1.0);

  if (abs(surfaceToSun.y) > 1e-8 &&
      dot(in_data.normalObjSpace, lightDirectionObjSpace) < 0.0)
  {
    float t = rayPlaneIntersection(p, surfaceToSun, vec3(0.0), RingPlaneNormal);

    vec3 ringIntersection = p + t * surfaceToSun;

    // Calculate distance from ring center
    float tx = length(ringIntersection.xy) / ringSize;
    // See advanced_rings_fs.glsl for explanation of textureOffset
    float texCoord = (tx - textureOffset.x) / (textureOffset.y - textureOffset.x);

    if (texCoord >= 0.0 && texCoord <= 1.0) {
      // Sample ring transparency texture
      float ringOpacity = texture(ringTextureTransparency, texCoord).r;

      // Increase the shadow darkness factor with low angle to simulate the light having
      // to pass through more material
      float angleFactor = clamp(abs(-dot(RingPlaneNormal, surfaceToSun)) / 2.0, 0.0, 0.3);
      // Calculate shadow factor based on ring opacity
      shadow = clamp(ringOpacity + angleFactor, 0.05, 1.0);
      lightColor = texture(ringTextureColor, texCoord).rgb;
    }
  }

  // Blend the light color passing through the rings with the pre-shaded color
  frag.color.rgb = mix(
    preShadedColor * lightColor * ambientIntensity,
    frag.color.rgb,
    shadow
  );
#endif // (SHADOW_MAPPING_ENABLED && PERFORM_SHADING && USE_RING_SHADOWS)

#if USE_DEPTHMAP_SHADOWS && nDepthMaps > 0
  const float Bias = 0.005;
  const int Size = 3;
  const float Norm = pow(2.0 * Size + 1.0, 2.0);
  float accum = 1.0;
  for (int idx = 0; idx < nDepthMaps; idx++) {
    vec2 ssz = 1.0 / textureSize(light_depth_maps[idx], 0);
    vec4 pls = in_data.positions_lightspace[idx];
    vec3 coords = 0.5 + 0.5 * pls.xyz / pls.w;
    for (int x = -Size; x <= Size; x++) {
      for (int y = -Size; y <= Size; y++) {
        float depth = texture(light_depth_maps[idx], coords.xy + vec2(x, y) * ssz).r;
        // inside of the far plane of the frustum
        if (coords.z < 1.0) {
          accum -= float(depth < coords.z - Bias) / Norm;
        }
        else {
          // outside of the far plane of the frustum, typically happens with long shadows
          // cast on the surface of a globe
          accum -= float(depth < 1.0) / Norm;
        }
      }
    }
  }

  // @TODO (2026-01-08, abock) This should become a property at some point. It determines
  // how much of the ground of the planet is visible in the shadowed region
  const float Ambience = 0.2;
  frag.color.xyz *= mix(max(0.0, accum), 1.0, Ambience);
#endif // USE_DEPTHMAP_SHADOWS && nDepthMaps > 0

  frag.color.a *= opacity;
  frag.color = clamp(frag.color, 0.0, 1.0);
  return frag;
}
