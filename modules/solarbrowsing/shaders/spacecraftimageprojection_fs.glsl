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

const int MaxSpacecraftObservatories = 7;

in Data {
  vec4 positionScreenSpace;
  vec3 vUv[MaxSpacecraftObservatories];
} in_data;

uniform int numSpacecraftCameraPlanes;
uniform dvec3 planePositionSpacecraft[MaxSpacecraftObservatories];
uniform sampler1D lut[MaxSpacecraftObservatories];
uniform sampler2D imageryTexture[MaxSpacecraftObservatories];
uniform bool hasLut[MaxSpacecraftObservatories];
uniform float contrastValue[MaxSpacecraftObservatories];
uniform float gammaValue[MaxSpacecraftObservatories];
uniform float imageSize[MaxSpacecraftObservatories];
uniform bool isEnabled[MaxSpacecraftObservatories];
uniform bool isCoronaGraph[MaxSpacecraftObservatories];
uniform float scale[MaxSpacecraftObservatories];
uniform vec2 centerPixel[MaxSpacecraftObservatories];
uniform float opacity;

const float HalfSunRadius = 1391600000 * 0.5;

float contrast(float intensity, int i) {
  return min(
    clamp(0.5 + (intensity - 0.5) * (1.0 + contrastValue[i] / 10.0), 0.0, 1.0),
    sqrt(intensity) + intensity
  );
}

Fragment getFragment() {
  vec4 NoDataColor = vec4(vec3(0.3), 1.0);
  vec4 outColor = NoDataColor;
  bool renderSurface = true;

  for (int i = 0; i < numSpacecraftCameraPlanes; i++) {
    if (isCoronaGraph[i] || !isEnabled[i]) {
      continue;
    }

    if (planePositionSpacecraft[i].z < in_data.vUv[i].z) {
      vec3 uv = in_data.vUv[i].xyz;
      uv /= (HalfSunRadius / scale[i]) * 2.0;
      uv += 0.5;

      uv.x += (centerPixel[i].x / HalfSunRadius) / 2.0;
      uv.y -= (centerPixel[i].y /  HalfSunRadius) / 2.0;

      float intensityOrg = texture(imageryTexture[i], vec2(uv.x, 1.0 - uv.y)).r;
      intensityOrg = contrast(intensityOrg, i);

      vec4 res;
      if (hasLut[i]) {
          res = texture(lut[i], intensityOrg);
      }
      else {
          res = vec4(intensityOrg, intensityOrg, intensityOrg, 1.0);
      }

      res.r = pow(res.r, gammaValue[i]);
      res.g = pow(res.g, gammaValue[i]);
      res.b = pow(res.b, gammaValue[i]);

      // @TODO (anden 2026-03-05): After discussion with Abock we've decided to remove
      // the blending since two spacecrafts will probably not display images at with
      // the same timestamp. Thus, we would blend two different timestamps which makes
      // little to no sense
      // if (outColor == NoDataColor) { // Not initialized
      //     outColor = res;
      // }
      // else {
      //     // Blend between
      //     float factor = smoothstep(0.5, 1.0 - uv.x, uv.z);
      //     float factor2 = smoothstep(0.5, uv.x, uv.z);
      //     outColor = mix(outColor, res, factor + factor2);
      // }

      // We'll always project the color of the last spacecraft for this pixel. The order
      // of the spacecrafts is determined by the order they are loaded by the renderable
      outColor = res;
      renderSurface = false;
    }
  }

  if (renderSurface) {
    // Arbitrary default shading
    outColor = NoDataColor;
  }

  outColor.a *= opacity;

  Fragment frag;
  frag.color = outColor;
  frag.depth = in_data.positionScreenSpace.w;
  return frag;
}
