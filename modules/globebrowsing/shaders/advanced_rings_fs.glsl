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

#include "powerscaling/powerscaling_fs.glsl"
#include "fragment.glsl"
#include "ellipsoid.glsl"

#define NSSamplesMinusOne #{nShadowSamples}
#define NSSamples (NSSamplesMinusOne + 1)

in Data {
  vec3 posObj;
  vec2 texCoords;
  vec3 normal;
  float screenSpaceDepth;
  vec4 shadowCoords;
} in_data;

uniform sampler1D textureForwards;
uniform sampler1D textureBackwards;
uniform sampler1D textureUnlit;
uniform sampler1D textureColor;
uniform sampler1D textureTransparency;
uniform vec2 textureOffset;
uniform float colorFilterValue;
uniform vec3 sunPosition;
uniform vec3 sunPositionObj;
uniform vec3 camPositionObj;
uniform float nightFactor;
uniform float zFightingPercentage;
uniform float opacity;
uniform vec3 ellipsoidRadii;


Fragment getFragment() {
  // Moving the origin to the center
  vec2 st = (in_data.texCoords - vec2(0.5)) * 2.0;

  // The length of the texture coordinates vector is our distance from the center
  float radius = length(st);

  // We only want to consider ring-like objects so we need to discard everything else
  if (radius > 1.0) {
    discard;
  }

  // Remapping the texture coordinates
  // Radius \in [0,1],  texCoords \in [textureOffset.x, textureOffset.y]
  // textureOffset.x -> 0
  // textureOffset.y -> 1
  float texCoords = (radius - textureOffset.x) / (textureOffset.y - textureOffset.x);
  if (texCoords < 0.0 || texCoords > 1.0) {
    discard;
  }

  vec4 colorBckwrd = texture(textureBackwards, texCoords);
  vec4 colorFwrd = texture(textureForwards, texCoords);
  vec4 colorMult = texture(textureColor, texCoords);
  float transparency = 1.0 - texture(textureTransparency, texCoords).r;

  float lerpFactor = dot(camPositionObj, sunPositionObj);

  // Jon Colors:
  // vec4 diffuse = mix(
  //   colorFwrd * vec4(1, 0.88, 0.82, 1.0),
  //   colorBckwrd * vec4(1, 0.88, 0.82, 1.0),
  //   lerpFactor
  // );
  vec4 diffuse = mix(colorFwrd, colorBckwrd, lerpFactor) * colorMult;
  diffuse.a = colorFilterValue * transparency;
  float colorValue = length(diffuse.rgb) / 0.57735026919;
  if (colorValue < 0.001) {
    discard;
  }

  // Check if ray from fragment to sun intersects the ellipsoid (globe)
  // This creates more accurate shadowing for rings
  bool intersectsGlobe = rayIntersectsEllipsoid(
    in_data.posObj,
    sunPositionObj,
    vec3(0.0),
    ellipsoidRadii
  );

  // shadow == 1.0 means it is not in shadow
  float shadow = intersectsGlobe  ?  0.05  :  1.0;

  // The normal for the one plane depends on whether we are dealing
  // with a front facing or back facing fragment
  // The plane is oriented on the xz plane
  // WARNING: This might not be the case for Uranus
  vec3 normal = gl_FrontFacing  ?  vec3(-1.0, 0.0, 0.0)  :  vec3(1.0, 0.0, 0.0);

  // Reduce the color of the fragment by the user factor
  // if we are facing away from the Sun
  if (dot(sunPosition, normal) < 0.0) {
    diffuse.rgb =
      vec3(1.0, 0.97075, 0.952) *  texture(textureUnlit, texCoords).rgb * nightFactor;
  }

  Fragment frag;
  frag.color = diffuse * shadow;
  frag.color.a *= opacity + (1.0 - shadow) * 0.5;
  frag.depth = in_data.screenSpaceDepth;
  frag.gPosition = vec4(1e30, 1e30, 1e30, 1.0);
  frag.gNormal = vec4(normal, 1.0);
  return frag;
}
