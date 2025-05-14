/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
in vec3 vs_normalViewSpace;
in float vs_depth;
in vec4 vs_positionCameraSpace;

uniform bool has_texture_diffuse;
uniform sampler2D baseTexture;
uniform vec4 baseColor;
uniform sampler2D projectionTexture;
uniform bool performShading;
uniform float projectionFading;
uniform vec3 directionToSunViewSpace;

const vec3 specularAlbedo = vec3(1.0);

const float ambientIntensity = 0.15;
const float diffuseIntensity = 1.0;
const float specularIntensity = 0.0;
const float specularPower = 100.0;


Fragment getFragment() {
  Fragment frag;
  frag.depth = vs_depth;
  frag.gPosition = vs_positionCameraSpace;
  frag.gNormal = vec4(vs_normalViewSpace, 0.0);
  frag.disableLDR2HDR = true;
  frag.color.a = 1.0;

  // Base color
  vec4 textureColor;
  if (has_texture_diffuse) {
    textureColor = texture(baseTexture, vs_st);
  }
  else {
    textureColor = vec4(baseColor.rgb, 1.0);
  }

  // Mix base color with the projection images
  vec4 projectionColor = texture(projectionTexture, vs_st);
  if (projectionColor.a > 0.0) {
    textureColor.rgb = mix(
      textureColor.rgb,
      projectionColor.rgb,
      projectionFading * projectionColor.a
    );
  }

  vec3 diffuseAlbedo = textureColor.rgb;

  if (performShading) {
    // Could be seperated into ambinet, diffuse and specular and passed in as uniforms
    const vec3 lightColor = vec3(1.0);
    const float specularPower = 100.0;

    // Ambient light
    vec3 ambientColor = ambientIntensity * lightColor * diffuseAlbedo;

    // Diffuse light
    vec3 normal = normalize(vs_normalViewSpace);
    vec3 lightDirection = directionToSunViewSpace;
    float diffuseFactor =  max(dot(normal, lightDirection), 0.0);
    vec3 diffuseColor =
      diffuseIntensity * lightColor * diffuseFactor * diffuseAlbedo;

    // Specular light
    vec3 viewDirection = normalize(vs_positionCameraSpace.xyz);
    vec3 reflectDirection = reflect(lightDirection, normal);
    float specularFactor =
      pow(max(dot(viewDirection, reflectDirection), 0.0), specularPower);
    vec3 specularColor =
      specularIntensity * lightColor * specularFactor * specularAlbedo;

    // Total light
    frag.color.rgb = ambientColor + diffuseColor + specularColor;
  }
  else {
    frag.color.rgb = diffuseAlbedo;
  }

  return frag;
}
