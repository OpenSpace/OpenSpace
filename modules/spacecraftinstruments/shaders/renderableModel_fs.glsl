/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
uniform vec3 baseColor;
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
  vec4 textureColor;
  if (has_texture_diffuse) {
      textureColor = texture(baseTexture, vs_st);
  }
  else {
      textureColor = vec4(baseColor, 1.0);
  }
  vec4 projectionColor = texture(projectionTexture, vs_st);
  if (projectionColor.a > 0.0) {
    textureColor.rgb = mix(
      textureColor.rgb,
      projectionColor.rgb,
      projectionFading * projectionColor.a
    );
  }
  
  vec3 diffuseAlbedo = textureColor.rgb;

  Fragment frag;
  if (performShading) {
    // Some of these values could be passed in as uniforms
    const vec3 lightColorAmbient = vec3(1.0);
    const vec3 lightColor = vec3(1.0);
    
    vec3 n = normalize(vs_normalViewSpace);
    vec3 l = directionToSunViewSpace;
    vec3 c = normalize(vs_positionCameraSpace.xyz);
    vec3 r = reflect(l, n);

    float diffuseCosineFactor = dot(n,l);
    float specularCosineFactor = dot(c,r);

    vec3 ambientColor = ambientIntensity * lightColorAmbient * diffuseAlbedo;
    vec3 diffuseColor = 
        diffuseIntensity * lightColor * diffuseAlbedo * max(diffuseCosineFactor, 0.0);
    vec3 specularColor = specularIntensity * lightColor * specularAlbedo *
        pow(max(specularCosineFactor, 0.0), specularPower);

    frag.color.rgb = ambientColor + diffuseColor + specularColor;
  }
  else {
    frag.color.rgb = diffuseAlbedo;
  }

  frag.color.a = 1.0;
  frag.depth = vs_depth;
  // frag.depth = 0.0;
  return frag;
}
