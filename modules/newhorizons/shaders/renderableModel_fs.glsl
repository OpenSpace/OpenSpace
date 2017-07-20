/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2017                                                             *
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
in vec4 vs_positionScreenSpace;
in vec4 vs_positionCameraSpace;

uniform sampler2D baseTexture;
uniform sampler2D projectionTexture;

uniform bool _performShading;
uniform float _projectionFading;
uniform vec3 directionToSunViewSpace;


Fragment getFragment() {
    vec4 textureColor = texture(baseTexture, vs_st);
    const vec4 projectionColor = texture(projectionTexture, vs_st);
    if (projectionColor.a > 0.0) {
        textureColor.rgb = mix(
            textureColor.rgb,
            projectionColor.rgb,
            _projectionFading * projectionColor.a
        );
    }
    
    const vec3 diffuseAlbedo = textureColor.rgb;
    const vec3 specularAlbedo = vec3(1.0);

    Fragment frag;
    if (_performShading) {
        // Some of these values could be passed in as uniforms
        const vec3 lightColorAmbient = vec3(1.0);
        const vec3 lightColor = vec3(1.0);
        
        const vec3 n = normalize(vs_normalViewSpace);
        const vec3 l = directionToSunViewSpace;
        const vec3 c = normalize(vs_positionCameraSpace.xyz);
        const vec3 r = reflect(l, n);

        const float ambientIntensity = 0.15;
        const float diffuseIntensity = 1.0;
        const float specularIntensity = 0.0;

        const float diffuseCosineFactor = dot(n,l);
        const float specularCosineFactor = dot(c,r);
        const float specularPower = 100;

        const vec3 ambientColor = ambientIntensity * lightColorAmbient * diffuseAlbedo;
        const vec3 diffuseColor = 
            diffuseIntensity * lightColor * diffuseAlbedo * max(diffuseCosineFactor, 0.0);
        const vec3 specularColor = specularIntensity * lightColor * specularAlbedo *
            pow(max(specularCosineFactor, 0.0), specularPower);

        frag.color.rgb = ambientColor + diffuseColor + specularColor;
    }
    else {
        frag.color.rgb = diffuseAlbedo;
    }

    frag.color.a = 1.0;
    frag.depth = vs_positionScreenSpace.w;
    // frag.depth = 0.0;
    return frag;
}
