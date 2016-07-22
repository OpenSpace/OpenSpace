/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
uniform float fading;
uniform bool performShading = true;

uniform vec3 directionToSunViewSpace;

uniform sampler2D texture1;

// Input from the vertex shader
in vec2 vs_st;
in vec3 vs_normalViewSpace;
in vec3 vs_cameraDirectionViewSpace;
in vec4 vs_position;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
    vec4 textureSample = texture(texture1, vs_st);
    
    vec3 colorDiffuse = textureSample.rgb;
    vec3 colorSpecular = vec3(1);

    vec3 color;

    if (performShading) {
        vec3 lightColorAmbient = vec3(1);    
        vec3 lightColor = vec3(1);    
        
        vec3 n = normalize(vs_normalViewSpace);
        vec3 l = directionToSunViewSpace;
        vec3 c = normalize(vs_cameraDirectionViewSpace);
        vec3 r = reflect(l, n);

        float ambientIntensity = 0.2;
        float diffuseIntensity = 1;
        float specularIntensity = 1;

        float diffuseCosineFactor = dot(n,l);
        float specularCosineFactor = dot(c,r);
        float specularPower = 10;

        vec3 ambient = ambientIntensity * lightColorAmbient * colorDiffuse;
        vec3 diffuse = specularIntensity * lightColor * colorDiffuse * max(diffuseCosineFactor, 0);
        vec3 specular = specularIntensity * lightColor * colorSpecular * pow(max(specularCosineFactor, 0), specularPower);

        color = ambient + diffuse + specular;
    }
    else {
        color = colorDiffuse;
    }

    float alpha = fading * transparency;

    Fragment frag;
    frag.color = vec4(color, alpha);
    frag.depth = vs_position.w;

    return frag;
}
