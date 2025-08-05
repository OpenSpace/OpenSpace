/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#version __CONTEXT__

layout (location = 0) out vec4 finalColor;

in vec2 UV;

uniform sampler2D mainColorTexture;

uniform float aspectRatio;
uniform float chromaticDistortion;
uniform float colorGradientAlpha;
uniform float dustAlpha;
uniform sampler2D dustTexture;
uniform float ghostsAlpha;
uniform float ghostsDispersal;
uniform int ghostsNumber;
uniform float ghostsWeight;
uniform sampler2D gradientTexture;
uniform float haloAlpha;
uniform float haloWidth;
uniform float haloWeight;
uniform float starAlpha;
uniform float starRotation;
uniform sampler2D starTexture;
uniform float thesholdBias;
uniform float thesholdScale;

// https://www.iquilezles.org/www/articles/functions/functions.htm
float cubicPulse( float c, float w, float x ){
    x = abs(x - c);
    if( x>w ) return 0.0;
    x /= w;
    return 1.0 - x*x*(3.0-2.0*x);
}

// https://gist.github.com/ayamflow/c06bc0c8a64f985dd431bd0ac5b557cd
vec2 rotateUV(vec2 uv, float rotation) {
    float mid = 0.5;
    return vec2(
        cos(rotation) * (uv.x - mid) + sin(rotation) * (uv.y - mid) + mid,
        cos(rotation) * (uv.y - mid) - sin(rotation) * (uv.x - mid) + mid
    );
}

vec3 textureDistorted(sampler2D tex, vec2 texcoord, vec2 direction, vec3 distortion) {
    return vec3(
        texture(tex, texcoord + direction * distortion.r).r,
        texture(tex, texcoord + direction * distortion.g).g,
        texture(tex, texcoord + direction * distortion.b).b
    );
}

vec3 sampleScaledBiasedTexture(sampler2D tex, vec2 uv){
    return max(vec3(0.0), texture( mainColorTexture, uv ).xyz + thesholdBias) * thesholdScale;
}

vec3 sampleScaledBiasedTexture(sampler2D tex, vec2 uv, vec2 direction, vec3 distortion){
    return max(vec3(0.0), textureDistorted( mainColorTexture, uv, direction, distortion ).xyz + thesholdBias) * thesholdScale;
}

// http://john-chapman-graphics.blogspot.com/2013/02/pseudo-lens-flare.html
// https://john-chapman.github.io/2017/11/05/pseudo-lens-flare.html
void main(){

    vec3 color = vec3(0.0);

    vec2 texelSize = 1.0 / vec2(textureSize(mainColorTexture, 0));
    
    vec2 texcoord = -UV + vec2(1.0); // flip image
    vec2 ghostVec = (vec2(0.5) - texcoord) * ghostsDispersal;
    vec3 distortion = vec3(-texelSize.x * chromaticDistortion, 0.0, texelSize.x * chromaticDistortion);
    vec2 direction = normalize(ghostVec);
    vec2 circVec = vec2(ghostVec.x*aspectRatio, ghostVec.y);
    float offset = 0.5 * (1.0 - aspectRatio);
    float offsetY = 0.5 * (1.0 - 1.0/aspectRatio);

    // ghosts
    for (int i = 0; i < ghostsNumber; ++i) {
        vec2 offset = fract(texcoord + ghostVec * float(i));
        float dist = distance(offset, vec2(0.5));

        // falloff from center
        float weight = smoothstep(0.0, 0.75, dist)
        weight = pow(1.0 - weight, ghostsWeight);

        color += ghostsAlpha * sampleScaledBiasedTexture(mainColorTexture, offset, direction, distortion) * weight;
    }

    // halo
    vec3 haloColor = vec3(0.0);
    vec2 haloVec = vec2(0.5) - UV;
    haloVec.x *= aspectRatio;
    haloVec = normalize(haloVec);
    haloVec.x /= aspectRatio;
    haloVec *= haloWidth;
    float d = distance( vec2(UV.x*aspectRatio+offset,UV.y), vec2(0.5) );
    float weight = cubicPulse( haloWidth, haloWeight, d );
    haloColor += 3.0 * haloAlpha * sampleScaledBiasedTexture(mainColorTexture, UV + haloVec, direction, distortion).xyz * weight;
    
    // star
    vec2 starUV = vec2(UV.x , UV.y / aspectRatio + offsetY);
    starUV = rotateUV(starUV, starRotation);
    haloColor *= starAlpha * textureDistorted(starTexture, starUV, direction, distortion).xyz;
    color += haloColor;

    // color gradient
    vec2 circUV = vec2(UV.x , UV.y / aspectRatio + offsetY);
    float xy = length(vec2(0.5) - circUV) / length(vec2(0.5));
    vec3 gradientColor = texture(gradientTexture, vec2(xy,0.0) ).xyz;
    color *= mix( vec3(1.0), gradientColor, colorGradientAlpha );

    // dust
    color *= mix( vec3(1.0), texture(dustTexture, UV).xyz, dustAlpha );

    finalColor = vec4(color.rgb,1.0);

    finalColor.r *= 0.75;
    finalColor.g *= 0.95;
    finalColor.b *= 1.00;

}

