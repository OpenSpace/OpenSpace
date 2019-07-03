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

uniform int numberOfSamples;
uniform int filterStep;
uniform sampler2DMS filterImage;
uniform sampler2D filterFirstPass;

// Gaussian Weights from OpenGL SuperBible 7 ed.
const float weights[] = float[](0.0024499299678342,
                                0.0043538453346397,
                                0.0073599963704157,
                                0.0118349786570722,
                                0.0181026699707781,
                                0.0263392293891488,
                                0.0364543006660986,
                                0.0479932050577658,
                                0.0601029809166942,
                                0.0715974486241365,
                                0.0811305381519717,
                                0.0874493212267511,
                                0.0896631113333857,
                                0.0874493212267511,
                                0.0811305381519717,
                                0.0715974486241365,
                                0.0601029809166942,
                                0.0479932050577658,
                                0.0364543006660986,
                                0.0263392293891488,
                                0.0181026699707781,
                                0.0118349786570722,
                                0.0073599963704157,
                                0.0043538453346397,
                                0.0024499299678342);

const float weights2[] = float[](0.077847, 
                                 0.123317, 
                                 0.077847, 
                                 0.123317, 
                                 0.195346, 
                                 0.123317,
                                 0.077847,
                                 0.123317,
                                 0.077847);

void main(void)
{
    vec4 color = vec4(0.0);
    // Transpose the image so the filter can be applied on X and Y
    ivec2 P = ivec2(gl_FragCoord.yx) - ivec2(0, weights.length() >> 1);
    float origAlpha = 1.0;

    for (int i = 0; i < weights.length(); i++)
    {   
        if (filterStep == 1) {
            vec4 tmpColor = vec4(0.0);
            for (int s = 0; s < numberOfSamples; ++s) {
                tmpColor += vec4(texelFetch(filterImage, P + ivec2(0, i), 0).rgb, s) * weights[i];    
            }
            tmpColor /= numberOfSamples;
            color += tmpColor;
            //color += vec4(texelFetch(filterImage, P + ivec2(0, i), 0).rgb, 1) * weights[i];
            origAlpha = color.a;
        } else if (filterStep == 2) {
            color += vec4(texelFetch(filterFirstPass, P + ivec2(0, i), 0).rgb, 0) * weights[i];
        } 
    }

    finalColor = vec4(color.rgb, 1.0);
}
