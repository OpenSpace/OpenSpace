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
uniform int maxResX;
uniform int maxResY;
uniform sampler2DMS msaaTexture;

// Gaussian Weights from OpenGL SuperBible 7 ed.
const float weights2[] = float[](0.0024499299678342,
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

// Gaussian weights calculated by http://dev.theomader.com/gaussian-kernel-calculator/
// sigma = 4.4625, kernel size = 5
const float weights3[] = float[](0.190079,
                                0.204885,
                                0.210072,
                                0.204885,
                                0.190079);

// Gaussian weights calculated by http://dev.theomader.com/gaussian-kernel-calculator/
// sigma = 8, kernel size = 45
const float weights[] = float[](0.001147, 
                                0.001605, 
                                0.002209,
                                0.002995, 
                                0.003998, 
                                0.005253, 
                                0.006795, 
                                0.008655, 
                                0.010852, 
                                0.013397, 
                                0.016283, 
                                0.019484, 
                                0.022952, 
                                0.02662, 
                                0.030396, 
                                0.03417, 
                                0.037817, 
                                0.041206, 
                                0.044204, 
                                0.046685, 
                                0.048543, 
                                0.049692, 
                                0.050082, 
                                0.049692, 
                                0.048543, 
                                0.046685, 
                                0.044204, 
                                0.041206, 
                                0.037817, 
                                0.03417, 
                                0.030396, 
                                0.02662, 
                                0.022952, 
                                0.019484, 
                                0.016283, 
                                0.013397, 
                                0.010852, 
                                0.008655, 
                                0.006795, 
                                0.005253, 
                                0.003998, 
                                0.002995, 
                                0.002209, 
                                0.001605, 
                                0.001147);

void main(void)
{
    vec4 color = vec4(0.0);
    // Transpose the image so the filter can be applied on X and Y
    // P is the central position in the filter mask
    ivec2 P = ivec2(gl_FragCoord.yx) - ivec2(0, weights.length() >> 1);
    
    for (int w = 0; w < weights.length(); w++)
    {   
        ivec2 texelCoord = P + ivec2(0, w);
        vec4 tmpColor = vec4(0.0);
        for (int s = 0; s < numberOfSamples; ++s) {
            tmpColor += texelFetch(msaaTexture, texelCoord, s);
        }
        tmpColor /= numberOfSamples;
        color += tmpColor * weights[w]; 
    }

    finalColor = color;
}
