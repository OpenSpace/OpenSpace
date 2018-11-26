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
uniform vec2 blurDirection;
uniform float blurMagnitude;

// Linear sampling, 9-tap
// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/

// 9-tap, sigma = 1.0
// 0.382928, 0.241732, 0.060598, 0.005977, 0.000229
// uniform int n = 3;
// uniform float offset[3] = float[]( 0, 1.200436609003407, 3.0368997744118595 );
// uniform float weight[3] = float[]( 0.382928, 0.30233, 0.006206 );

// 9-tap, sigma = 1.75
// 0.227204, 0.193829, 0.120338, 0.054364, 0.017867
// uniform int n = 3;
// uniform float offset[3] = float[]( 0.227204, 1.383038320383745, 3.247359167116612 );
// uniform float weight[3] = float[]( 0, 0.314167, 0.072231 );

// 9-tap, sigma = 15
// 0.112759, 0.112509, 0.111762, 0.110527, 0.108822
uniform int n = 3;
uniform float offset[3] = float[]( 0, 1.4983346041173402, 3.4961134994916776 );
uniform float weight[3] = float[]( 0.112759, 0.224271, 0.21934900000000002 );

// 17-tap, sigma = 3.0
// 0.13298, 0.125858, 0.106701, 0.081029, 0.055119, 0.033585, 0.018331, 0.008962, 0.003924
// uniform int n = 5;
// uniform float offset[5] = float[]( 0.13298, 1.4588126023933712, 3.4048461967858508, 5.35308960628708, 7.304516529566972 );
// uniform float weight[5] = float[]( 0, 0.23255900000000002, 0.136148, 0.051916, 0.012886 );

// 33-tap, sigma = 5.0
// 0.079733, 0.078159, 0.073622, 0.066638, 0.05796, 0.048441, 0.038903, 0.030022, 0.022263, 0.015864, 0.010863, 0.007147, 0.004519, 0.002745, 0.001603, 0.000899, 0.000485
// uniform int n = 9;
// uniform float offset[9] = float[]( 0, 1.48505412403397, 3.4651760060354095, 5.445399798497894, 7.425800898919384, 9.406442922886967, 11.387364992285274, 13.36867525298988, 15.35043352601156 );
// uniform float weight[9] = float[]( 0.079733, 0.151781, 0.124598, 0.087344, 0.052285, 0.026727, 0.011666, 0.004348, 0.001384 );

// 33-tap, sigma = 15.0
// 0.036493, 0.036412, 0.03617, 0.035771, 0.035219, 0.034522, 0.033688, 0.032729, 0.031657, 0.030483, 0.029224, 0.027892, 0.026502, 0.025071, 0.023611, 0.022138, 0.020665
// uniform int n = 9;
// uniform float offset[9] = float[]( 0, 1.4983329200077153, 3.4961121284687984, 5.493886526902214, 7.491675208896344, 9.489456847605808, 11.487222855461999, 13.485004724538843, 15.482793262154521 );
// uniform float weight[9] = float[]( 0.036493, 0.07258200000000001, 0.07099, 0.06820999999999999, 0.064386, 0.059706999999999996, 0.054394, 0.048682, 0.042803 );

void main() {
    vec4 sum = vec4(0.0);
    vec2 tc = UV;
    float mag = blurMagnitude;
    float hstep = blurDirection.x;
    float vstep = blurDirection.y;
    float h = mag*hstep;
    float v = mag*vstep;

    sum = texture( mainColorTexture, tc ) * weight[0];
    for (int i=1; i<n; i++) {
        sum += texture( mainColorTexture, tc + vec2( offset[i] * h, offset[i] * v ) ) * weight[i];
        sum += texture( mainColorTexture, tc - vec2( offset[i] * h, offset[i] * v ) ) * weight[i];
    }

    finalColor = vec4(sum.rgb,1.0);

}
