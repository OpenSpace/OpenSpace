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

#include "fragment.glsl"

in vec2 uv;

uniform sampler2D renderedTexture;
uniform dmat4 projection;
uniform vec2 screenSize;
uniform int filterSize;
uniform float sigma;

const float M_PI = 3.141592653589793238462;
const float DEFAULT_DEPTH = 3.08567758e19; // 1000 Pc

Fragment getFragment() {

    vec4 color = vec4(0.0);
    
    // GL_POINTS
    // Scale filter components to compensate for a skewed frustum!
    float near = float(projection[3][2] / (projection[2][2] - 1.0));
    float left = float(near * (projection[2][0] - 1.0) / projection[0][0]);
    float right = float(near * (projection[2][0] + 1.0) / projection[0][0]);
    float top = float(near * (projection[2][1] + 1.0) / projection[1][1]);
    float bottom = float(near * (projection[2][1] - 1.0) / projection[1][1]);
    
    // Find screenPos in skewed frustum. uv is [0, 1]
    //vec2 screenPos = (uv - 0.5) * 2.0; // [-1, 1]
    vec2 screenPos = uv * vec2(right - left, top - bottom) + vec2(left, bottom); 
    vec2 screenOrigo = vec2(-left, -bottom) / vec2(right - left, top - bottom);
    vec2 scaleFactor = vec2(
        pow(screenPos.x / near, 2.0),  
        pow(screenPos.y / near, 2.0)  
    );
    scaleFactor *= 0.5;

    // Use the following to find the origo in a skewed frustum.
    //Fragment origoFrag;
    //if (abs(screenOrigo.x - uv.x) > 0.0005 && abs(screenOrigo.y - uv.y) > 0.0005) {
    //    origoFrag.color = vec4(0.0);
    //} else {
    //    origoFrag.color = vec4(1.0);
    //}
    //return origoFrag;

    // Uncomment to compare to original filterSize.
    //scaleFactor = vec2(1.0);

    // Make use of this to switch betweeen circle and ellipse.
    bool useCircleDist = false;

    // Scale filter.
    int newFilterSize = int(filterSize * (1 + length(scaleFactor)));

    // Get a [filterSize x filterSize] filter around our pixel. UV is [0, 1]
    vec3 intensity = vec3(0.0);
    vec2 pixelSize = 1.0 / screenSize;
    int halfFilterSize = newFilterSize - 1 / 2;
    for (int y = -halfFilterSize; y <= halfFilterSize; y += 1) {
        for (int x = -halfFilterSize; x <= halfFilterSize; x += 1) {
            vec2 sPoint = uv + (pixelSize * vec2(x, y));
            
            // Don't sample outside of the FBO texture.
            if (all(greaterThan(sPoint, vec2(0.0))) && all(lessThan(sPoint, vec2(1.0)))) {
                vec4 sIntensity = texture( renderedTexture, sPoint );

                // Use normal distribution function for halo/bloom effect. 
                if (useCircleDist) {
                    float circleDist = sqrt(pow(x / (1 + length(scaleFactor)), 2.0)
                        + pow(y / (1 + length(scaleFactor)), 2.0));
                    intensity += sIntensity.rgb * (1.0 / (sigma * sqrt(2.0 * M_PI))) * 
                        exp(-(pow(circleDist, 2.0) / (2.0 * pow(sigma, 2.0)))) / filterSize;
                    }
                else {
                    // Elliptic gaussian distribution.
                    float alpha = atan(sPoint.y - screenOrigo.y, sPoint.x - screenOrigo.x);
                    //alpha = M_PI / 4;
                    float sigmaWidth = sigma * (1 + length(scaleFactor));
                    float sigmaHeight = sigma;

                    float a = pow(cos(alpha), 2.0) / (2 * pow(sigmaWidth, 2.0)) 
                        + pow(sin(alpha), 2.0) / (2 * pow(sigmaHeight, 2.0)) ;
                    float b = sin(2 * alpha) / (4 * pow(sigmaWidth, 2.0)) 
                        - sin(2 * alpha) / (4 * pow(sigmaHeight, 2.0)) ;
                    float c = pow(sin(alpha), 2.0) / (2 * pow(sigmaWidth, 2.0))  
                        + pow(cos(alpha), 2.0) / (2 * pow(sigmaHeight, 2.0)) ;
                    intensity += sIntensity.rgb * exp(-( a * pow(x / (1 + length(sigmaWidth)), 2.0)
                        + 2 * b * x / (1 + length(sigmaWidth)) * y / (1 + length(sigmaHeight))
                        + c * pow(y / (1 + length(sigmaHeight)), 2.0) )) / newFilterSize;
                }
            }
        }
    }
    // Tonemap intensity to color!
    intensity = 1.0 - 1.0 * exp(-25.0 * intensity);

    if (length(intensity) < 0.01) {
        discard;
    }

    color = vec4(intensity, 1.0f);

    // Use the following to check for any intensity at all.
    //color = (length(intensity.rgb) > 0.001) ? vec4(1.0) : vec4(0.0);

    Fragment frag;
    frag.color = color;
    // Place stars at back to begin with. 
    frag.depth = DEFAULT_DEPTH;
    frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
    frag.blend = BLEND_MODE_NORMAL;

    return frag;
}
