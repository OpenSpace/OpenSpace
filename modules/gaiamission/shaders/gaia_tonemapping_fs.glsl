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
uniform vec2 screenSize;

const float M_PI = 3.141592653589793238462;
const float DEFAULT_DEPTH = 3.08567758e19; // 1000 Pc
const int filterSize = 7;
const float sigma = 1.0;

Fragment getFragment() {

    vec4 color = vec4(0.0);
    
    // GL_POINTS
    // Get a [filterSize x filterSize] filter around our pixel. UV is [0, 1]
    vec3 intensity = vec3(0.0);
    vec2 pixelSize = 1.0 / screenSize;
    int halfFilterSize = (filterSize - 1) / 2;
    for (int y = -halfFilterSize; y <= halfFilterSize; y += 1) {
        for (int x = -halfFilterSize; x <= halfFilterSize; x += 1) {
            vec2 sPoint = uv + (pixelSize * vec2(x, y)); // + vec2(pixelSize / 2.0);
            
            // Don't sample outside of the FBO texture.
            if (all(greaterThan(sPoint, vec2(0.0))) && all(lessThan(sPoint, vec2(1.0)))) {
                vec4 sIntensity = texture( renderedTexture, sPoint );

                // Use normal distribution function for halo/bloom effect. 
                float circleDist = sqrt(pow(x/1, 2.0) + pow(y/1, 2.0));
                intensity += sIntensity.rgb * (1.0 / (sigma * sqrt(2.0 * M_PI))) * 
                    exp(-(pow(circleDist, 2.0) / (2.0 * pow(sigma, 2.0)))) / filterSize;
            }
        }
    }
    // Tonemap intensity to color!
    intensity = 1.0 - 1.0 * exp(-25.0 * intensity);

    
    // BILLBOARDS
    // Sample color. Tonemapping done in first shader pass.  
    //vec4 intensity = texture( renderedTexture, uv );
    

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
