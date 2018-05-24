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
    // Scale filter to compensate for curved screens.
    vec2 screenPos = (uv - 0.5) * 2.0; // [-1, 1]
    vec2 fov = vec2(projection[0][0], projection[1][1]);
    vec2 scaleFactor = vec2(
        1.0 / (1.0 + pow(screenPos.x / fov.x, 2.0)), 
        1.0 / (1.0 + pow(screenPos.y / fov.y, 2.0))
    );
    // Uncomment to compare to original filterSize.
    //scaleFactor = vec2(1.0);
    scaleFactor *= 1.0;
    dvec2 newFilterSize = filterSize / scaleFactor;


    // Get a [filterSize x filterSize] filter around our pixel. UV is [0, 1]
    vec3 intensity = vec3(0.0);
    vec2 pixelSize = 1.0 / screenSize;
    vec2 halfFilterSize = vec2(newFilterSize - 1.0) / 2.0;
    for (float y = -halfFilterSize.y; y <= halfFilterSize.y; y += 1.0) {
        for (float x = -halfFilterSize.x; x <= halfFilterSize.x; x += 1.0) {
            vec2 sPoint = uv + (pixelSize * vec2(x, y));
            
            // Don't sample outside of the FBO texture.
            if (all(greaterThan(sPoint, vec2(0.0))) && all(lessThan(sPoint, vec2(1.0)))) {
                vec4 sIntensity = texture( renderedTexture, sPoint );

                // Use normal distribution function for halo/bloom effect. 
                float circleDist = sqrt(pow(x * scaleFactor.x, 2.0) + pow(y * scaleFactor.y, 2.0));
                intensity += sIntensity.rgb * (1.0 / (sigma * sqrt(2.0 * M_PI))) * 
                    exp(-(pow(circleDist, 2.0) / (2.0 * pow(sigma, 2.0)))) / filterSize;
            }
        }
    }
    // Tonemap intensity to color!
    intensity = 1.0 - 1.0 * exp(-25.0 * intensity);

    if (length(intensity) < 0.01) {
        discard;
    }
    //color = vec4(vec2(newFilterSize) / 14.0, 1.0, 1.0f);
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
