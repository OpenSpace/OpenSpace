/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

uniform vec4 color#{id};
uniform float time#{id};
uniform float maxStepSize#{id} = 0.02;

void sample#{id}(vec3 samplePos, vec3 dir, inout vec3 accumulatedColor,
                 inout vec3 accumulatedAlpha, inout float stepSize)
{
    // Generate a procedural placeholder volume.
    // In real situations, the sample function would sample a
    // 3D texture to retrieve the color contribution of a given point.

    vec3 fromCenter = vec3(0.5, 0.5, 0.5) - samplePos;

    float theta = atan(fromCenter.x, fromCenter.z);
    float angularRatio = (theta + 3.1415) / 6.283;
    angularRatio = mod(angularRatio + time#{id}*0.01, 1.0);


    float timeWave = sin(mod(time#{id}*0.05, 2.0 * 3.1415));
    float rDisplacement = 0.1 * timeWave;

    vec4 c = color#{id};
    float r = length(fromCenter);
    c.a *= (1.0 - smoothstep(0.35 + rDisplacement, 0.40 + rDisplacement, r));
    c.a *= (1.0 - smoothstep(0.30 + rDisplacement, 0.25 + rDisplacement, r));
    c.a *= (1.0 - smoothstep(0.1, 0.2, abs(fromCenter.y) / angularRatio * 0.5));

    c.a *= 1.0 - smoothstep(0.0, 1.0, clamp(angularRatio, 0.0, 1.0));
    c.a *= smoothstep(0.0, 0.1, clamp(angularRatio, 0.0, 1.0));

    vec3 backAlpha = c.aaa * 10.0;
    vec3 backColor = c.rgb * backAlpha;

    backColor *= stepSize;
    backAlpha *= stepSize;

    backColor = clamp(backColor, 0.0, 1.0);
    backAlpha = clamp(backAlpha, 0.0, 1.0);

    vec3 oneMinusFrontAlpha = vec3(1.0) - accumulatedAlpha;
    accumulatedColor += oneMinusFrontAlpha * backColor;
    accumulatedAlpha += oneMinusFrontAlpha * backAlpha;

    stepSize = maxStepSize#{id};
}

float stepSize#{id}(vec3 samplePos, vec3 dir) {
    return maxStepSize#{id};
}
