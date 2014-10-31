/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

// Based on http://prideout.net/blog/?p=64

uniform sampler3D texVolume;
uniform mat4 modelView;
uniform mat4 modelViewProjection;
uniform float focalLength;
uniform vec2 windowSize;
uniform vec3 rayOrigin;
uniform float stepSize;

const int maxNumSamples = 128;

out vec4 fragColor;

struct Ray {
    vec3 Origin;
    vec3 Dir;
};

struct AABB {
    vec3 Min;
    vec3 Max;
};

bool IntersectBox(Ray r, AABB box, out float t0, out float t1) {
    vec3 invR = 1.0 / r.Dir;
    vec3 tbot = invR * (box.Min-r.Origin);
    vec3 ttop = invR * (box.Max-r.Origin);
    vec3 tmin = min(ttop, tbot);
    vec3 tmax = max(ttop, tbot);
    vec2 t = max(tmin.xx, tmin.yz);
    t0 = max(t.x, t.y);
    t = min(tmax.xx, tmax.yz);
    t1 = min(t.x, t.y);
    return t0 <= t1;
}

void main() {
    vec3 rayDirection;
    rayDirection.x = 2.0 * gl_FragCoord.x / windowSize.x - 1.0;
    rayDirection.y = 2.0 * gl_FragCoord.y / windowSize.y - 1.0;
    rayDirection.z = -focalLength;
    rayDirection = (vec4(rayDirection, 0) * modelView).xyz;

    Ray eye = Ray( rayOrigin, normalize(rayDirection) );
    AABB box = AABB(vec3(-1.0), vec3(1.0));

    float tnear, tfar;
    IntersectBox(eye, box, tnear, tfar);
    tnear = max(tnear, 0.0);

    vec3 front = eye.Origin + eye.Dir* tnear;
    vec3 back = eye.Origin + eye.Dir* tfar;
    front = 0.5 * (front + 1.0);
    back = 0.5 * (back + 1.0);

    vec3 direction = back-front;
    float directionLength = length(direction);
    direction = normalize(direction);
    vec3 position = front;
    vec4 tmp, color = vec4(0);
    int i = 0;

    while (length(position-front) < directionLength && color.r != 1.0  && i < maxNumSamples) {
        ++i;
        tmp = texture(texVolume, position);
        color = max(color, tmp); // MIP
        position = position + direction * stepSize;
    }

    fragColor = vec4(color.rrr,1.0);    

    // // DEBUG DEBUG DEBUG
    // fragColor = vec4(front, 1.0);
    // if (front.x < 0.1)
    //     fragColor = vec4(1.0);

    // if (front.y < 0.1)
    //     fragColor = vec4(1.0);

    // if (front.x > 0.9)
    //     fragColor = vec4(1.0);

    // if (front.y > 0.9)
    //     fragColor = vec4(1.0);

    // if (front.x > 0.45 && front.x < 0.55)
    //     fragColor = vec4(0.0);

    // if (front.y > 0.45 && front.y < 0.55)
    //     fragColor = vec4(0.0);
}