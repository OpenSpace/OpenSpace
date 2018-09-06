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

#include "PowerScaling/powerScaling_vs.hglsl"

in vec2 vs_position;

layout (location = 0) out vec4 color; 
layout (location = 1) out vec4 stencil;

uniform sampler2D projectionTexture;

uniform mat4 ProjectorMatrix;
uniform mat4 ModelTransform;

uniform vec2 _scaling;
uniform vec3 _radius;
uniform int _segments;

uniform vec3 boresight;

#define M_PI 3.14159265358979323846

vec4 uvToModel(vec2 uv, vec3 radius, float segments){
    float fj = uv.x * segments;
    float fi = (1.0 - uv.y) * segments;

    float theta = fi * float(M_PI) / segments;  // 0 -> PI
    float phi   = fj * float(M_PI) * 2.0f / segments;

    float x = radius[0] * sin(phi) * sin(theta);  //
    float y = radius[1] * cos(theta);             // up 
    float z = radius[2] * cos(phi) * sin(theta);  //
    
    return vec4(x, y, z, 0.0);
}

bool inRange(float x, float a, float b){
    return (x >= a && x <= b);
} 

void main() {
    vec2 uv = (vs_position + vec2(1.0)) / vec2(2.0);

    vec4 vertex = uvToModel(uv, _radius, _segments);

    vec4 raw_pos   = psc_to_meter(vertex, _scaling);
    vec4 projected = ProjectorMatrix * ModelTransform * raw_pos;

    projected.x /= projected.w;
    projected.y /= projected.w;

    projected = projected * 0.5 + vec4(0.5);

    vec3 normal = normalize((ModelTransform * vec4(vertex.xyz, 0.0)).xyz);

    vec3 v_b = normalize(boresight);

    if((inRange(projected.x, 0.0, 1.0) && inRange(projected.y, 0.0, 1.0)) &&
       dot(v_b, normal) < 0.0)
    {
        color = texture(projectionTexture, vec2(projected.x, projected.y));
        stencil = vec4(1.0); 
    }
    else {
        color = vec4(0.0);
        stencil = vec4(0.0);
    }
}
