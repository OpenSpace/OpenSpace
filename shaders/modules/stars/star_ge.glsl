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

#version __CONTEXT__

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 1.0), 
    vec2(0.0, 0.0), 
    vec2(1.0, 1.0), 
    vec2(1.0, 0.0) 
);

#include "PowerScaling/powerScalingMath.hglsl"


layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

layout(location = 0) in vec4 psc_position[];
layout(location = 1) in vec3 vs_brightness[];
layout(location = 2) in vec3 vs_velocity[];
layout(location = 3) in float vs_speed[];
layout(location = 4) in vec4 cam_position[];

layout(location = 0) out vec4 vs_position;
layout(location = 1) out vec3 ge_brightness;
layout(location = 2) out vec3 ge_velocity;
layout(location = 3) out float ge_speed;
layout(location = 4) out vec2 texCoord;

uniform mat4 projection;

uniform float spriteBaseSize;
uniform float spriteResponseSize;

// As soon as the scalegraph is in place, replace this by a dynamic calculation
// of apparent magnitude in relation to the camera position ---abock
void main() {
    // Skip the Sun
    if (psc_position[0].x == 0.0 && psc_position[0].y == 0.0 && psc_position[0].z == 0.0) {
        return;
    }
    ge_brightness = vs_brightness[0];
    ge_velocity = vs_velocity[0];
    ge_speed = vs_speed[0];

    float M  = vs_brightness[0].z;

    M = clamp(M, 1.0, 5.0);
    float modifiedSpriteSize = exp((-5 - M) * 0.871);

    for(int i = 0; i < 4; i++){
        vec4 p1     = gl_in[0].gl_Position;
        p1.xy      += vec2(modifiedSpriteSize * (corners[i] - vec2(0.5))); 
        vs_position = p1;
        gl_Position = projection * p1;
        // gl_Position = z_normalization(projection * p1);
        texCoord    = corners[i];
      EmitVertex();
    }
    EndPrimitive();
}

// Old method, still in the code for reference ---abock
#if 0
void main() {
    ge_brightness = vs_brightness[0];
    ge_velocity = vs_velocity[0];
    ge_speed = vs_speed[0];
    
    //  float M  = vs_brightness[0][0];                                 // get ABSOLUTE magnitude (x param)
    float M  = vs_brightness[0].z; // if NOT running test-target.

    // We are using a fixed position until the scalegraph is implemented ---abock
    // vec4 cam = vec4(-cam_position[0].xyz, cam_position[0].w);                  // get negative camera position   
    // vec4 cam = vec4(0.0);
    
    vec4 pos = psc_position[0];                                    // get OK star position
    
    // vec4 result = psc_addition(pos, cam);                          // compute vec from camera to position
    vec4 result = pos;
    vec2 pc = vec2(
        length(result.xyz),
        result.w
    );

    // convert meters into parsecs
    // pc[0] *= 0.324077929f;
    // pc[1] -= 18.0f;
    
    float distLog = log10(pc[0]) + pc[1];
    float apparent = (M - 5.f * (1.f - distLog));

    // p = vec4(vec3(apparent), 1.0);
     
     // check everything below this ---abock
    float weight = 0.000025f;                                           // otherwise this takes over.
    double depth  = pc[0] * pow(10, pc[1]);
    depth       *= pow(apparent,3);

    double modifiedSpriteSize = (spriteBaseSize * 0.0005f) + (depth*weight); 
    modifiedSpriteSize *= spriteResponseSize;

    // // modifiedSpriteSize = min(modifiedSpriteSize, -0.0025f);
    // {
    //     vec4 p = gl_in[0].gl_Position;
    //     p.xy += vec2(modifiedSpriteSize * (corners[1] - vec2(0.5)));
    //     p = projection * p;

    //     vec4 q = gl_in[0].gl_Position;
    //     q.xy += vec2(modifiedSpriteSize * (corners[2] - vec2(0.5)));
    //     q = projection * q;

    //     if (length(p.xyz-q.xyz) < 0.025)
    //         return;

    //     // if (sin(dot(p,q)) < 0.1)
    //     //     return;

    // }

    // if (depth < -0.00000001)
        // return;

    // float distLy = vs_brightness[0].w;
    // float distParsec = distLy * 0.306594845;
    // if (distParsec > 200 && -apparent > 1.0)
    //     return;


    for(int i = 0; i < 4; i++){
        vec4 p1     = gl_in[0].gl_Position;
        // vec4 p1 = psc_position[0];
        p1.w = 1.0;
        // p1.w = 1.0;
        p1.xy      += vec2(modifiedSpriteSize * (corners[i] - vec2(0.5))); 
        vs_position = p1;
        gl_Position = projection * p1;
        // gl_Position = p1;
        // if (modifiedSpriteSize > -0.001f)
            // texCoord = vec2(0.5);
    // else
            texCoord    = corners[i];
      EmitVertex();
    }
    EndPrimitive();
}
#endif
