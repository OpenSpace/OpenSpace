/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

in vec2 out_position;
//in float pointRadius;

out vec4 FragColor;

void main () {
    
    // calculate normal from texture coordinates
    //vec3 n;
    //n.xy = gl_PointCoord.st*vec2(2.0, -2.0) + vec2(-1.0, 1.0);
    //float mag = dot(n.xy, n.xy);
    //if (mag > 1.0) discard;   // kill pixels outside circle
    //n.z = sqrt(1.0-mag);

    //vec3 eye = mvPosition + vec3(0.0, 0.0, pointRadius * n.z);
    //float depth = (P[2][2] * eye.z + P[3][2]) / (P[2][3] * eye.z + P[3][3]);

    //gl_FragDepth = (depth + 1.0) / 2.0;

    
    // calculate lighting
    //const vec3 light_dir = vec3(0.0, 0.0, 1.0);
    //float diffuse = max(0.0, dot(light_dir, n));
 
    //vec3 halfVector = normalize( eye + light_dir);  
    //float spec = pow(max(0.0, dot(n,halfVector)), 100.0);

    // modify color according to the velocity
    //vec3 color = vec3(0.3, 0.3, 0.9);
    //vec3 hsv = rgb2hsv(color);
    //float v = length(outVelocity);
    //v = min((1.0/maxVelocity)*v*v, 1.0);
    //vec3 fluidColor = hsv2rgb(vec3(hsv.x, max(1.0 - v, 0.0), 1.0));

    // compute final color
    //vec3 outColor = 0.25 * fluidColor;
    //outColor += 0.7 * diffuse * fluidColor;
    //outColor += 0.05 * spec * vec3(1.0);
    //outColor = clamp(outColor, 0.0, 1.0);

    FragColor = vec4(1.0, 1.0, 1.0, 1.0);
}