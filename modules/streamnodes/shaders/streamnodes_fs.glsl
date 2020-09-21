/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

uniform sampler2D texture1;
uniform bool drawCircles;
uniform bool drawHollow;
uniform bool useGaussian;
uniform bool usingCameraPerspective;
//uniform float testChange;
uniform bool pulsatingAlways;
uniform bool usingPulse;
uniform bool usingGaussianPulse;
uniform vec3 cameraPos;
uniform vec4 streamColor;

in vec2 vs_st;
in vec4 vs_color;
in float vs_depth;
in float vs_closeToEarth;
in flat double vs_time;
in float camera_IsCloseEnough;

Fragment getFragment() {
    if (vs_color.a == 0) {
        discard;
    }

    vec4 fragColor = vs_color;
    vec2 pos = vec2(0.5)-vs_st;

    float r = length(pos)*2.0;
    float a = atan(pos.y,pos.x);
    float f = cos(a*3.);

    vec3 color = vec3(0.0);
    color = vec3( 1.-smoothstep(f,f, r) );

    //fragColor = vec4(color, 1.0);

    Fragment frag;
    frag.depth = vs_depth;
    frag.color = fragColor;
    vec2 coord = gl_PointCoord - vec2(0.5);
   
  // if(camera_IsCloseEnough > 0.5){
    /*zif(length(coord.x) > 0.15){
        if(length(coord.y) > 0.15){
            discard;
        }
    }*/

    if(drawCircles){
        if(length(coord) > 0.5){
            discard;
        }
    }

    // if(vs_closeToEarth > 0.5){
    if(drawHollow && length(coord) < 0.4){
        //frag.color.xyz = streamColor.xyz;
        if(vs_closeToEarth > 0.5 || distance(cameraPos, vec3(0)) < 500000000000.f){
            if(usingGaussianPulse && usingCameraPerspective){
                if(vs_closeToEarth > 0.5){
                    if(length(coord) < 0.3){
                        if(pulsatingAlways || camera_IsCloseEnough > 0.5){
                            float e = 2.718055f;
                            float y = 1 * pow(e, - (pow(length(coord), 2)) /( 2 * pow(0.2, 2))); 
                            if(y < 0.05){
                                discard;
                            }
                        frag.color.a = y;
                        }
                    }
                }
                else{
                    discard;
                }
            }
            else{
                discard;
            }   
        }
    }

    //}
    // outline
    /*
    if(length(coord) > 0.4){
        frag.color = vec4(1, 1, 1, 1);
    }
    */
    /*
    if(length(coord) < 0.1){
        frag.color.a = 1.0;
    }
    */
    //float alphaV = 1 - smoothstep(0, 1, length(coord));
    float e = 2.718055f;

   if(useGaussian){
       float y = 1 * pow(e, - (pow(length(coord), 2)) /( 2 * pow(0.2, 2))); 
       if(y < 0.05){
           discard;
       }
   frag.color.a = y;
   }
    //}

    if(usingPulse && usingCameraPerspective){
        if(vs_closeToEarth > 0.5){
            if(pulsatingAlways || camera_IsCloseEnough > 0.5){
                if(length(coord) > 0.46){ //0.46 (utan vec4(1, 1, 1, 1)), 0.4, 0.32 
                //frag.color = vec4(1,1,1,1); //HÄR
                    float speed = 60.f;
                    int modulusResult = int(double(speed) * vs_time) % 60;
                    if(modulusResult > 0 && modulusResult < 30){
                        //discard; 
                    }
                }
            }
        }
    }
    
    //homecooked solution to get similar to normal distribution
    /*
        float alphaV = sqrt(pow(1 - length(coord), 3));
            alphaV = pow(alphaV, 3);
        if(alphaV < 0.1){
            discard;
        }
        */
   
    //else{
    //frag.color.a = alphaV;
    //}   
    
    //vec2 coord = gl_PointCoord;
    
    /*
    vec2 coord = gl_PointCoord;
    if(coord.y > 0.5){
    discard;
    }
    */
    //if(vs_st.x != -1){
    //if (gl_FrontFacing) {
    //    frag.color = texture(texture1, vs_st);
   // }
   // else {
   //     frag.color = texture(texture1, vec2(1 - vs_st.s, vs_st.t));
   // }
   // }

    // G-Buffer
   // frag.gPosition = vec4(0.0); //vs_gPosition;
    frag.gPosition  = vec4(1e27, 1e27, 1e27, 1.0);
    frag.gNormal    = vec4(0.0, 0.0, 0.0, 1.0);
    // There is no normal here
    // TODO: Add the correct normal if necessary (JCC)
    //frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);

    return frag;
}
