/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2016                                                                    *
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

uniform float maxStepSize#{id} = 0.1;
uniform vec3 aspect#{id} = vec3(1.0);
uniform float opacityCoefficient#{id} = 1.0; 

uniform sampler3D galaxyTexture#{id};

void sample#{id}(vec3 samplePos,
                 vec3 dir,
                 inout vec3 accumulatedColor,
                 inout vec3 accumulatedAlpha,
                 inout float maxStepSize) {
    
    vec3 aspect = aspect#{id};
    maxStepSize = maxStepSize#{id} * length(dir * 1/aspect);
    
    vec4 sampledColor = texture(galaxyTexture#{id}, samplePos.xyz);

    float STEP_SIZE = maxStepSize;

    vec3 alphaTint = vec3(0.3, 0.54, 0.85);
    //alphaTint = vec3(0.0, 0.5, 1.0);

    sampledColor = sampledColor*sampledColor;
    sampledColor.a = pow(sampledColor.a, 0.7);
    //sampledColor.rgba = min(vec4(1.0), sampledColor.rgba);

    //sampledColor.a = clamp(sampledColor.a * 10000000000.0, 0.0, 1.0);
    //sampledColor.a = exp(-sampledColor.a);
    //

    //sampledColor.rgb = pow(sampledColor.rgb, vec3(10.0));
    //sampledColor.a = pow(sampledColor.a, 10.0);
    //sampledColor.a = pow(sampledColor.a, 100000000.0);
    sampledColor.rgb *= 4000.0;
    sampledColor.a = sampledColor.a * 0.3; //1.0;

    
    //float emissionCoefficient = 80;
    //float absorptionCoefficient = 1;
//    sampledColor = clamp(sampledColor, 0.0, 1.0);

    //backColor = vec3(1.0) - pow(vec3(1.0) - backColor, vec3(STEP_SIZE));    
    /*if (sampledColor.a > 1.0) {
        sampledColor.a = 1.0;
        //accumulatedColor = vec3(1.0, 0.0, 0.0);
        //accumulatedAlpha = vec3(1.0, 1.0, 1.0);
        //return;
        }*/
    //sampledColor.a = 1.2;

    //sampledColor.a *= 0.00001;

    vec3 backColor = sampledColor.rgb;
    vec3 backAlpha = sampledColor.a * alphaTint;

    backColor *= STEP_SIZE * opacityCoefficient#{id};
    backAlpha *= STEP_SIZE * opacityCoefficient#{id};

    backColor = clamp(backColor, 0.0, 1.0);
    backAlpha = clamp(backAlpha, 0.0, 1.0);
    
    vec3 oneMinusFrontAlpha = vec3(1.0) - accumulatedAlpha;
    accumulatedColor += oneMinusFrontAlpha * backColor;
    accumulatedAlpha += oneMinusFrontAlpha * backAlpha;

    // acc+= 1.0;

    //accumulatedColor = vec3(opacityCoefficient#{id});
}

float stepSize#{id}(vec3 samplePos, vec3 dir) {
    return maxStepSize#{id} * length(dir * 1.0/aspect#{id});
}
