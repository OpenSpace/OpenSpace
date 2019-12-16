/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2019                                                               *
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
uniform float absorptionMultiply#{id} = 50.0;
uniform float emissionMultiply#{id} = 1500.0;
uniform sampler3D galaxyTexture#{id};

void sample#{id}(
  vec3 samplePos,
  vec3 dir,
  inout vec3 accumulatedColor,
  inout vec3 accumulatedAlpha,
  inout float stepSize
  ) {
    vec3 aspect = aspect#{id};
    stepSize = maxStepSize#{id} / length(dir / aspect);

    //Early ray termination on black parts of the data
    vec3 normalizedPos = samplePos * 2.f - 1.f;
    if (normalizedPos.x * normalizedPos.x + normalizedPos.y * normalizedPos.y > 0.7) {
        return;
    }

    vec4 sampledColor = texture(galaxyTexture#{id}, samplePos.xyz);

    // Source textures currently are square-rooted to avoid dithering in the shadows.
    // So square them back
    sampledColor = sampledColor*sampledColor;

    // Fudge for the dust "spreading"
    sampledColor.a = clamp(sampledColor.a, 0.f, 1.f);
    sampledColor.a = pow(sampledColor.a, 0.7f);

    // Absorption probability
    float scaledDensity = sampledColor.a * stepSize * absorptionMultiply#{id};
    vec3 alphaTint = vec3(0.3f, 0.54f, 0.85f);
    vec3 absorption = alphaTint * scaledDensity;

    // Extinction
    vec3 extinction = exp(-absorption);
    accumulatedColor.rgb *= extinction;

    // Emission
    accumulatedColor.rgb +=
        sampledColor.rgb * stepSize * emissionMultiply#{id} * opacityCoefficient#{id};

    vec3 oneMinusFrontAlpha = vec3(1.f) - accumulatedAlpha;
    accumulatedAlpha += oneMinusFrontAlpha * sampledColor.rgb  * opacityCoefficient#{id};
  }

  float stepSize#{id}(vec3 samplePos, vec3 dir) {
    return maxStepSize#{id} * length(dir * 1.f / aspect#{id});
  }
