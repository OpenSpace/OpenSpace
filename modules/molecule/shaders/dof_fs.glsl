/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#pragma optionNV(unroll all)

in vec2 texCoords;
out vec4 fragColor;

// From http://blog.tuxedolabs.com/2018/05/04/bokeh-depth-of-field-in-single-pass.html

uniform sampler2D texHalfRes; // Half res color (rgb) and coc (a)
uniform sampler2D texColor;    // Image to be processed
uniform sampler2D texDepth;    // Linear depth

uniform vec2 texelSize;    // The size of a pixel: vec2(1.0/width, 1.0/height)
uniform float focusDepth;
uniform float focusScale;
uniform float time;

const float GoldenAngle = 2.39996323;
const float MaxBlurSize = 15.0;
const float RadScale = 1.0; // Smaller = nicer blur, larger = faster

vec4 srand4(vec2 n) {
  vec4 r = fract(
    sin(dot(n.xy, vec2(12.9898, 78.233))) *
    vec4(43758.5453, 28001.8384, 50849.4141, 12996.89)
  );
  return r * 2.0 - 1.0;
}

float blurSize(float depth, float focusPoint, float focusScale) {
  float coc = clamp((1.0 / focusPoint - 1.0 / depth) * focusScale, -1.0, 1.0);
  return abs(coc) * MaxBlurSize;
}

vec3 depthOfField(vec2 texCoord, float focusPoint, float focusScale) {
  float centerDepth = texture(texDepth, texCoord).r;
  vec3 centerColor = texture(texColor, texCoord).rgb;
  float centerCoc = blurSize(centerDepth, focusPoint, focusScale);
  vec4 colorCocSum = vec4(centerColor, centerCoc);

  float contribSum = 1.0;
  float radius = RadScale;
  float ang = 0.0;

  for (; radius < MaxBlurSize; ang += GoldenAngle) {
    vec2 tc = texCoord + vec2(cos(ang), sin(ang)) * texelSize * radius;
    float sampleDepth = texture(texDepth, tc).r;
    vec3 sampleColor = texture(texColor, tc).rgb;
    float sampleCoc = blurSize(sampleDepth, focusPoint, focusScale);
    if (sampleDepth > centerDepth) {
      sampleCoc = min(sampleCoc, centerCoc * 2.0);
    }

    vec4 sampleColorCoc = vec4(sampleColor, sampleCoc);

    colorCocSum += mix(
      colorCocSum / contribSum,
      sampleColorCoc,
      smoothstep(radius - 0.5, radius + 0.5, sampleColorCoc.a)
    );
    contribSum += 1.0;
    radius += RadScale / radius;

    if (colorCocSum.a / contribSum > 1.0) {
      break;
    }
  }

  const float HalfResRadScale = 2.0;
  for (; radius < MaxBlurSize; ang += GoldenAngle) {
    vec2 tc = texCoord + vec2(cos(ang), sin(ang)) * texelSize * radius;
    vec4 sampleColorCoc = texture(texHalfRes, tc) * vec4(1.0, 1.0, 1.0, MaxBlurSize);

    float sampleDepth = texture(texDepth, tc).r;
    if (sampleDepth > centerDepth) {
      sampleColorCoc.a = min(sampleColorCoc.a, centerCoc * 2.0);
    }

    colorCocSum += mix(
      colorCocSum / contribSum,
      sampleColorCoc,
      smoothstep(radius - 0.5, radius + 0.5, sampleColorCoc.a)
    );
    contribSum += 1.0;
    radius += HalfResRadScale / radius;
  }
  return colorCocSum.rgb /= contribSum;
}

void main() {
  vec3 dof = depthOfField(texCoords, focusDepth, focusScale);

  // To hide banding artifacts
  vec4 noise = srand4(texCoords + time + 0.6959174) / 15.0;
  dof += noise.xyz;
  fragColor = vec4(dof, 1.0);
}
