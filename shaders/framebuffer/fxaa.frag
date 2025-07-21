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

#version __CONTEXT__

#define EDGE_THRESHOLD_MIN 0.0312f
#define EDGE_THRESHOLD_MAX 0.125f
#define ITERATIONS 12
#define SUBPIXEL_QUALITY 0.75f

const float[12] QUALITY = float[](1.f, 1.f, 1.f, 1.f, 1.f, 1.5f, 2.f, 2.f, 2.f, 2.f, 4.f, 8.f);
// const float[24] QUALITY = {2.f, 4.f, 6.f, 8.f, 10.f, 12.f, 12.f, 12.f, 12.f, 12.f, 14.f, 18.f,
//                             18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f,
//                             18.f, 18.f};

in vec2 texCoord;
layout (location = 0) out vec4 aaFinalColor;

uniform vec2 inverseScreenSize;
uniform sampler2D renderedTexture;
uniform vec4 Viewport;
uniform vec2 Resolution;

// Relative luminance
float getLum(vec3 rgb){
  return dot(vec3(0.2126, 0.7152, 0.0722), rgb);
}

void main() {
  // Modify the texCoord based on the Viewport and Resolution. This modification is
  // necessary in case of side-by-side stereo as we only want to access the part of the
  // feeding texture that we are currently responsible for.  Otherwise we would map the
  // entire feeding texture into our half of the result texture, leading to a doubling of
  // the "missing" half.  If you don't believe me, load a configuration file with the
  // side_by_side stereo mode enabled, disable FXAA, and remove this modification.
  // The same calculation is done in the HDR resolving shader
  vec2 st = texCoord;
  st.x = st.x / (Resolution.x / Viewport[2]) + (Viewport[0] / Resolution.x);
  st.y = st.y / (Resolution.y / Viewport[3]) + (Viewport[1] / Resolution.y);

  vec4 colorCenter = texture(renderedTexture, st);

  // Detecting where to apply AA
  float pixelLumCenter = getLum(colorCenter.rgb);
  float pixelLumDown   = getLum(textureOffset(renderedTexture, st, ivec2(0,-1)).rgb);
  float pixelLumUp     = getLum(textureOffset(renderedTexture, st, ivec2(0,1)).rgb);
  float pixelLumLeft   = getLum(textureOffset(renderedTexture, st, ivec2(-1,0)).rgb);
  float pixelLumRight  = getLum(textureOffset(renderedTexture, st, ivec2(1,0)).rgb);

  float pixelLumMin = min(pixelLumCenter, min(min(pixelLumDown, pixelLumUp), min(pixelLumLeft, pixelLumRight)));
  float pixelLumMax = max(pixelLumCenter, max(max(pixelLumDown, pixelLumUp), max(pixelLumLeft, pixelLumRight)));

  // Delta
  float pixelLumRange = pixelLumMax - pixelLumMin;

  // If the pixelLum variation is lower that a threshold (or if we are in a really dark
  // area), we are not on an edge, don't perform any AA.
  if (pixelLumRange < max(EDGE_THRESHOLD_MIN, pixelLumMax * EDGE_THRESHOLD_MAX)) {
    aaFinalColor = colorCenter;
    return;
  }

  // Estimating the gradient
  float pixelLumDownLeft = getLum(textureOffset(renderedTexture, st, ivec2(-1,-1)).rgb);
  float pixelLumUpRight = getLum(textureOffset(renderedTexture, st, ivec2(1,1)).rgb);
  float pixelLumUpLeft = getLum(textureOffset(renderedTexture, st, ivec2(-1,1)).rgb);
  float pixelLumDownRight = getLum(textureOffset(renderedTexture, st, ivec2(1,-1)).rgb);

  float pixelLumDownUp = pixelLumDown + pixelLumUp;
  float pixelLumLeftRight = pixelLumLeft + pixelLumRight;
  float pixelLumLeftCorners = pixelLumDownLeft + pixelLumUpLeft;
  float pixelLumDownCorners = pixelLumDownLeft + pixelLumDownRight;
  float pixelLumRightCorners = pixelLumDownRight + pixelLumUpRight;
  float pixelLumUpCorners = pixelLumUpRight + pixelLumUpLeft;

  // Compute an estimation of the gradient
  float edgeHorizontal = abs(-2.0 * pixelLumLeft + pixelLumLeftCorners) +
    abs(-2.0 * pixelLumCenter + pixelLumDownUp) * 2.0 + abs(-2.0 * pixelLumRight + pixelLumRightCorners);
  float edgeVertical = abs(-2.0 * pixelLumUp + pixelLumUpCorners) +
    abs(-2.0 * pixelLumCenter + pixelLumLeftRight) * 2.0  + abs(-2.0 * pixelLumDown + pixelLumDownCorners);

  // Choosing Edge Orientation
  bool isHorizontal = (edgeHorizontal >= edgeVertical);
  float pixelLum1  = isHorizontal ? pixelLumDown : pixelLumLeft;
  float pixelLum2  = isHorizontal ? pixelLumUp : pixelLumRight;

  // Gradients
  float gradient1 = pixelLum1 - pixelLumCenter;
  float gradient2 = pixelLum2 - pixelLumCenter;

  bool is1Steepest = abs(gradient1) >= abs(gradient2);
  float gradientScaled = 0.25 * max(abs(gradient1), abs(gradient2));

  // Step size (one pixel) according to the edge direction.
  float stepLength = isHorizontal ? inverseScreenSize.y : inverseScreenSize.x;

  float pixelLumLocalAverage = 0.0;

  if (is1Steepest) {
    stepLength = - stepLength;
    pixelLumLocalAverage = 0.5 * (pixelLum1 + pixelLumCenter);
  }
  else {
    pixelLumLocalAverage = 0.5 * (pixelLum2 + pixelLumCenter);
  }

  vec2 currentUv = st;
  if (isHorizontal) {
    currentUv.y += stepLength * 0.5;
  }
  else {
    currentUv.x += stepLength * 0.5;
  }

  // Iterations
  vec2 offset = isHorizontal ?
    vec2(inverseScreenSize.x, 0.0) :
    vec2(0.0, inverseScreenSize.y);

  vec2 uv1 = currentUv - offset;
  vec2 uv2 = currentUv + offset;

  // Read the pixelLums at both current extremities of the exploration segment,
  // and compute the delta wrt to the local average pixelLum.
  float pixelLumEnd1 = getLum(texture(renderedTexture, uv1).rgb);
  float pixelLumEnd2 = getLum(texture(renderedTexture, uv2).rgb);
  pixelLumEnd1 -= pixelLumLocalAverage;
  pixelLumEnd2 -= pixelLumLocalAverage;

  bool reached1 = abs(pixelLumEnd1) >= gradientScaled;
  bool reached2 = abs(pixelLumEnd2) >= gradientScaled;
  bool reachedBoth = reached1 && reached2;

  if (!reached1) {
    uv1 -= offset;
  }

  if (!reached2) {
    uv2 += offset;
  }

  // Still exploring
  if (!reachedBoth) {
    for (int i = 2; i < ITERATIONS; i++) {
      // If needed, read pixelLum in 1st direction, compute delta.
      if (!reached1) {
        pixelLumEnd1 = getLum(texture(renderedTexture, uv1).rgb);
        pixelLumEnd1 = pixelLumEnd1 - pixelLumLocalAverage;
      }
      // If needed, read pixelLum in opposite direction, compute delta.
      if (!reached2) {
        pixelLumEnd2 = getLum(texture(renderedTexture, uv2).rgb);
        pixelLumEnd2 = pixelLumEnd2 - pixelLumLocalAverage;
      }
      reached1 = abs(pixelLumEnd1) >= gradientScaled;
      reached2 = abs(pixelLumEnd2) >= gradientScaled;
      reachedBoth = reached1 && reached2;

      // If the side is not reached
      if (!reached1) {
        uv1 -= offset * QUALITY[i];
      }

      if (!reached2) {
        uv2 += offset * QUALITY[i];
      }

      // If both sides have been reached
      if (reachedBoth) {
        break;
      }
    }
  }

  // Estimating the offset
  float distance1 = isHorizontal ? (st.x - uv1.x) : (st.y - uv1.y);
  float distance2 = isHorizontal ? (uv2.x - st.x) : (uv2.y - st.y);

  bool isDirection1 = distance1 < distance2;
  float distanceFinal = min(distance1, distance2);

  float edgeThickness = (distance1 + distance2);

  // Read in the direction of the closest side of the edge
  float pixelOffset = - distanceFinal / edgeThickness + 0.5;

  bool ispixelLumCenterSmaller = pixelLumCenter < pixelLumLocalAverage;

  // If the pixelLum at center is smaller than at its neighbour, the delta pixelLum at
  // each end should be positive (same variation).
  bool correctVariation = ((isDirection1 ? pixelLumEnd1 : pixelLumEnd2) < 0.0) != ispixelLumCenterSmaller;

  // If the pixelLum variation is incorrect, do not offset.
  float finalOffset = correctVariation ? pixelOffset : 0.0;

  // Subpixel antialiasing
  float pixelLumAverage = (1.0/12.0) * (2.0 * (pixelLumDownUp + pixelLumLeftRight) +
    pixelLumLeftCorners + pixelLumRightCorners);

  float subPixelOffset1 = clamp(abs(pixelLumAverage - pixelLumCenter) / pixelLumRange, 0.0, 1.0);
  float subPixelOffset2 = (-2.0 * subPixelOffset1 + 3.0) * subPixelOffset1 * subPixelOffset1;
  float subPixelOffsetFinal = subPixelOffset2 * subPixelOffset2 * SUBPIXEL_QUALITY;

  // Biggest of the two offsets
  finalOffset = max(finalOffset, subPixelOffsetFinal);

  vec2 finalUV = st;
  if (isHorizontal) {
    finalUV.y += finalOffset * stepLength;
  }
  else {
    finalUV.x += finalOffset * stepLength;
  }

  aaFinalColor = texture(renderedTexture, finalUV);
}
