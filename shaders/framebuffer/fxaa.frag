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

#version __CONTEXT__

#define EDGE_THRESHOLD_MIN 0.0312f 
#define EDGE_THRESHOLD_MAX 0.125f
#define ITERATIONS 12
#define SUBPIXEL_QUALITY 0.75f

const float[12] QUALITY = {1.f, 1.f, 1.f, 1.f, 1.f, 1.5f, 2.f, 2.f, 2.f, 2.f, 4.f, 8.f};
// const float[24] QUALITY = {2.f, 4.f, 6.f, 8.f, 10.f, 12.f, 12.f, 12.f, 12.f, 12.f, 14.f, 18.f,
//                             18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 18.f, 
//                             18.f, 18.f};

layout (location = 0) out vec4 aaFinalColor;

uniform vec2 inverseScreenSize;
uniform sampler2D renderedTexture;

in vec2 texCoord;

// Relative luminance
float getLum(vec3 rgb){
    return dot(vec3(0.2126, 0.7152, 0.0722), rgb);
}

void main() {
    vec4 colorCenter = texture(renderedTexture,texCoord);

    // ============================
    // Detecting where to apply AA:
    // ============================

    // Luma at the current fragment
    float lumaCenter = getLum(colorCenter.rgb);

    // Luma at the four direct neighbours of the current fragment.
    float lumaDown  = getLum(textureOffset(renderedTexture, texCoord, ivec2(0,-1)).rgb);
    float lumaUp    = getLum(textureOffset(renderedTexture, texCoord, ivec2(0,1)).rgb);
    float lumaLeft  = getLum(textureOffset(renderedTexture, texCoord, ivec2(-1,0)).rgb);
    float lumaRight = getLum(textureOffset(renderedTexture, texCoord, ivec2(1,0)).rgb);
    
    // Find the maximum and minimum luma around the current fragment.
    float lumaMin = min(lumaCenter, min(min(lumaDown, lumaUp), min(lumaLeft, lumaRight)));
    float lumaMax = max(lumaCenter, max(max(lumaDown, lumaUp), max(lumaLeft, lumaRight)));

    // Compute the delta.
    float lumaRange = lumaMax - lumaMin;

    // If the luma variation is lower that a threshold (or if we are in a really dark area), 
    // we are not on an edge, don't perform any AA.
    if (lumaRange < max(EDGE_THRESHOLD_MIN, lumaMax * EDGE_THRESHOLD_MAX)) {
        aaFinalColor = colorCenter;
        return;
    }

    // ============================
    // Estimating the gradient:
    // ============================
    // Query the 4 remaining corners lumas.
    float lumaDownLeft = getLum(textureOffset(renderedTexture, texCoord, ivec2(-1,-1)).rgb);
    float lumaUpRight = getLum(textureOffset(renderedTexture, texCoord, ivec2(1,1)).rgb);
    float lumaUpLeft = getLum(textureOffset(renderedTexture, texCoord, ivec2(-1,1)).rgb);
    float lumaDownRight = getLum(textureOffset(renderedTexture, texCoord, ivec2(1,-1)).rgb);

    // Combine the four edges lumas (using intermediary variables for future computations 
    // with the same values).
    float lumaDownUp = lumaDown + lumaUp;
    float lumaLeftRight = lumaLeft + lumaRight;

    // Same for corners
    float lumaLeftCorners  = lumaDownLeft + lumaUpLeft;
    float lumaDownCorners  = lumaDownLeft + lumaDownRight;
    float lumaRightCorners = lumaDownRight + lumaUpRight;
    float lumaUpCorners    = lumaUpRight + lumaUpLeft;

    // Compute an estimation of the gradient along the horizontal and vertical axis.
    float edgeHorizontal = abs(-2.0 * lumaLeft + lumaLeftCorners) + 
        abs(-2.0 * lumaCenter + lumaDownUp ) * 2.0 + abs(-2.0 * lumaRight + lumaRightCorners);
    float edgeVertical = abs(-2.0 * lumaUp + lumaUpCorners) + 
        abs(-2.0 * lumaCenter + lumaLeftRight) * 2.0  + abs(-2.0 * lumaDown + lumaDownCorners);

    // Is the local edge horizontal or vertical ?
    bool isHorizontal = (edgeHorizontal >= edgeVertical);

    // ============================
    // Choosing Edge Orientation:
    // ============================
    // Select the two neighboring texels lumas in the opposite direction to the local edge.
    float luma1 = isHorizontal ? lumaDown : lumaLeft;
    float luma2 = isHorizontal ? lumaUp : lumaRight;
    
    // Compute gradients in this direction.
    float gradient1 = luma1 - lumaCenter;
    float gradient2 = luma2 - lumaCenter;

    // Which direction is the steepest ?
    bool is1Steepest = abs(gradient1) >= abs(gradient2);

    // Gradient in the corresponding direction, normalized.
    float gradientScaled = 0.25 * max(abs(gradient1), abs(gradient2));

    // Choose the step size (one pixel) according to the edge direction.
    float stepLength = isHorizontal ? inverseScreenSize.y : inverseScreenSize.x;

    // Average luma in the correct direction.
    float lumaLocalAverage = 0.0;

    if (is1Steepest) {
        // Switch the direction
        stepLength = - stepLength;
        lumaLocalAverage = 0.5 * (luma1 + lumaCenter);
    } else {
        lumaLocalAverage = 0.5 * (luma2 + lumaCenter);
    }

    // Shift UV in the correct direction by half a pixel.
    vec2 currentUv = texCoord;
    if (isHorizontal) {
        currentUv.y += stepLength * 0.5;
    } else {
        currentUv.x += stepLength * 0.5;
    }

    // ============================
    // Iterations:
    // ============================
    // Compute offset (for each iteration step) in the right direction.
    vec2 offset = isHorizontal ? vec2(inverseScreenSize.x, 0.0) : vec2(0.0, inverseScreenSize.y);
    
    // Compute UVs to explore on each side of the edge, orthogonally. 
    // The QUALITY allows us to step faster.
    vec2 uv1 = currentUv - offset;
    vec2 uv2 = currentUv + offset;

    // Read the lumas at both current extremities of the exploration segment, 
    // and compute the delta wrt to the local average luma.
    float lumaEnd1 = getLum(texture(renderedTexture, uv1).rgb);
    float lumaEnd2 = getLum(texture(renderedTexture, uv2).rgb);
    lumaEnd1 -= lumaLocalAverage;
    lumaEnd2 -= lumaLocalAverage;

    // If the luma deltas at the current extremities are larger than the local gradient, 
    // we have reached the side of the edge.
    bool reached1 = abs(lumaEnd1) >= gradientScaled;
    bool reached2 = abs(lumaEnd2) >= gradientScaled;
    bool reachedBoth = reached1 && reached2;

    // If the side is not reached, we continue to explore in this direction.
    if (!reached1) {
        uv1 -= offset;
    }

    if (!reached2) {
        uv2 += offset;
    }   

    // If both sides have not been reached, continue to explore.
    if (!reachedBoth) {
        for (int i = 2; i < ITERATIONS; i++) {
            // If needed, read luma in 1st direction, compute delta.
            if (!reached1) {
                lumaEnd1 = getLum(texture(renderedTexture, uv1).rgb);
                lumaEnd1 = lumaEnd1 - lumaLocalAverage;
            }
            // If needed, read luma in opposite direction, compute delta.
            if (!reached2) {
                lumaEnd2 = getLum(texture(renderedTexture, uv2).rgb);
                lumaEnd2 = lumaEnd2 - lumaLocalAverage;
            }
            // If the luma deltas at the current extremities is larger than 
            // the local gradient, we have reached the side of the edge.
            reached1 = abs(lumaEnd1) >= gradientScaled;
            reached2 = abs(lumaEnd2) >= gradientScaled;
            reachedBoth = reached1 && reached2;

            // If the side is not reached, we continue to explore in this direction, 
            // with a variable quality.
            if (!reached1) {
                uv1 -= offset * QUALITY[i];
            }

            if (!reached2) {
                uv2 += offset * QUALITY[i];
            }

            // If both sides have been reached, stop the exploration.
            if (reachedBoth) { 
                break;
            }
        }
    }

    // ============================
    // Estimating the offset:
    // ============================
    // Compute the distances to each extremity of the edge.
    float distance1 = isHorizontal ? (texCoord.x - uv1.x) : (texCoord.y - uv1.y);
    float distance2 = isHorizontal ? (uv2.x - texCoord.x) : (uv2.y - texCoord.y);

    // In which direction is the extremity of the edge closer ?
    bool isDirection1 = distance1 < distance2;
    float distanceFinal = min(distance1, distance2);

    // Length of the edge.
    float edgeThickness = (distance1 + distance2);

    // UV offset: read in the direction of the closest side of the edge.
    float pixelOffset = - distanceFinal / edgeThickness + 0.5;

    // Is the luma at center smaller than the local average ?
    bool isLumaCenterSmaller = lumaCenter < lumaLocalAverage;

    // If the luma at center is smaller than at its neighbour, the delta luma at 
    // each end should be positive (same variation).
    // (in the direction of the closer side of the edge.)
    bool correctVariation = ((isDirection1 ? lumaEnd1 : lumaEnd2) < 0.0) != isLumaCenterSmaller;

    // If the luma variation is incorrect, do not offset.
    float finalOffset = correctVariation ? pixelOffset : 0.0;

    // ============================
    // Subpixel antialiasing:
    // ============================
    // Sub-pixel shifting
    // Full weighted average of the luma over the 3x3 neighborhood.
    float lumaAverage = (1.0/12.0) * (2.0 * (lumaDownUp + lumaLeftRight) + 
        lumaLeftCorners + lumaRightCorners);
    // Ratio of the delta between the global average and the center luma, over the luma range in the 3x3 neighborhood.
    float subPixelOffset1 = clamp(abs(lumaAverage - lumaCenter) / lumaRange, 0.0, 1.0);
    float subPixelOffset2 = (-2.0 * subPixelOffset1 + 3.0) * subPixelOffset1 * subPixelOffset1;
    // Compute a sub-pixel offset based on this delta.
    float subPixelOffsetFinal = subPixelOffset2 * subPixelOffset2 * SUBPIXEL_QUALITY;

    // Pick the biggest of the two offsets.
    finalOffset = max(finalOffset, subPixelOffsetFinal);

    // Compute the final UV coordinates.
    vec2 finalUv = texCoord;
    if (isHorizontal) {
        finalUv.y += finalOffset * stepLength;
    } else {
        finalUv.x += finalOffset * stepLength;
    }

    // Read the color at the new UV coordinates, and use it.
    aaFinalColor = texture(renderedTexture, finalUv);
}