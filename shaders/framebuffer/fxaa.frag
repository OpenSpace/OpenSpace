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

const float[12] QUALITY = float[](1.f, 1.f, 1.f, 1.f, 1.f, 1.5f, 2.f, 2.f, 2.f, 2.f, 4.f, 8.f);
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

vec4 fxaaMethod1() {
    vec4 colorCenter = texture(renderedTexture,texCoord);

    // ============================
    // Detecting where to apply AA:
    // ============================

    float pixelLumCenter = getLum(colorCenter.rgb);
    float pixelLumDown   = getLum(textureOffset(renderedTexture, texCoord, ivec2(0,-1)).rgb);
    float pixelLumUp     = getLum(textureOffset(renderedTexture, texCoord, ivec2(0,1)).rgb);
    float pixelLumLeft   = getLum(textureOffset(renderedTexture, texCoord, ivec2(-1,0)).rgb);
    float pixelLumRight  = getLum(textureOffset(renderedTexture, texCoord, ivec2(1,0)).rgb);
    
    float pixelLumMin = min(pixelLumCenter, min(min(pixelLumDown, pixelLumUp), min(pixelLumLeft, pixelLumRight)));
    float pixelLumMax = max(pixelLumCenter, max(max(pixelLumDown, pixelLumUp), max(pixelLumLeft, pixelLumRight)));

    // Delta.
    float pixelLumRange = pixelLumMax - pixelLumMin;

    // If the pixelLum variation is lower that a threshold (or if we are in a really dark area), 
    // we are not on an edge, don't perform any AA.
    if (pixelLumRange < max(EDGE_THRESHOLD_MIN, pixelLumMax * EDGE_THRESHOLD_MAX)) {
        aaFinalColor = colorCenter;
        return vec4(-1.0);
    }

    // ============================
    // Estimating the gradient:
    // ============================
    float pixelLumDownLeft  = getLum(textureOffset(renderedTexture, texCoord, ivec2(-1,-1)).rgb);
    float pixelLumUpRight   = getLum(textureOffset(renderedTexture, texCoord, ivec2(1,1)).rgb);
    float pixelLumUpLeft    = getLum(textureOffset(renderedTexture, texCoord, ivec2(-1,1)).rgb);
    float pixelLumDownRight = getLum(textureOffset(renderedTexture, texCoord, ivec2(1,-1)).rgb);

    float pixelLumDownUp       = pixelLumDown + pixelLumUp;
    float pixelLumLeftRight    = pixelLumLeft + pixelLumRight;
    float pixelLumLeftCorners  = pixelLumDownLeft + pixelLumUpLeft;
    float pixelLumDownCorners  = pixelLumDownLeft + pixelLumDownRight;
    float pixelLumRightCorners = pixelLumDownRight + pixelLumUpRight;
    float pixelLumUpCorners    = pixelLumUpRight + pixelLumUpLeft;

    // Compute an estimation of the gradient
    float edgeHorizontal = abs(-2.0 * pixelLumLeft + pixelLumLeftCorners) + 
        abs(-2.0 * pixelLumCenter + pixelLumDownUp ) * 2.0 + abs(-2.0 * pixelLumRight + pixelLumRightCorners);
    float edgeVertical = abs(-2.0 * pixelLumUp + pixelLumUpCorners) + 
        abs(-2.0 * pixelLumCenter + pixelLumLeftRight) * 2.0  + abs(-2.0 * pixelLumDown + pixelLumDownCorners);

    // ============================
    // Choosing Edge Orientation:
    // ============================
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
    } else {
        pixelLumLocalAverage = 0.5 * (pixelLum2 + pixelLumCenter);
    }

    vec2 currentUv = texCoord;
    if (isHorizontal) {
        currentUv.y += stepLength * 0.5;
    } else {
        currentUv.x += stepLength * 0.5;
    }

    // ============================
    // Iterations:
    // ============================
    vec2 offset = isHorizontal ? vec2(inverseScreenSize.x, 0.0) : vec2(0.0, inverseScreenSize.y);
    
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

    // ============================
    // Estimating the offset:
    // ============================
    float distance1 = isHorizontal ? (texCoord.x - uv1.x) : (texCoord.y - uv1.y);
    float distance2 = isHorizontal ? (uv2.x - texCoord.x) : (uv2.y - texCoord.y);

    bool isDirection1 = distance1 < distance2;
    float distanceFinal = min(distance1, distance2);

    float edgeThickness = (distance1 + distance2);

    // Read in the direction of the closest side of the edge.
    float pixelOffset = - distanceFinal / edgeThickness + 0.5;

    bool ispixelLumCenterSmaller = pixelLumCenter < pixelLumLocalAverage;

    // If the pixelLum at center is smaller than at its neighbour, the delta pixelLum at 
    // each end should be positive (same variation).
    bool correctVariation = ((isDirection1 ? pixelLumEnd1 : pixelLumEnd2) < 0.0) != ispixelLumCenterSmaller;

    // If the pixelLum variation is incorrect, do not offset.
    float finalOffset = correctVariation ? pixelOffset : 0.0;

    // ============================
    // Subpixel antialiasing:
    // ============================
    float pixelLumAverage = (1.0/12.0) * (2.0 * (pixelLumDownUp + pixelLumLeftRight) + 
        pixelLumLeftCorners + pixelLumRightCorners);
    
    float subPixelOffset1     = clamp(abs(pixelLumAverage - pixelLumCenter) / pixelLumRange, 0.0, 1.0);
    float subPixelOffset2     = (-2.0 * subPixelOffset1 + 3.0) * subPixelOffset1 * subPixelOffset1;
    float subPixelOffsetFinal = subPixelOffset2 * subPixelOffset2 * SUBPIXEL_QUALITY;

    // Biggest of the two offsets.
    finalOffset = max(finalOffset, subPixelOffsetFinal);

    vec2 finalUV = texCoord;
    if (isHorizontal) {
        finalUV.y += finalOffset * stepLength;
    } else {
        finalUV.x += finalOffset * stepLength;
    }

    return texture(renderedTexture, finalUV);
}

// Calculates the luminosity of a sample.
float FxaaLuma(vec3 rgb) {
    return rgb.y * (0.587/0.299) + rgb.x;
}

vec4 fxaaMethod2() {
    vec4 outColor = vec4(0.0);
    float FXAA_SPAN_MAX = 8.0;
    float FXAA_REDUCE_MUL = 1.0/8.0;
    float FXAA_REDUCE_MIN = 1.0/128.0;
    
    // Sample 4 texels including the middle one.
    // Since the texture is in UV coordinate system, the Y is
    // therefore, North direction is –ve and south is +ve.
    vec3 rgbNW = texture(renderedTexture,texCoord+(vec2(-1.,-1.) * inverseScreenSize)).xyz;
    vec3 rgbNE = texture(renderedTexture,texCoord+(vec2(1.,-1.) * inverseScreenSize)).xyz;
    vec3 rgbSW = texture(renderedTexture,texCoord+(vec2(-1.,1.) * inverseScreenSize)).xyz;
    vec3 rgbSE = texture(renderedTexture,texCoord+(vec2(1.,1.) * inverseScreenSize)).xyz;
    vec3 rgbM = texture(renderedTexture,texCoord).xyz;

    float lumaNW = FxaaLuma(rgbNW); // Top-Left
    float lumaNE = FxaaLuma(rgbNE); // Top-Right
    float lumaSW = FxaaLuma(rgbSW); // Bottom-Left
    float lumaSE = FxaaLuma(rgbSE); // Bottom-Right

    float lumaM = FxaaLuma(rgbM); // Middle

    // Get the edge direction, since the y components are inverted
    // be careful to invert the resultant x
    vec2 dir;
    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
    dir.y = ((lumaNW + lumaSW) - (lumaNE + lumaSE));

    // Now, we know which direction to blur,
    // But far we need to blur in the direction?
    float dirReduce = max((lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL),FXAA_REDUCE_MIN);
    float rcpDirMin = 1.0/(min(abs(dir.x),abs(dir.y))+dirReduce);

    dir = min(vec2( FXAA_SPAN_MAX, FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX,-FXAA_SPAN_MAX), dir*rcpDirMin)) * inverseScreenSize;

    vec3 rgbA = (1.0/2.0)*(texture(renderedTexture, texCoord.xy + dir * (1.0/3.0 - 0.5)).xyz + 
        texture(renderedTexture, texCoord.xy + dir * (2.0/3.0 - 0.5)).xyz);
    vec3 rgbB = rgbA * (1.0/2.0) + (1.0/4.0) * (texture(renderedTexture, texCoord.xy + dir * (0.0/3.0 - 0.5)).xyz + 
        texture(renderedTexture, texCoord.xy + dir * (3.0/3.0 - 0.5)).xyz);

    float lumaB = FxaaLuma(rgbB);
    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));
    
    if((lumaB < lumaMin) || (lumaB > lumaMax)) {
        outColor = vec4(rgbA, 1.0);
    } else{
        outColor = vec4(rgbB, 1.0);
    }
    return outColor;
}

void main() {
    //vec4 fxaaColor = fxaaMethod1();
    vec4 fxaaColor = fxaaMethod2();
    if (fxaaColor == vec4(-1.0))
        return;
    aaFinalColor = fxaaColor;
}