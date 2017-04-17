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

#version __CONTEXT__

uniform bool isMorphing;

uniform float stepSize;
uniform float minLength;
uniform float clippingRadius;
uniform float state_progression;

uniform int integrationMethod;
uniform int maxComponentOutput;

uniform mat4 modelViewProjection;

uniform sampler3D volumeTexture;
uniform sampler3D nextVolumeTexture;

uniform vec2 domainWidthLimits;
uniform vec2 domainDepthLimits;
uniform vec2 domainHeightLimits;

uniform vec3 domainMins;
// uniform vec3 domainMaxs;
uniform vec3 domainDiffs;

in vec4 vs_color[];

layout(points) in;
layout(line_strip, max_vertices = 204) out;

out float gs_depth;
// uniform vec3 bMins;
// uniform vec3 bMaxs;
// uniform vec3 bDiff;
// uniform int maxTracingSteps
// uniform float scale;
// out vec4 gs_color;
// out vec3 gs_color;

#include "PowerScaling/powerScaling_vs.hglsl"

// Normalize coodinates to fit in range [0,1] (needed for texture lookups)
vec3 normalizeCoords(in vec3 unscaledPos) {
    return (unscaledPos - domainMins) / (domainDiffs);
}

void addVertex(in vec3 unscaledPos) {
    vec4 position_meters = vec4(unscaledPos * 6371000.0, 1.0); // Scale by 1 earth radii TODO change to input scale uniform
    gl_Position = z_normalization(modelViewProjection * position_meters.xyzw);
    gs_depth = gl_Position.w;
    EmitVertex();
}

vec3 getFieldVecAtPosition(in vec3 unscaledPos) {
    vec3 normCoords = normalizeCoords(unscaledPos); // gl texture coords (range 0 to 1)
    if (isMorphing) {
        vec3 prevMeasurement = texture(volumeTexture, normCoords).xyz;
        vec3 nextMeasurement = texture(nextVolumeTexture, normCoords).xyz;
        return prevMeasurement * (1.0 - state_progression) +
               nextMeasurement * state_progression;
    }

    return texture(volumeTexture, normCoords).xyz; // TODO only vec3?
}

vec3 eulerStep(in vec3 unscaledPos) {
    return unscaledPos + stepSize * normalize(getFieldVecAtPosition(unscaledPos));
}

vec3 rk4(in vec3 unscaledPos) {
    vec3 k1 = stepSize * normalize(getFieldVecAtPosition(unscaledPos));
    vec3 k2 = stepSize * normalize(getFieldVecAtPosition(unscaledPos + k1 / 2.0));
    vec3 k3 = stepSize * normalize(getFieldVecAtPosition(unscaledPos + k2 / 2.0));
    vec3 k4 = stepSize * normalize(getFieldVecAtPosition(unscaledPos + k3));

    // vec3 normCoords = normalizeCoords(unscaledPos);
    // vec4 interpTexVal = texture(volumeTexture, normCoords); // TODO only vec3?
    // vec3 k1 = stepSize * normalize(interpTexVal.xyz);

    // normCoords = normalizeCoords(unscaledPos + k1 / 2.0);
    // interpTexVal = texture(volumeTexture, normCoords);
    // vec3 k2 = stepSize * normalize(interpTexVal.xyz);

    // normCoords = normalizeCoords(unscaledPos + k2 / 2.0);
    // interpTexVal = texture(volumeTexture, normCoords);
    // vec3 k3 = stepSize * normalize(interpTexVal.xyz);

    // normCoords = normalizeCoords(unscaledPos + k3);
    // interpTexVal = texture(volumeTexture, normCoords);
    // vec3 k4 = stepSize * normalize(interpTexVal.xyz);

    return unscaledPos + (1.0/6.0) * (k1 + 2.0*k2 + 2.0*k3 + k4);
}

// Assumes GSM frame
// Ensure new line is not crossing clipping sphere
bool validateLine(in vec3 prevUnscaledPos, in vec3 newUnscaledPos) {
    vec3 diff = newUnscaledPos - prevUnscaledPos;
    float a = diff.x*diff.x + diff.y*diff.y + diff.z*diff.z;
    float b = 2.0*(diff.x*newUnscaledPos.x + diff.y*newUnscaledPos.y + diff.z*newUnscaledPos.z); // PROVIDED GSM REFERENCE FRAME (EARTH IS AT (0,0,0))
    float c = newUnscaledPos.x*newUnscaledPos.x + newUnscaledPos.y*newUnscaledPos.y + newUnscaledPos.z*newUnscaledPos.z - clippingRadius*clippingRadius /* - 2.0*(0.0) */ ;

    // Larger than zero implies collision with sphere => not valid
    if (b*b-4.0*a*c > 0) {
        return false;
    }
    // newUnscaledPos += newUnscaled;

    return true;
}

bool validatePoint(in vec3 newUnscaledPoint) {
    // Check if inside voxel grid (domain) and outside of clipping radius
    if (newUnscaledPoint.x < domainWidthLimits.x  || newUnscaledPoint.x > domainWidthLimits.y  ||
        newUnscaledPoint.y < domainDepthLimits.x  || newUnscaledPoint.y > domainDepthLimits.y  ||
        newUnscaledPoint.z < domainHeightLimits.x || newUnscaledPoint.z > domainHeightLimits.y ||
        sqrt(pow(newUnscaledPoint.x,2) + pow(newUnscaledPoint.y,2) + pow(newUnscaledPoint.z,2)) < clippingRadius  ) {
        return false;
    }
    return true;
}

bool findNextPoint(inout vec3 unscaledPos) {
    vec3 newUnscaledPoint;
    switch (integrationMethod) {
        case 0 : {
            newUnscaledPoint = eulerStep(unscaledPos);
            break;
        } case 1 : {
            newUnscaledPoint = rk4(unscaledPos);
            break;
        } default :
            break;
    }

    if (!validatePoint(newUnscaledPoint)) {
        return false;
    }

    // Make sure line is not crossing the clipping sphere
    if (!validateLine(unscaledPos, newUnscaledPoint)) {
        return false;
    }

    unscaledPos = newUnscaledPoint;

    return true;
}

void traceFieldline(in vec3 unscaledSeedPoint) {

    addVertex(unscaledSeedPoint);

    vec3 unscaledPos = unscaledSeedPoint;

    // TODO range of loop should be specified as a uniform (depends on GL_MAX_GEOMETRY_VERTICES)
    for (int i = 0 ; i < 203 ; ++i) {
        bool isValidPoint = findNextPoint(unscaledPos);
        if (!isValidPoint) {
            break;
        }
        addVertex(unscaledPos);
    }
}

void main() {
    vec3 unscaledSeedPoint = gl_in[0].gl_Position.xyz;
    // Check that seed point is valid
    if (validatePoint(unscaledSeedPoint)) {
        // Seed point is valid, trace line in one direction! (depends on stepSize)
        traceFieldline(unscaledSeedPoint);
    }
    EndPrimitive();
}
