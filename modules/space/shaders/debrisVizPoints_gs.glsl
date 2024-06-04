/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include "PowerScaling/powerScalingMath.hglsl"

layout(lines) in;
flat in float currentRevolutionFraction[];
flat in float vertexRevolutionFraction[];

uniform dmat4 modelTransform;
uniform dmat4 viewTransform;
uniform mat4 projectionTransform;
uniform dvec3 cameraPositionWorld;
uniform dvec3 cameraUpWorld;
uniform float pointSizeExponent;
uniform bool enableMaxSize;
uniform float maxSize;

layout(triangle_strip, max_vertices = 4) out;
out float projectionViewDepth;
out vec4 viewSpace;
out vec2 texCoord;

void main() {
  // cFrac is how far along the trail orbit the head of the trail is.
  // v0Frac and v1Frac are how far the two vertices that creates the current line strip
  // are along the trail orbit. The variables span between 0 and 1, where 0 is the
  // beginning of the trail and 1 is the end of the trail (a full orbit).
  float cFrac = currentRevolutionFraction[0];
  float v0Frac = vertexRevolutionFraction[0];
  float v1Frac = vertexRevolutionFraction[1];

  // Only create/emit vertices for segments where the head of the trail falls within
  if (cFrac >= v0Frac && cFrac <= v1Frac) {

    // Interpolate position of current position of the trail head
    float dFrac = abs(cFrac - v0Frac);
    float vFrac = abs(v1Frac - v0Frac);
    float percentage = dFrac / vFrac;
    
    vec4 v0Weighted = (1.0 - percentage) * gl_in[0].gl_Position;
    vec4 v1Weighted = (percentage) * gl_in[1].gl_Position;
    vec4 pos = v0Weighted + v1Weighted;
    // ==========================

    // Cast to float based data types
    mat4 modelTrans = mat4(modelTransform);
    mat4 viewTrans = mat4(viewTransform);
    vec3 camPosWorld = vec3(cameraPositionWorld);

    // Calculate current vertex position to world space
    vec4 vertPosWorldSpace = modelTrans * pos;

    // Calculate new axis for plane
    vec3 normal = vec3(normalize(camPosWorld - vertPosWorldSpace.xyz));
    vec3 right = normalize(cross(vec3(cameraUpWorld), normal));
    vec3 up = normalize(cross(normal, right));

    // Calculate size of points
    float initialSize = pow(10.0, pointSizeExponent);
    right *= initialSize;
    up *= initialSize;

    float opp = length(right);
    float adj = length(camPosWorld - vertPosWorldSpace.xyz);
    float angle = atan(float(opp/adj));
    float maxAngle = radians(maxSize * 0.5);

    // Controls the point size
    if (enableMaxSize && (angle > maxAngle) && (adj > 0.0)) {
      float correction = (adj * tan(maxAngle)) / opp;
      right *= correction;
      up *= correction;
    }

    // Calculate and set corners of the new quad
    vec4 p0World = vertPosWorldSpace + vec4(up-right, 0.0);
    vec4 p1World = vertPosWorldSpace + vec4(-right-up,0.0);
    vec4 p2World = vertPosWorldSpace + vec4(right+up, 0.0);
    vec4 p3World = vertPosWorldSpace + vec4(right-up, 0.0);

    mat4 ViewProjectionTransform = projectionTransform * viewTrans;

    // Set some additional out parameters
    viewSpace = z_normalization(projectionTransform * viewTrans * modelTrans * pos);
    projectionViewDepth = viewSpace.w;

    // left-top
    vec4 p0Screen = z_normalization(ViewProjectionTransform * p0World);
    gl_Position = p0Screen;
    texCoord = vec2(0.0, 0.0);
    EmitVertex();

    // left-bot
    vec4 p1Screen = z_normalization(ViewProjectionTransform * p1World);
    gl_Position = p1Screen;
    texCoord = vec2(1.0, 0.0);
    EmitVertex();

    // right-top
    vec4 p2Screen = z_normalization(ViewProjectionTransform * p2World);
    gl_Position = p2Screen;
    texCoord = vec2(0.0, 1.0);
    EmitVertex();

    // right-bot
    vec4 p3Screen = z_normalization(ViewProjectionTransform * p3World);
    gl_Position = p3Screen;
    texCoord = vec2(1.0, 1.0);
    EmitVertex();
  } 

  EndPrimitive();
}
