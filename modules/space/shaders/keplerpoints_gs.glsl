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

#include "powerscaling/powerscalingmath.glsl"

layout(lines) in;
in Data {
  flat float currentRevolutionFraction;
  flat float vertexRevolutionFraction;
} in_data[];

layout(triangle_strip, max_vertices = 4) out;
out Data {
  vec4 viewSpace;
  vec2 texCoord;
  float projectionViewDepth;
} out_data;

uniform dmat4 modelTransform;
uniform dmat4 viewTransform;
uniform mat4 projectionTransform;
uniform float pointSizeExponent;
uniform bool enableMaxSize;
uniform float maxSize;
uniform int renderOption;

// Camera View Direction
uniform vec3 cameraViewDirectionUp;
uniform vec3 cameraViewDirectionRight;

// Camera Normal
uniform dvec3 cameraPositionWorld;
uniform vec3 cameraUpWorld;

const int RenderOptionCameraViewDirection = 0;
const int RenderOptionCameraPositionNormal = 1;


void main() {
  // cFrac is how far along the trail orbit the head of the trail is.
  // v0Frac and v1Frac are how far the two vertices that creates the current line strip
  // are along the trail orbit. The variables span between 0 and 1, where 0 is the
  // beginning of the trail and 1 is the end of the trail (a full orbit).
  float cFrac = in_data[0].currentRevolutionFraction;
  float v0Frac = in_data[0].vertexRevolutionFraction;
  float v1Frac = in_data[1].vertexRevolutionFraction;

  // Interpolate position of current position of the trail head
  float dFrac = cFrac - v0Frac;
  float vFrac = v1Frac - v0Frac;
  float percentage = dFrac / vFrac;

  vec4 pos = mix(gl_in[0].gl_Position, gl_in[1].gl_Position, percentage);

  // Calculate current vertex position to world space
  dvec4 vertPosWorldSpace = modelTransform * pos;
  vec3 camPosToVertPos = vec3(cameraPositionWorld - vertPosWorldSpace.xyz);

  vec3 up;
  vec3 right;
  // Calculate new axis for plane
  if (renderOption == RenderOptionCameraViewDirection) {
    up = cameraViewDirectionUp;
    right = cameraViewDirectionRight;
  }
  else {
    // Camera Position Normal
    vec3 normal = normalize(camPosToVertPos);
    right = normalize(cross(cameraUpWorld, normal));
    up = normalize(cross(normal, right));
  }

  // Calculate size of points
  float initialSize = pow(10.0, pointSizeExponent);
  right *= initialSize;
  up *= initialSize;

  float opp = length(right);
  float adj = length(camPosToVertPos);
  float angle = atan(opp / adj);
  float maxAngle = radians(maxSize * 0.5);

  // Controls the point size
  if (enableMaxSize && (angle > maxAngle) && (adj > 0.0)) {
    float correction = (adj * tan(maxAngle)) / opp;
    right *= correction;
    up *= correction;
  }

  // Calculate and set corners of the new quad
  dvec4 p0World = vertPosWorldSpace + vec4(up - right, 0.0);
  dvec4 p1World = vertPosWorldSpace + vec4(-right - up, 0.0);
  dvec4 p2World = vertPosWorldSpace + vec4(right + up, 0.0);
  dvec4 p3World = vertPosWorldSpace + vec4(right - up, 0.0);

  // Set some additional out parameters
  out_data.viewSpace = z_normalization(
    vec4(projectionTransform * viewTransform * modelTransform * pos)
  );
  out_data.projectionViewDepth = out_data.viewSpace.w;

  dmat4 viewProjectionTransform = projectionTransform * viewTransform;

  // left-top
  gl_Position = z_normalization(vec4(viewProjectionTransform * p0World));
  out_data.texCoord = vec2(0.0, 0.0);
  EmitVertex();

  // left-bot
  gl_Position = z_normalization(vec4(viewProjectionTransform * p1World));
  out_data.texCoord = vec2(1.0, 0.0);
  EmitVertex();

  // right-top
  gl_Position = z_normalization(vec4(viewProjectionTransform * p2World));
  out_data.texCoord = vec2(0.0, 1.0);
  EmitVertex();

  // right-bot
  gl_Position = z_normalization(vec4(viewProjectionTransform * p3World));
  out_data.texCoord = vec2(1.0, 1.0);
  EmitVertex();

  EndPrimitive();
}
