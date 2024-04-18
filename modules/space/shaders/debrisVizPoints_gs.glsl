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
flat in double currentRevolutionFraction[];
flat in double vertexRevolutionFraction[];

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
flat out int skip;

dvec4 dz_normalization(dvec4 dv_in) {
  dvec4 dv_out = dv_in;
  dv_out.z = 0;
  return dv_out;
}

void main() {
  skip = 0;
  
  double cFrac = currentRevolutionFraction[0];
  double v0Frac = vertexRevolutionFraction[0];
  double v1Frac = vertexRevolutionFraction[1];
  if (cFrac >= v0Frac && cFrac <= v1Frac) {

    // Interpolate position of point
    double dFrac = abs(cFrac - v0Frac);
    double vFrac = abs(v1Frac - v0Frac);
    double percentage = dFrac / vFrac;

    dvec4 v0Weighted = (1.0 - percentage) * gl_in[0].gl_Position;
    dvec4 v1Weighted = (percentage) * gl_in[1].gl_Position;

    dvec4 pos = v0Weighted + v1Weighted;
    // ==========================

    // Calculate current vertex position to world space
    dvec4 vertPosWorldSpace = modelTransform * pos;

    // Calculate new axis for plane
    dvec3 normal = normalize(cameraPositionWorld - vertPosWorldSpace.xyz);
    dvec3 right = normalize(cross(cameraUpWorld, normal));
    dvec3 up = normalize(cross(normal, right));

    // Calculate size of points
    double initialSize = pow(10.0, pointSizeExponent);
    right *= initialSize;
    up *= initialSize;

    double opp = length(right);
    double adj = length(cameraPositionWorld.xyz - vertPosWorldSpace.xyz);
    float angle = atan(float(opp/adj));
    float maxAngle = radians(maxSize * 0.5);

    // Controls the point size
    if (enableMaxSize && (angle > maxAngle) && (adj > 0.0)) {
      double correction = (adj * tan(maxAngle)) / opp;
      right *= correction;
      up *= correction;
    }

    // Calculate and set corners of the quad
    dvec4 p0World = vertPosWorldSpace + (dvec4(up-right, 0.0));
    dvec4 p1World = vertPosWorldSpace + (dvec4(-right-up,0.0));
    dvec4 p2World = vertPosWorldSpace + (dvec4(right+up, 0.0));
    dvec4 p3World = vertPosWorldSpace + (dvec4(right-up, 0.0));

    dmat4 ViewProjectionTransform = dmat4(projectionTransform) * viewTransform;

    // Set some additional out parameters
    viewSpace = z_normalization(
      vec4(projectionTransform * viewTransform * modelTransform * pos)
    );
    projectionViewDepth = viewSpace.w;

    //left-top
    vec4 p0Screen = vec4(dz_normalization(ViewProjectionTransform * p0World));
    gl_Position = p0Screen;
    texCoord = vec2(0.0, 0.0);
    EmitVertex();

    //left-bot
    vec4 p1Screen = vec4(dz_normalization(ViewProjectionTransform * p1World));
    gl_Position = p1Screen;
    texCoord = vec2(1.0, 0.0);
    EmitVertex();

    //right-top
    vec4 p2Screen = vec4(dz_normalization(ViewProjectionTransform * p2World));
    gl_Position = p2Screen;
    texCoord = vec2(0.0, 1.0);
    EmitVertex();

    //right-bot
    vec4 p3Screen = vec4(dz_normalization(ViewProjectionTransform * p3World));
    gl_Position = p3Screen;
    texCoord = vec2(1.0, 1.0);
    EmitVertex();
    
    // Primitive
    EndPrimitive();

  } 
  else {
    skip = 1;
    EmitVertex();
    EndPrimitive();
  }
}
