#version __CONTEXT__

#include "powerscaling/powerscaling_vs.glsl"

layout(location = 0) in vec3 in_position;
layout(location = 1) in float in_density;
layout(location = 2) in float in_selected;

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform double unitToMeter;
uniform float pointSize;

out Data {
  flat float density;
  flat float selected;
  float depth;
} out_data;

void main() {
  dvec4 viewPosition = modelViewTransform * dvec4(dvec3(in_position) * unitToMeter, 1.0);
  gl_Position = z_normalization(projectionTransform * vec4(viewPosition));
  gl_PointSize = pointSize;
  out_data.density = in_density;
  out_data.selected = in_selected;
  out_data.depth = gl_Position.w;
}
