#version __CONTEXT__

#include "powerscaling/powerscaling_vs.glsl"

layout(location = 0) in vec3 in_position;

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform double unitToMeter;

out Data {
  float depth;
} out_data;

void main() {
  dvec4 viewPosition = modelViewTransform * dvec4(dvec3(in_position) * unitToMeter, 1.0);
  gl_Position = z_normalization(projectionTransform * vec4(viewPosition));
  out_data.depth = gl_Position.w;
}
