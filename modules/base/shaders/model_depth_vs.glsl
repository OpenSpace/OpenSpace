#version __CONTEXT__

layout(location = 0) in vec4 in_position;

uniform dmat4 model;
uniform dmat4 light_vp;
uniform mat4 meshTransform;

void main() {
  gl_Position = vec4(light_vp * model * meshTransform * in_position);
}
