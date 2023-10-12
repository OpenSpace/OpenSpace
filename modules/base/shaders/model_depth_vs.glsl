#version __CONTEXT__

layout(location = 0) in vec4 in_position;

uniform dmat4 model;
uniform dmat4 view;
uniform dmat4 projection;
uniform mat4 meshTransform;

void main() {
  gl_Position = vec4(projection * view * model * in_position);
}
