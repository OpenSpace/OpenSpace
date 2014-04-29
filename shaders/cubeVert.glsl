#version 330

uniform mat4 projectionMatrix;
uniform mat4 viewMatrix;
uniform mat4 modelMatrix;

in vec4 position;
out vec4 color;

void main() {
  gl_Position = projectionMatrix * viewMatrix * modelMatrix * position;
  color = position;
}