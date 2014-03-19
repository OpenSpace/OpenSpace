#version 330

in vec4 position;
out vec2 texCoord;

void main() {
  gl_Position = position;
  texCoord = position.xy/2.0 + 0.5;
}
