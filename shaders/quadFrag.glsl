#version 330

uniform sampler2D quadTex;
in vec2 texCoord;
out vec4 color;

void main() {
  color = texture(quadTex, texCoord);
}