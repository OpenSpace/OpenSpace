#version 430

in vec4 position;
out vec2 texCoord;

void main() {
	gl_Position = position;
	texCoord = 0.5 + position.xy / 2.0;
}
