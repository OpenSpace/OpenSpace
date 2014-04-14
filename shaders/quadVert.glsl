#version 330

layout(location = 0) in vec2 texCoordinate;
layout(location = 2) in vec3 vertPosition;

in vec4 position;
out vec2 texCoord;

const vec2 screenScale = vec2(0.5, 0.5);

void main() {
	texCoord = vertPosition.xy*screenScale+screenScale; // scale vertex attribute to [0-1] range
	gl_Position = vec4(vertPosition.xy, 0.0, 1.0);
}
