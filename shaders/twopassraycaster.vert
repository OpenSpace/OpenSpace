#version 400 core

layout(location = 0) in vec2 texCoordinate;
layout(location = 2) in vec3 vertPosition;

out vec3 vPosition;
out vec2 texCoords;

// Source: http://stackoverflow.com/questions/2588875/whats-the-best-way-to-draw-a-fullscreen-quad-in-opengl-3-2
const vec2 screenScale = vec2(0.5, 0.5);

void main() {
	texCoords = vertPosition.xy*screenScale+screenScale; // scale vertex attribute to [0-1] range
	gl_Position = vec4(vertPosition.xy, 0.0, 1.0);
}