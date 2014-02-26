#version 400 core

layout(location = 0) in vec2 texCoordinate;
layout(location = 2) in vec3 vertPosition;
uniform mat4 modelViewProjection;

out vec3 vPosition;
out vec2 texCoords;

void main() {
	gl_Position = modelViewProjection * vec4(vertPosition, 1.0);
	// vPosition = gl_Position.xyz;
	vPosition = vertPosition;
	texCoords = texCoordinate;
}