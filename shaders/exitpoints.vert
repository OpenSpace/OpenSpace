#version 430 core

layout(location = 2) in vec3 vertPosition;
uniform mat4 modelViewProjection;

out vec3 vPosition;

void main() {
	gl_Position = modelViewProjection * vec4(vertPosition, 1.0);
	vPosition = vertPosition;
}