#version 430 core

layout(location = 2) in vec3 vertPosition;
uniform mat4 modelViewProjection;
uniform mat4 modelTransform;

out vec3 vPosition;
out vec3 worldPosition;

void main() {
	vPosition = vertPosition;
	worldPosition = (modelTransform *vec4(vPosition, 1.0)).xyz;
	gl_Position = modelViewProjection  *vec4(worldPosition, 1.0);
}