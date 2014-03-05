#version 400 core

// layout(location = 0) in vec3 vertPosition;
in vec4 Position;

uniform mat4 modelViewProjection;
out vec4 vPosition;

void main() {
	// vec4 Position = vec4(vertPosition, 1.0);
    gl_Position = modelViewProjection * Position;
    vPosition = Position;
}