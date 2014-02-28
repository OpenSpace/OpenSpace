#version 400 core

layout(location = 5) in vec3 vertPosition;
uniform mat4 modelViewProjection;

in vec4 Position;
out vec4 vPosition;

void main() {
	vec4 vPos = vec4(vertPosition, 1.0);
    gl_Position = modelViewProjection * vPos;
    vPosition = vPos;
}