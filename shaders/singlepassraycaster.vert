#version 400 core

// Based on http://prideout.net/blog/?p=64

layout(location = 0) in vec4 Position;
out vec4 vPosition;
uniform mat4 modelViewProjection;

void main() {
    gl_Position = modelViewProjection * Position;
    vPosition = Position;
}