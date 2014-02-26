#version 400 core

in vec4 Position;
out vec4 vPosition;
uniform mat4 modelViewProjection;

void main()
{
    gl_Position = modelViewProjection * Position;
    vPosition = Position;
}