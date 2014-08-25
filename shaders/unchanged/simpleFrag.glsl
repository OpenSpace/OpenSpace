#version 440
out vec4 vFragColor;

uniform vec3 Color;

void main(void)
{
    // calculate normal from texture coordinates
    vFragColor = vec4(Color,1);
}