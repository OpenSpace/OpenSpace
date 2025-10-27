#version __CONTEXT__

layout(location=0) in vec2 vertex_position;

out vec2 uv;

void main()
{
    uv = 2 * (vertex_position - 0.5);
    gl_Position = vec4(vertex_position, 0.0, 1.0);
}
