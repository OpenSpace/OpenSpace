#version __CONTEXT__

in vec2 in_position;

uniform float height;
uniform ivec2 res;

uniform mat4 ortho;

void main() {
    float y = float(res.y) - in_position.y * height * res.y;

    gl_Position = ortho * vec4(in_position.x * res.x, y, 0.0, 1.0);
}
