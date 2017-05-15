#version __CONTEXT__

in vec2 in_position;

uniform float height;
uniform float width;
uniform ivec2 res;

uniform mat4 ortho;

void main() {
    const float x = in_position.x * res.x * width + (res.x / 2.0 - width * res.x / 2.0);
    const float y = float(res.y) - in_position.y * height * res.y;

    gl_Position = ortho * vec4(x, y,  0.0, 1.0);
}
