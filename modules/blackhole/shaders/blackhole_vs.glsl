#version __CONTEXT__

in vec2 aPos;
in vec2 aTexCoord;

out vec2 TexCoord; // Pass to fragment shader

void main(void) {
    TexCoord = aTexCoord;
    gl_Position = vec4(aPos,0.0, 1.0);
}
