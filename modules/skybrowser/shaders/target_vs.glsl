#version __CONTEXT__

uniform mat4 modelTransform;
uniform mat4 viewProj;

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;

out vec2 vs_st;
out vec4 vs_position;

void main(){
    vs_st = in_st;
    vs_position = viewProj * modelTransform * in_position;
    gl_Position = vec4(vs_position);
}
