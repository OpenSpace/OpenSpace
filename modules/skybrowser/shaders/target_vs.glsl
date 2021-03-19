#version __CONTEXT__

uniform mat4 ModelTransform;
uniform mat4 ViewProjectionMatrix;

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;

out vec2 vs_st;
out vec4 vs_position;

void main(){
    vs_st = in_st;
    vs_position = ViewProjectionMatrix * ModelTransform * in_position;
    gl_Position = vec4(vs_position);
}
