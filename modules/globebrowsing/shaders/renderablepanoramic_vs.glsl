#version __CONTEXT__

uniform mat4 ModelTransform;
uniform mat4 ViewProjection;

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

#include "PowerScaling/powerScaling_vs.hglsl"

out vec2 vs_st;
out vec4 vs_position;
out vec3 vs_normal;

void main()
{
    vec4 tmp = in_position;

    mat4 mt = ModelTransform;

    //vec4 position = pscTransform(tmp, mt);

    vec4 position = mt * tmp;

    //vec4 position = tmp;

    vs_position = tmp;
    vs_st = in_st;
    vs_normal = in_normal;

    position = ViewProjection * position;
    gl_Position =  z_normalization(position);
}
