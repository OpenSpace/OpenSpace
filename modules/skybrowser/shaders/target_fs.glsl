uniform sampler2D texture1;
uniform float OcclusionDepth;
uniform float Alpha;

in vec2 vs_st;
in vec4 vs_position;

#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;

    vec3 color = vec3(1.0);
    frag.color = vec4(color, 1.0); 

    return frag;
}