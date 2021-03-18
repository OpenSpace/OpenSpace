uniform sampler2D texture1;
uniform float Scale;
uniform float BorderWidth;

in vec2 vs_st;
in vec4 vs_position;

#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;

    vec2 bl = step(vec2(BorderWidth),1.0-vs_st);    // bottom-left line 
    vec2 tr = step(vec2(BorderWidth),vs_st);        // top-right line 

    vec3 border = vec3(tr.x * tr.y * bl.x * bl.y);

    frag.color = texture(texture1, vs_st);

    if(border == vec3(1.0)) {

        frag.color.a = 0.0;
    }
    
    return frag;
}
