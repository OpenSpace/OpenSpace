uniform sampler2D texture1;
uniform float borderWidth;
uniform vec2 targetRatio;

in vec2 vs_st;
in vec4 vs_position;

#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;

    // draw square border
    vec2 bl = step(vec2(borderWidth),(1.0-vs_st)*targetRatio);    // bottom-left line 
    vec2 tr = step(vec2(borderWidth),vs_st*targetRatio);        // top-right line 
    vec3 border = vec3(tr.x * tr.y * bl.x * bl.y);

    frag.color = vec4(1,1,1,1);

    if(border == vec3(1.0)) {
        frag.color.a = 0.0;
    }
    
    return frag;
}

