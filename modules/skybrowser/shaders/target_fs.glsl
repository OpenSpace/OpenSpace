uniform sampler2D texture1;
uniform float borderWidth;
uniform vec2 targetRatio;
uniform float fieldOfView;

in vec2 vs_st;
in vec4 vs_position;

float box(in vec2 _st, in vec2 _size){
    _size = vec2(0.5) - _size*0.5;
    vec2 uv = smoothstep(_size,_size, vs_st);
    uv *= smoothstep(_size,_size,vec2(1.0)-vs_st);
    return uv.x*uv.y;
}

float cross(in vec2 _st, float _size){
    return  box(vs_st, vec2(_size/.6, _size/3.)) +
            box(vs_st, vec2(_size/5.,_size/.4));
}

#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;


    // draw cross
    vec3 crosshair = vec3(cross(vs_st, 0.1));

    // draw square border
    vec2 bl = step(vec2(borderWidth),(1.0-vs_st)*targetRatio);    // bottom-left line 
    vec2 tr = step(vec2(borderWidth),vs_st*targetRatio);        // top-right line 
    vec3 border = vec3(tr.x * tr.y * bl.x * bl.y);

    frag.color = vec4(1,1,1,1);

    if(fieldOfView < 10.f) {
        frag.color = vec4(1,1,1,1);
        if(crosshair == vec3(0.0)) {
            frag.color.a = 0.0;
        }
    }
    else {
        if(border == vec3(1.0)) {
            frag.color.a = 0.0;

        }
    }

    return frag;
}


    /*
    if(crosshair == vec3(0.0) {
        frag.color.a = 0.0;
    }
    */


    /*
        // draw square border
    vec2 bl = step(vec2(borderWidth),(1.0-vs_st)*targetRatio);    // bottom-left line 
    vec2 tr = step(vec2(borderWidth),vs_st*targetRatio);        // top-right line 
    vec3 border = vec3(tr.x * tr.y * bl.x * bl.y);

    frag.color = vec4(1,1,1,1);

    if(border == vec3(1.0)) {
        frag.color.a = 0.0;
    }

    */
    


