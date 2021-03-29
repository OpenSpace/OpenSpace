uniform sampler2D texture1;
uniform float borderWidth;
uniform vec2 targetDimensions;
uniform bool showCrosshair;
uniform vec3 borderColor;


in vec2 vs_st;
in vec4 vs_position;


float crossLine(in float _width, in float _coord) {

    float center = 0.5f;

    float line = smoothstep(center, center+(_width/2) , _coord) - 
                 smoothstep(center-(_width/2), center, _coord);    
    
    return line;
}

#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;

    // draw crosshair
    float crossWidth = 0.1f;
    float ratio = targetDimensions.y / targetDimensions.x;
    vec3 crosshair = vec3(crossLine(crossWidth*ratio, (vs_st).x) + crossLine(crossWidth, (vs_st).y));

    // draw square border
    vec2 bl = step(vec2(borderWidth),(1.0-vs_st));         // bottom-left line 
    vec2 tr = step(vec2(borderWidth),vs_st);               // top-right line 
    vec3 border = vec3(tr.x * tr.y * bl.x * bl.y);

    // show crosshair or border 
    frag.color = vec4(1,1,1,1);
    frag.color.rgb = vec3(borderColor / 255);

    if(showCrosshair) {

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

