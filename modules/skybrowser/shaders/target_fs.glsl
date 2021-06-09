uniform sampler2D texture;
uniform float borderWidth;
uniform vec2 targetDimensions;
uniform bool showCrosshair;
uniform bool showCrosshairInTarget;
uniform vec4 borderColor;


in vec2 vs_st;
in vec4 vs_position;


float crossLine(in float _width, in float _coord) {
    float center = 0.5f;
    float line = smoothstep(center, center+(_width/2), _coord)
               - smoothstep(center-(_width/2), center, _coord);
    return line;
}

#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;

    float ratio = targetDimensions.y / targetDimensions.x;

    // draw square border
    float border_bl = step(borderWidth*ratio, vs_st.x) * step(borderWidth*ratio, (1.0)-vs_st.x);
    float border_tr = step(borderWidth, vs_st.y) * step(borderWidth, (1.0)-vs_st.y);
    vec3 border = vec3(border_bl*border_tr);

    // draw crosshair inside square border
    float crosshair_border_linewidth = 0.06f;
    float border_crosshair_bl = step(borderWidth*ratio*4, vs_st.x) * step(borderWidth*ratio*4, (1.0)-vs_st.x);
    float border_crosshair_tr = step(borderWidth*4, vs_st.y) * step(borderWidth*4, (1.0)-vs_st.y);
    vec3 crosshair_small = vec3(border_crosshair_bl*border_crosshair_tr);

    vec3 crosshair_inside_border = vec3(crossLine(crosshair_small.x * crosshair_border_linewidth, (vs_st).x)
    + crossLine(crosshair_small.y * crosshair_border_linewidth, (vs_st).y));
    vec3 crosshair_and_border =  (1.0 - border) + crosshair_inside_border;

    // draw crosshair
    float crosshair_linewidth = 0.14f;
    vec3 crosshair = vec3(crossLine(crosshair_linewidth*ratio*1.1, vs_st.x) + crossLine(crosshair_linewidth, vs_st.y));

    // show crosshair or border or both
    frag.color = vec4(1,1,1,1);
    frag.color.rgba = vec4(borderColor);

    if(showCrosshair) {
        frag.color.rgba = vec4(borderColor);
        if(crosshair == vec3(0.0)) {
            frag.color.a = 0.0;
        }
    }
    else if(showCrosshairInTarget) {
        frag.color.rgba = vec4(borderColor);
        if(crosshair_and_border == vec3(0.0)) {
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
