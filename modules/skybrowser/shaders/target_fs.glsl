uniform sampler2D texture1;
uniform float borderWidth;
uniform vec2 targetDimensions;
uniform bool showCrosshair;
uniform vec4 borderColor;


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

    float ratio = targetDimensions.y / targetDimensions.x;

    // draw crosshair
    float crossWidth = 0.1f;
    vec3 crosshair = vec3(crossLine(crossWidth*ratio, (vs_st).x) + crossLine(crossWidth, (vs_st).y));

    // draw square border
    float borderBottomLeft = step(borderWidth*ratio, vs_st.x) * step(borderWidth*ratio, (1.0)-vs_st.x);
    float borderTopRight = step(borderWidth, vs_st.y) * step(borderWidth, (1.0)-vs_st.y);
    vec3 border = vec3(borderBottomLeft*borderTopRight);

    // show crosshair or border
    frag.color = vec4(1,1,1,1);
    frag.color.rgba = vec4(borderColor);

    if(showCrosshair) {
        frag.color.rgba = vec4(borderColor);
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
