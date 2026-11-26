#include "fragment.glsl"

in vec2 vFragPos;
in vec4 vColor;
in vec3 vConic;
in vec4 vViewPos;

Fragment getFragment() {
    // Evaluate full 2D Gaussian quadratic form: -0.5 * (a*x^2 + 2*b*x*y + c*y^2)
    float power = -0.5 * (
        vConic.x * vFragPos.x * vFragPos.x +
        2.0 * vConic.y * vFragPos.x * vFragPos.y +
        vConic.z * vFragPos.y * vFragPos.y
    );

    if (power > 0.0) {
        discard;
    }

    // vColor.a already includes globalOpacity from splat_vs.glsl
    float alpha = min(0.99, vColor.a * exp(power));
    if (alpha < 1.0 / 255.0) {
        discard;
    }

    Fragment frag;
    // Premultiplied alpha output matching GL_ONE, GL_ONE_MINUS_SRC_ALPHA
    frag.color = vec4(vColor.rgb * alpha, alpha);
    frag.depth = gl_FragCoord.z;
    frag.gPosition = vViewPos;
    frag.gNormal = vec4(0.0, 0.0, 1.0, 0.0);
    return frag;
}