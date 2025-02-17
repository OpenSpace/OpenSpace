#include "fragment.glsl"
in vec2 TexCoord;

Fragment getFragment() {
    Fragment frag;
    // Gradient from blue (bottom-left) to purple (top-right)
    frag.color = vec4(TexCoord.x, TexCoord.y, 1.0 - TexCoord.x, 1.0);
    return frag;
}
