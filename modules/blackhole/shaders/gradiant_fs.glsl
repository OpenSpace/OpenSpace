#include "fragment.glsl"
in vec2 TexCoord;

uniform sampler2D enviromentTexture;

Fragment getFragment() {
    Fragment frag;

    vec4 texColor = texture(enviromentTexture, TexCoord);
    
    frag.color = texColor;

    return frag;
}
