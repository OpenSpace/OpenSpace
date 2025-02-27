#include "fragment.glsl"
in vec2 TexCoord;

uniform sampler2D enviromentTexture;
uniform sampler2D viewGrid;

const float PI = 3.1415926535897932384626433832795f;

vec2 sphereToUV(vec2 sphereCoords){
    float u = sphereCoords.x / (2.0f * PI) + 0.5f;
    float v = sphereCoords.y / PI;
    
    return vec2(u, v);
}

Fragment getFragment() {
    Fragment frag;
    vec2 sphereCoords = texture(viewGrid, TexCoord).rg;
    vec4 texColor = texture(enviromentTexture, sphereToUV(sphereCoords));
    
    frag.color = texColor;
    return frag;
}