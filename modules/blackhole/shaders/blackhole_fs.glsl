#include "fragment.glsl"
in vec2 TexCoord;

uniform sampler2D enviromentTexture;
uniform sampler2D viewGrid;

const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;

vec2 sphereToUV(vec2 sphereCoords){
    float u = sphereCoords.x / (2.0f * PI) + 0.5f;
    float v = sphereCoords.y / PI;

    return vec2(u, v);
}

float atan2(float a, float b){
    if (b != 0.0f) return atan(a, b);
    if (a > 0.0f) return PI / 2.0f;
    if (a < 0.0f) return -PI / 2.0f;
        
    return 0.0f;
}

vec2 cartisianToSphereical(vec2 cartisian) {
    float theta = atan(sqrt(cartisian.x * cartisian.x + cartisian.y * cartisian.y) , VIEWGRIDZ);
    float phi = atan2(cartisian.y, cartisian.x);

    return vec2(phi, theta);
}

Fragment getFragment() {
    Fragment frag;
    vec2 cartisianCoords = texture(viewGrid, TexCoord).xy;
    vec2 sphereicaleCoords = cartisianToSphereical(cartisianCoords);
    vec2 uv = sphereToUV(sphereicaleCoords);

    vec4 texColor = texture(enviromentTexture, uv);  
    frag.color = texColor;
    
    return frag;
}