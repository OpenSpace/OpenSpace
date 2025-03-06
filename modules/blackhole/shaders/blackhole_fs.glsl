#include "fragment.glsl"
in vec2 TexCoord;

uniform sampler2D enviromentTexture;
uniform sampler2D viewGrid;
uniform mat4 cameraRotationMatrix;

layout (std430) buffer ssbo_warp_table {
  float schwarzschildWarpTable[];
};

const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;
const float INF = 1.0f/0.0f;


vec2 sphereToUV(vec2 sphereCoords){
    float u = sphereCoords.x / (2.0f * PI) + 0.5f;
    float v = sphereCoords.y / PI;
    
    return vec2(u, v);
}

float getEndAngleFromTable(float phi){
    float endPhiWdithLeastDistance;
    float currentDistance = INF;

    int tableLength = schwarzschildWarpTable.length();

    for(int i = 0; i < tableLength; i += 2){
        float phiDistance = abs(schwarzschildWarpTable[i] - phi);
        if(phiDistance < currentDistance){
            currentDistance = phiDistance;
            endPhiWdithLeastDistance = schwarzschildWarpTable[i+1];
        }
    }
    return endPhiWdithLeastDistance;
}

vec2 localToEnvSphereCoords(vec2 cameraOutSphereCoords){
    float phi = cameraOutSphereCoords.x;
    float theta = cameraOutSphereCoords.y;
    theta = getEndAngleFromTable(theta);

    return vec2(phi, theta);
}

float atan2(float a, float b){
    if (b != 0.0f) return atan(a, b);
    if (a > 0.0f) return PI / 2.0f;
    if (a < 0.0f) return -PI / 2.0f;
        
    return 0.0f;
}

vec2 cartisianToSphereical(vec3 cartisian) {
    float theta = atan2(sqrt(cartisian.x * cartisian.x + cartisian.y * cartisian.y) , cartisian.z);
    float phi = atan2(cartisian.y, cartisian.x);

    return vec2(phi, theta);
}

Fragment getFragment() {
    Fragment frag;

    vec4 cartisianCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));
    cartisianCoords = cameraRotationMatrix * cartisianCoords;
    
    vec2 sphereicaleCoords = cartisianToSphereical(cartisianCoords.xyz);
    vec2 envSphereCoords = localToEnvSphereCoords(sphereicaleCoords);
    
    if (isnan(envSphereCoords.y)) {
        frag.color = vec4(0);
        return frag;
    }

    vec2 uv = sphereToUV(envSphereCoords);
    vec4 texColor = texture(enviromentTexture, uv);
    
    frag.color = texColor;
    return frag;
}