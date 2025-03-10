#include "fragment.glsl"
in vec2 TexCoord;


#define SHOW_BLACK_HOLE 1


uniform sampler2D environmentTexture;
uniform sampler2D viewGrid;
uniform mat4 cameraRotationMatrix;
uniform float cameraToAnchorNodeDistance;

layout (std430) buffer ssbo_warp_table {
  float schwarzschildWarpTable[];
};


const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;
const float INF = 1.0f/0.0f;

float bstWarpTable(float phi){
    float midPhi = -1.0f;
    float deltaPhi = -1.0f;

    float minDeltaPhi = INF;
    int index = -1;

    int left = 0;
    int mid = -1;
    int right = schwarzschildWarpTable.length() / 2 - 1;

    while(left <= right){
        mid = (left + right) / 2;
        midPhi = schwarzschildWarpTable[mid * 2];

        deltaPhi = abs(midPhi - phi);
        if(deltaPhi < minDeltaPhi){
            minDeltaPhi = deltaPhi;
            index = mid;
        }

        if (phi > midPhi) {
            right = mid - 1;
        } else {
            left = mid + 1;
        }
    }

    return (index != -1) ? schwarzschildWarpTable[index * 2 + 1] : 0.0f;
}

float getEndAngleFromTable(float phi){
    return bstWarpTable(phi);
}

vec2 applyBlackHoleWarp(vec2 cameraOutSphereCoords){
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

vec2 cartesianToSpherical(vec3 cartisian) {
    float theta = atan2(sqrt(cartisian.x * cartisian.x + cartisian.y * cartisian.y) , cartisian.z);
    float phi = atan2(cartisian.y, cartisian.x);

    return vec2(phi, theta);
}

vec3 sphericalToCartesian(float phi, float theta){
    float x = sin(theta)*cos(phi);
    float y = sin(theta)*sin(phi);
    float z = cos(theta);

    return vec3(x, y, z);
}

vec2 sphericalToUV(vec2 sphereCoords){
    float u = sphereCoords.x / (2.0f * PI) + 0.5f;
    float v = mod(sphereCoords.y, PI) / PI;
    
    return vec2(u, v);
}

float lerp(float start, float end, float t) {
    return start + t * (end - start);
}

Fragment getFragment() {
    Fragment frag;

    vec4 viewCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));
    vec4 rotatedViewCoords = cameraRotationMatrix * viewCoords;
    
    vec2 sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
    
    vec2 envMapSphericalCoords;
    #if SHOW_BLACK_HOLE == 1
    // Apply black hole warping to spherical coordinates
    envMapSphericalCoords = applyBlackHoleWarp(sphericalCoords);
    if (isnan(envMapSphericalCoords.y)) {
        // If inside the event horizon
        frag.color = vec4(0.0f);
        return frag;
    }
    #else
    envMapSphericalCoords = sphericalCoords;
    #endif

    // Init rotation of the black hole
    vec4 envMapCoords = vec4(sphericalToCartesian(envMapSphericalCoords.x, envMapSphericalCoords.y), 0.0f);
    float rotationAngle = PI/2;
    float cameraRotationAngle = cameraToAnchorNodeDistance;

    mat4 rotationMatrixX = mat4(
        1.0f,    0.0f,                 0.0f,               0.0f,
        0.0f,    cos(rotationAngle),  -sin(rotationAngle), 0.0f,
        0.0f,    sin(rotationAngle),   cos(rotationAngle), 0.0f,
        0.0f,    0.0f,                 0.0f,               1.0f
    );

    mat4 rotationMatrixZ = mat4(
        cos(cameraRotationAngle), -sin(cameraRotationAngle), 0.0f, 0.0f,
        sin(cameraRotationAngle),  cos(cameraRotationAngle), 0.0f, 0.0f,
        0.0f,               0.0f,               1.0f, 0.0f,
        0.0f,               0.0f,               0.0f, 1.0f
    );
    
    envMapCoords = rotationMatrixZ * rotationMatrixX * envMapCoords;
    sphericalCoords = cartesianToSpherical(envMapCoords.xyz);

    vec2 uv = sphericalToUV(sphericalCoords);
    vec4 texColor = texture(environmentTexture, uv);
    
    frag.color = texColor;
    return frag;
}
