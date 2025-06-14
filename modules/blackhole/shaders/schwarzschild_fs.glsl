#include "fragment.glsl"
#include <${MODULE_BLACKHOLE}/shaders/stars.glsl>

in vec2 TexCoord;

uniform sampler2D environmentTexture;
uniform sampler2D viewGrid;

uniform mat4 cameraRotationMatrix;
uniform mat4 worldRotationMatrix;

uniform float r_0;

layout (std430) buffer ssbo_warp_table {
  float schwarzschildWarpTable[];
};

#ifndef INF
#define INF 1.0 / 0.0
#endif

const float shadow_delta = 0.01f;
const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;

const mat4 alignToWorldAxisRotation = mat4(
    1.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, -1.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 1.0
);

int layerCount, tableSize, num_rays;

float _shadow_angle;

/**********************************************************
                        Math
***********************************************************/

float lerp(float P0, float P1, float t) {
    return P0 + t * (P1 - P0);
}

float atan2(float a, float b){
    if (b != 0.0f) return atan(a, b);
    if (a > 0.0f) return PI / 2.0f;
    if (a < 0.0f) return -PI / 2.0f;
        
    return 0.0f;
}

/**********************************************************
                        Conversions
***********************************************************/

vec2 cartesianToSpherical(vec3 cartesian) {
    float theta = atan2(sqrt(cartesian.x * cartesian.x + cartesian.y * cartesian.y) , cartesian.z);
    float phi = atan2(cartesian.y, cartesian.x);

    return vec2(theta, phi);
}

vec3 sphericalToCartesian(float theta, float phi){
    float x = sin(theta)*cos(phi);
    float y = sin(theta)*sin(phi);
    float z = cos(theta);

    return vec3(x, y, z);
}

vec2 sphericalToUV(vec2 sphereCoords){
    float u = sphereCoords.y / (2.0f * PI) + 0.5f;
    float v = sphereCoords.x / PI;
    
    return vec2(u, v);
}


/**********************************************************
                        Warp Table
***********************************************************/

float ShadowAngle() {
    // Critical impact parameter for a Schwarzschild black hole in geometric units.
    float b_crit = 3.0 * sqrt(3.0) / 2.0;  // ~2.598

    // Compute the sine of the shadow angle:
    float ratio = b_crit * sqrt(1.0 - 1.0 / r_0) / r_0;

    // Clamp to [0,1] to avoid domain errors
    ratio = clamp(ratio, 0.0, 1.0);

    return asin(ratio) - shadow_delta;
}

ivec2 getClosestWarpIndices(float phi) {
    //Delta needs to be the same as the Cuda implemtation that calculates the Schwarszchild table
    float denom = shadow_delta - (PI - _shadow_angle);
    float s = (phi - (PI - _shadow_angle)) / denom;
    s = clamp(s, 0.0, 1.0);

    float idx_f = s * float(num_rays - 1);
    int idx0 = int(floor(idx_f)) * (layerCount + 1);
    int idx1 = int(min(float(num_rays - 1), ceil(idx_f))) * (layerCount + 1);

    return ivec2(idx0, idx1);
}

float interpelateWarpTable(int indexStart, int indexEnd, float localPhi, int layer){
    float envMapPhiStart = schwarzschildWarpTable[indexStart + layer];
    float envMapPhiEnd = schwarzschildWarpTable[indexEnd + layer];

    float localPhiStart = schwarzschildWarpTable[indexStart];
    float localPhiEnd = schwarzschildWarpTable[indexEnd];

    float t = (localPhi - localPhiStart) / (localPhiEnd - localPhiStart);
    t = clamp(t, 0.0, 1.0);
    
    return lerp(envMapPhiStart, envMapPhiEnd, t);
}

float getEndAngleFromTable(float phi, int layer){
    ivec2 indices = getClosestWarpIndices(phi);
    return interpelateWarpTable(indices.x, indices.y, phi, layer);
}

vec2 applyBlackHoleWarp(vec2 cameraOutSphereCoords, int layer){
    float theta = cameraOutSphereCoords.x;
    float phi = cameraOutSphereCoords.y;
    theta = getEndAngleFromTable(theta, layer + 1);
    return vec2(theta, phi);
}

/**********************************************************
                        Fragment shader
***********************************************************/

Fragment getFragment() {
    Fragment frag;
    _shadow_angle = ShadowAngle();
    layerCount = starMapKDTreesIndices.length();
    tableSize = schwarzschildWarpTable.length() / 2;
    num_rays = schwarzschildWarpTable.length() / (layerCount + 1);

    vec4 viewCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));

    // User local input rotation of the black hole
    vec4 rotatedViewCoords = cameraRotationMatrix * viewCoords;
    


    vec2 sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
    if(sphericalCoords.x < _shadow_angle){
            frag.color = vec4(0.0f);
            return frag;
    }

    vec2 envMapSphericalCoords;
    
    vec4 accumulatedColor = vec4(0.0f);
    float accumulatedWeight = 0.0f;  // Track total weight of blending

    // Apply black hole warping to spherical coordinates
    for (int l = 0; l < layerCount; ++l) {
        sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
        envMapSphericalCoords = applyBlackHoleWarp(sphericalCoords, l);

        vec4 envMapCoords = vec4(sphericalToCartesian(envMapSphericalCoords.x, envMapSphericalCoords.y), 0.0f);

        // User world input rotation of the black hole
        envMapCoords = alignToWorldAxisRotation * worldRotationMatrix * envMapCoords;

        sphericalCoords = cartesianToSpherical(envMapCoords.xyz);
        vec4 starColor = searchNearestStar(vec3(0.0f, sphericalCoords.x, sphericalCoords.y), l);

        if (starColor.a > 0.0) {
            float layerWeight = 1;  // Earlier layers have more weight

            // Blend using weighted alpha blending
            accumulatedColor.rgb = (accumulatedColor.rgb * accumulatedWeight + starColor.rgb * starColor.a * layerWeight) / (accumulatedWeight + starColor.a * layerWeight);
            accumulatedWeight += starColor.a * layerWeight;
        }
    }

    vec2 uv = sphericalToUV(sphericalCoords);
    vec4 texColor = texture(environmentTexture, uv);

    frag.color.rgb = accumulatedColor.rgb * accumulatedWeight + texColor.rgb * (1.0 - accumulatedWeight);
    frag.color.a = 1.0;
    return frag;
}
