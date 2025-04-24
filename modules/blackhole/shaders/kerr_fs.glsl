#include "fragment.glsl"

in vec2 TexCoord;

#define SHOW_BLACK_HOLE 1

uniform sampler2D environmentTexture;
uniform sampler2D viewGrid;
uniform sampler1D colorBVMap;
uniform mat4 cameraRotationMatrix;
uniform mat4 worldRotationMatrix;

layout (std430) buffer ssbo_warp_table {
  float schwarzschildWarpTable[];
};

const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;
const float INF = 1.0f/0.0f;
const float LUM_LOWER_CAP = 0.01;

const int NODE_SIZE = 6;
const int layerCount = 1;
const float starRadius = 0.002f;
const int STACKSIZE = 32;
const int tableSize = schwarzschildWarpTable.length() / 2;
const int num_rays = schwarzschildWarpTable.length() / (layerCount + 1);

const float max_theta = PI;
const float max_phi = 2.0 * PI;
const int num_rays_per_dim = 250;

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
    float theta = atan2(sqrt(cartesian.x * cartesian.x + cartesian.y * cartesian.y), cartesian.z);
    float phi = atan2(cartesian.y, cartesian.x);

    // Remap phi from [-PI, +PI] to [0, 2*PI)
    if (phi < 0.0) {
        phi += 2.0 * PI;
    }

    return vec2(theta, phi);
}

vec3 sphericalToCartesian(float theta, float phi){
    float x = sin(theta)*cos(phi);
    float y = sin(theta)*sin(phi);
    float z = cos(theta);

    return vec3(x, y, z);
}

vec2 sphericalToUV(vec2 sphereCoords){
    float u = sphereCoords.y / (2.0f * PI); // phi ∈ [0, 2π] → u ∈ [0, 1]
    float v = sphereCoords.x / PI;          // theta ∈ [0, π] → v ∈ [0, 1]

    return vec2(u, v);
}

/**********************************************************
                        Warp Table
***********************************************************/

vec2 getInterpolatedWarpAngles(float input_theta, float input_phi, int layer) {
    float phi = input_phi; // already in [0, 2PI)
    float theta = input_theta;

    float theta_f = clamp(theta / max_theta * float(num_rays_per_dim - 1),
                          0.0, float(num_rays_per_dim - 1));
    float phi_f   = clamp(phi   / max_phi * float(num_rays_per_dim - 1),
                          0.0, float(num_rays_per_dim - 1));

    int theta0 = int(floor(theta_f));
    int phi0   = int(floor(phi_f));
    int theta1 = min(theta0 + 1, num_rays_per_dim - 1);
    int phi1   = (phi0 + 1) % num_rays_per_dim;

    float t = fract(theta_f);
    float u = fract(phi_f);

    int N      = num_rays_per_dim;
    int stride = 2 + layerCount * 2;
    int off    = 2 + layer * 2;

    #define FETCH(t_idx, p_idx) \
      vec2( \
        schwarzschildWarpTable[((t_idx)*N + (p_idx))*stride + off], \
        schwarzschildWarpTable[((t_idx)*N + (p_idx))*stride + off + 1] \
      )

    vec2 v00 = FETCH(theta0, phi0);
    vec2 v10 = FETCH(theta1, phi0);
    vec2 v01 = FETCH(theta0, phi1);
    vec2 v11 = FETCH(theta1, phi1);

    bool beyoundTheHorizion = isnan(v00.x) || isnan(v10.x) || isnan(v01.x) || isnan(v11.x);
    if (beyoundTheHorizion) {
      return vec2(0/0);
    }

    vec2 low  = mix(v00, v10, t);
    vec2 high = mix(v01, v11, t);
    return mix(low, high, u);
}
#undef FETCH

vec2 applyBlackHoleWarp(vec2 cameraOutAngles, int layer){
    float theta = cameraOutAngles.x;
    float phi = cameraOutAngles.y;
    return getInterpolatedWarpAngles(theta, phi, layer);
}

/**********************************************************
                         Fragment shader
***********************************************************/

Fragment getFragment() {
    Fragment frag;

    vec4 viewCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));
    vec4 rotatedViewCoords = cameraRotationMatrix * viewCoords;

    vec2 sphericalCoords;
    vec4 accumulatedColor = vec4(0.0f);
    float accumulatedWeight = 0.0f;

    for (int l = 0; l < layerCount; ++l) {
        sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
        sphericalCoords = applyBlackHoleWarp(sphericalCoords, l);

        if (sphericalCoords == vec2(0.0f/0.0f)) {
            frag.color = vec4(0.0f);
            return frag;
        }
    }

    vec2 uv = sphericalToUV(sphericalCoords);
    vec4 texColor = texture(environmentTexture, uv);

    frag.color.rgb = texColor.rgb;
    frag.color.a = 1.0;
    return frag;
}
