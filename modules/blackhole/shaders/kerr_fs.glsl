#include "fragment.glsl"
#include <${MODULE_BLACKHOLE}/shaders/stars.glsl>

in vec2 TexCoord;

#define SHOW_BLACK_HOLE 1

#define HORIZION 0.0f/0.0f
#define DISK -1337

uniform sampler2D environmentTexture;
uniform sampler2D viewGrid;
uniform sampler1D accretionDisk;

uniform mat4 cameraRotationMatrix;
uniform mat4 worldRotationMatrix;

layout (std430) buffer ssbo_warp_table {
  float schwarzschildWarpTable[];
};

#ifndef INF
#define INF 1.0 / 0.0
#endif

const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;

int layerCount, num_rays_per_dim;
const float max_theta = PI;
const float max_phi = 2.0 * PI;

/**********************************************************
                        Math
***********************************************************/

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
    // if (phi < 0.0) {
    //     phi += PI;
    // }

    return vec2(theta, phi);
}

vec3 sphericalToCartesian(float theta, float phi){
    float x = sin(theta)*cos(phi);
    float y = sin(theta)*sin(phi);
    float z = cos(theta);

    return vec3(x, y, z);
}

vec2 sphericalToUV(vec2 sphereCoords){
    float u = (sphereCoords.y + PI) / (2.0f * PI); // phi ∈ [-π, π] → u ∈ [0, 1]
    float v = sphereCoords.x / PI;          // theta ∈ [0, π] → v ∈ [0, 1]

    return vec2(u, v);
}

float mixAngle(float a, float b, float t) {
    float diff = b - a;
    // Wrap it into [-PI, +PI)
    diff -= floor((diff + PI) / (2.0 * PI)) * (2.0 * PI);
    return a + t * diff;
}

/**********************************************************
                        Warp Table
***********************************************************/

vec2 getInterpolatedWarpAngles(float input_theta, float input_phi, int layer) {
    float phi = input_phi; // already in [0, 2PI)
    float theta = input_theta;

    float theta_f = clamp(theta / max_theta * float(num_rays_per_dim - 1),
                          0.0, float(num_rays_per_dim - 1));
    float phi_f   = clamp((phi + PI) / max_phi * float(num_rays_per_dim - 1),
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

    // detect which corners are on the disk
    bool d00 = (v00.x == DISK);
    bool d10 = (v10.x == DISK);
    bool d01 = (v01.x == DISK);
    bool d11 = (v11.x == DISK);

    // only mix low/high if *both* edges had at least one on-disk sample
    bool onDisk = d00 || d10 || d01 || d11;

    if (onDisk) {
        // interpolat only if both points are on disk,
        // otherwise pick the one that is on-disk (or 0.0 if neither)
        float low = (d00 && d10)
            ? mix(v00.y, v10.y, t)
            : (d00 ? v00.y : (d10 ? v10.y : 0.0));

        float high = (d01 && d11)
            ? mix(v01.y, v11.y, t)
            : (d01 ? v01.y : (d11 ? v11.y : 0.0));

        return vec2(DISK, mix(low, high, u));
    }

    bool beyoundTheHorizion = isnan(v00.x) || isnan(v10.x) || isnan(v01.x) || isnan(v11.x);
    if (beyoundTheHorizion) {
      return vec2(HORIZION);
    }

    float lowTheta  = mix(v00.x, v10.x, t);
    float highTheta = mix(v01.x, v11.x, t);
    float outTheta = mix(lowTheta, highTheta, u);

    float lowPhi  = mixAngle(v00.y, v10.y, t);
    float highPhi = mixAngle(v01.y, v11.y, t);
    float outPhi = mixAngle(lowPhi, highPhi, u);
    return vec2(outTheta, outPhi);
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

    layerCount = starMapKDTreesIndices.length();
    num_rays_per_dim = int(sqrt(schwarzschildWarpTable.length() / ((layerCount + 1) * 2)));

    vec4 viewCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));

    vec4 rotatedViewCoords = cameraRotationMatrix * viewCoords;

    vec2 sphericalCoords;
    vec4 accumulatedColor = vec4(0.0f);
    float accumulatedWeight = 0.0f;

    for (int l = 0; l < layerCount; ++l) {
        sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
        sphericalCoords = applyBlackHoleWarp(sphericalCoords, l);

        if (sphericalCoords == vec2(HORIZION)) {
            frag.color = vec4(0.0f);
            return frag;
        } else if(sphericalCoords.x == DISK){
            frag.color = vec4(texture(accretionDisk, sphericalCoords.y).rgb, 1.0f);
            return frag;
        }
        vec4 starColor = searchNearestStar(vec3(0.0f, sphericalCoords.x, sphericalCoords.y), l);

        if (starColor.a > 0.0) {
            // Blend using weighted alpha blending
            accumulatedColor.rgb = (accumulatedColor.rgb * accumulatedWeight + starColor.rgb * starColor.a) / (accumulatedWeight + starColor.a);
            accumulatedWeight += starColor.a;
        }
    }

    vec2 uv = sphericalToUV(sphericalCoords);
    vec4 texColor = texture(environmentTexture, uv);

    frag.color.rgb = accumulatedColor.rgb * accumulatedWeight + texColor.rgb * (1.0 - accumulatedWeight);
    frag.color.a = 1.0;
    return frag;
}
