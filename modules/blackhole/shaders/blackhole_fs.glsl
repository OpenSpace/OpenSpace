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

layout (std430) buffer ssbo_star_map {
    float starKDTree[];
};

const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;
const float INF = 1.0f/0.0f;
const float LUM_LOWER_CAP = 0.01;


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
    float v = mod(sphereCoords.x, PI) / PI;
    
    return vec2(u, v);
}

/**********************************************************
                        Warp Table
***********************************************************/
ivec2 bstWarpTable(float phi){
    float midPhi = -1.0f;
    float deltaPhi = -1.0f;

    float minDeltaPhi = INF;
    int closestIndex = -1;

    int left = 0;
    int mid = -1;
    const int tableSize = schwarzschildWarpTable.length() / 2;
    int right = tableSize - 1;

    while(left <= right){
        mid = (left + right) / 2;
        midPhi = schwarzschildWarpTable[mid * 2];

        deltaPhi = abs(midPhi - phi);
        
        if(deltaPhi < minDeltaPhi){
            minDeltaPhi = deltaPhi;
            closestIndex = mid;
        }

        if (phi > midPhi) {
            right = mid - 1;
        } else {
            left = mid + 1;
        }
    }
    
    int leftIndex = closestIndex - 1;
    int rightIndex = closestIndex + 1;
    float leftDist = INF; 
    float rightDist = INF;

    if (rightIndex < tableSize - 1){
        rightDist = abs(schwarzschildWarpTable[rightIndex * 2] - phi);
    }
    if (leftIndex > 0){
        leftDist = abs(schwarzschildWarpTable[leftIndex * 2] - phi);
    }

    int nextClosestIndex = (rightDist < leftDist) ? rightIndex : leftIndex;

    float v1 = schwarzschildWarpTable[closestIndex * 2 + 1];
    float v2 = schwarzschildWarpTable[nextClosestIndex * 2 + 1];

    return v1 < v2 ? ivec2(closestIndex, nextClosestIndex) : ivec2(nextClosestIndex, closestIndex);
}

float interpelateWarpTable(int indexStart, int indexEnd, float localPhi){
    float envMapPhiStart = schwarzschildWarpTable[indexStart * 2 + 1];
    float envMapPhiEnd = schwarzschildWarpTable[indexEnd * 2 + 1];

    float localPhiStart = schwarzschildWarpTable[indexStart * 2];
    float localPhiEnd = schwarzschildWarpTable[indexEnd * 2];

    float t = (localPhi - localPhiStart) / (localPhiEnd - localPhiStart);
    t = clamp(t, 0.0, 1.0);
    
    return lerp(envMapPhiStart, envMapPhiEnd, t);
}

float getEndAngleFromTable(float phi){
    ivec2 indices = bstWarpTable(phi);
    return interpelateWarpTable(indices.x, indices.y, phi);
}

vec2 applyBlackHoleWarp(vec2 cameraOutSphereCoords){
    float theta = cameraOutSphereCoords.x;
    float phi = cameraOutSphereCoords.y;
    theta = getEndAngleFromTable(theta);
    return vec2(theta, phi);
}

/**********************************************************
                         Star Map
***********************************************************/

vec3 BVIndex2rgb(float color) {
  // BV is [-0.4, 2.0]
  float st = (color + 0.4) / (2.0 + 0.4);

  return texture(colorBVMap, st).rgb;
}

float angularDist(vec2 a, vec2 b) {
    float dTheta = a.x - b.x;
    float dPhi = a.y - b.y;
    return sqrt(dTheta * dTheta + sin(a.x) * sin(b.x) * dPhi * dPhi);
}

vec4 searchNearestStar(vec3 sphericalCoords) {
    const int NODE_SIZE = 6;
    const int SIZE = starKDTree.length() / NODE_SIZE;
    int index = 0;
    int nodeIndex = 0;
    int depth = 0;
    int axis = -1;

    while(index < SIZE && starKDTree[nodeIndex] > 0.0f){
        if (angularDist(sphericalCoords.yz, vec2(starKDTree[nodeIndex + 1], starKDTree[nodeIndex + 2])) < 0.002f){
                float luminosity = pow(10.0, 1.89 - 0.4 * starKDTree[nodeIndex + 4]);

                // If luminosity is really really small then set it to a static low number.
                if (luminosity < LUM_LOWER_CAP) {
                    luminosity = LUM_LOWER_CAP;
                }
                return vec4(BVIndex2rgb(starKDTree[nodeIndex + 3]), 1.0f);
        }

        axis = depth % 2 + 1;
        if(sphericalCoords[axis] < starKDTree[nodeIndex + axis]){
            index = 2 * index + 1;
        } else {
            index = 2 * index + 2;
        }
        nodeIndex = index * NODE_SIZE;
        depth += 1;
    }
    return vec4(0.0f);
}


/**********************************************************
                        Fragment shader
***********************************************************/

Fragment getFragment() {
    Fragment frag;

    vec4 viewCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));

    // User loacal input rotation of the black hole
    vec4 rotatedViewCoords = cameraRotationMatrix * viewCoords;
    //vec4 rotatedViewCoords = viewCoords;
    
    vec2 sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
    
    vec2 envMapSphericalCoords;
    #if SHOW_BLACK_HOLE == 1
    // Apply black hole warping to spherical coordinates
    envMapSphericalCoords = applyBlackHoleWarp(sphericalCoords);
    if (isnan(envMapSphericalCoords.x)) {
        // If inside the event horizon
        frag.color = vec4(0.0f);
        return frag;
    }
    #else
    envMapSphericalCoords = sphericalCoords;
    #endif

    // Init rotation of the black hole
    vec4 envMapCoords = vec4(sphericalToCartesian(envMapSphericalCoords.x, envMapSphericalCoords.y), 0.0f);
    
    float initRotationAngle = PI/2;
    mat4 rotationMatrixX = mat4(
        1.0f,    0.0f,                 0.0f,               0.0f,
        0.0f,    cos(initRotationAngle),  -sin(initRotationAngle), 0.0f,
        0.0f,    sin(initRotationAngle),   cos(initRotationAngle), 0.0f,
        0.0f,    0.0f,                 0.0f,               1.0f
    );


    // User world input rotation of the black hole
    envMapCoords = worldRotationMatrix * envMapCoords;
    envMapCoords = rotationMatrixX * envMapCoords;

    sphericalCoords = cartesianToSpherical(envMapCoords.xyz);
    vec2 uv = sphericalToUV(sphericalCoords);
    vec4 texColor = texture(environmentTexture, uv);
    
    texColor = clamp(texColor + searchNearestStar(vec3(0.0f, sphericalCoords.x, sphericalCoords.y)), 0.f, 1.f);
    
    frag.color = texColor;
    return frag;
}
