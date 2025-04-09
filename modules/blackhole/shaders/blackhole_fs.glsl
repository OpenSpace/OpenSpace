//#include "fragment.glsl"

#version __CONTEXT__

in vec2 TexCoord;


#define SHOW_BLACK_HOLE 1

#define hash

uniform sampler2D environmentTexture;
uniform sampler2D viewGrid;
uniform sampler1D colorBVMap;
uniform mat4 cameraRotationMatrix;
uniform mat4 worldRotationMatrix;
uniform float r_0;

layout (location = 0) out vec4 finalColor;

layout (std430) buffer ssbo_warp_table {
  float schwarzschildWarpTable[];
};

layout (std430) buffer ssbo_star_map {
    float starMapKDTrees[];
};

layout (std430) buffer ssbo_star_map_start_indices  {
    int starMapKDTreesIndices[];
};

const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;
const float INF = 1.0f/0.0f;
const float LUM_LOWER_CAP = 0.01;

const int NODE_SIZE = 6;
const int StarArrySize = starMapKDTrees.length() / NODE_SIZE;
const int layerCount = starMapKDTreesIndices.length();
const int tableNodeSize = starMapKDTreesIndices.length() + 1;
const float starRadius = 0.002f;
const int STACKSIZE = 32;
const int tableSize = schwarzschildWarpTable.length() / 2;
const int num_rays = schwarzschildWarpTable.length() / (layerCount + 1);

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
#ifdef hash
float ShadowAngle() {
    // Critical impact parameter for a Schwarzschild black hole in geometric units.
    float b_crit = 3.0 * sqrt(3.0) / 2.0;  // ~2.598

    // Compute the sine of the shadow angle:
    float ratio = b_crit * sqrt(1.0 - 1.0 / r_0) / r_0;

    // Clamp to [0,1] to avoid domain errors
    ratio = clamp(ratio, 0.0, 1.0);

    return asin(ratio);
}

ivec2 getClosestWarpIndices(float phi) {
    //Delta needs to be the same as the Cuda implemtation that calculates the Schwarszchild table
    float r_0;
    float theta_shadow = ShadowAngle();
    float delta = 0.01f;
    float lower_bound = theta_shadow - delta;

    float denom = delta - (PI - lower_bound);
    float s = (phi - (PI - lower_bound)) / denom;
    s = clamp(s, 0.0, 1.0);

    float idx_f = s * float(num_rays - 1);
    int idx0 = int(floor(idx_f)) * (layerCount + 1);
    int idx1 = int(min(float(num_rays - 1), ceil(idx_f))) * (layerCount + 1);

    return ivec2(idx0, idx1);
    // float phi0 = schwarzschildWarpTable[idx0];
    // float phi1 = schwarzschildWarpTable[idx1];

    // // Determine which is closer
    // if (abs(phi - phi0) < abs(phi - phi1)) {
    //     return ivec2(idx0, idx1);
    // } else {
    //     return ivec2(idx1, idx0);
    // }
}
#else
ivec2 bstWarpTable(float phi, int layer){
    float midPhi = -1.0f;
    float deltaPhi = -1.0f;

    float minDeltaPhi = INF;
    int closestIndex = -1;

    int left = 0;
    int mid = -1;
    int right = tableSize - 1;

    while(left <= right){
        mid = (left + right) / 2;
        midPhi = schwarzschildWarpTable[mid * tableNodeSize];

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
        rightDist = abs(schwarzschildWarpTable[rightIndex * tableNodeSize] - phi);
    }
    if (leftIndex > 0){
        leftDist = abs(schwarzschildWarpTable[leftIndex * tableNodeSize] - phi);
    }

    int nextClosestIndex = (rightDist < leftDist) ? rightIndex : leftIndex;

    float v1 = schwarzschildWarpTable[closestIndex * tableNodeSize + layer];
    float v2 = schwarzschildWarpTable[nextClosestIndex * tableNodeSize + layer];

    return v1 < v2 ? ivec2(closestIndex * tableNodeSize, nextClosestIndex * tableNodeSize) : ivec2(nextClosestIndex * tableNodeSize, closestIndex * tableNodeSize);
}
#endif
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
    #ifdef hash
    ivec2 indices = getClosestWarpIndices(phi);
    #else
    ivec2 indices = bstWarpTable(phi, layer);
    #endif
    return interpelateWarpTable(indices.x, indices.y, phi, layer);
}

vec2 applyBlackHoleWarp(vec2 cameraOutSphereCoords, int layer){
    float theta = cameraOutSphereCoords.x;
    float phi = cameraOutSphereCoords.y;
    theta = getEndAngleFromTable(theta, layer + 1);
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

// Search a single KD-tree given its start and node count.
// Returns a vec4 where rgb is the accumulated star color and a is the total alpha.
vec4 searchTree(int treeStart, int treeNodeCount, vec3 sphericalCoords) {
    // Local struct for tree traversal.
    struct TreeIndex {
        int index; // local index (starting at 0 for this tree)
        int depth;
    };

    // Initialize candidate tracking for this tree.
    float bestDist = INF;
    int bestLocalIndex = -1;
    
    TreeIndex stack[STACKSIZE];
    int stackIndex = 0;
    TreeIndex cur;
    cur.index = 0; // root of the tree (local index 0)
    cur.depth = 0;
    
    // Perform iterative search on the single KD-tree.
    while (true) {
        // If current index is out-of-bounds, backtrack if possible.
        if (cur.index < 0 || cur.index >= treeNodeCount) {
            if (stackIndex > 0) {
                cur = stack[--stackIndex];
                continue;
            } else {
                break;
            }
        }
        
        // Convert local index to absolute index in the flat array.
        int absIndex = treeStart + cur.index;
        int base = absIndex * NODE_SIZE;
        
        float d = angularDist(sphericalCoords.yz,
                              vec2(starMapKDTrees[base + 1], starMapKDTrees[base + 2]));
        if (d < bestDist) {
            bestDist = d;
            bestLocalIndex = cur.index;
        }
        
        // Determine axis (1 or 2) using depth (sphericalCoords: index 1=theta, index 2=phi)
        int axis = cur.depth % 2 + 1;
        float diff = sphericalCoords[axis] - starMapKDTrees[base + axis];
        
        int offset = int(diff >= 0.0);
        int closerLocal = 2 * cur.index + 1 + offset;
        int fartherLocal = 2 * cur.index + 2 - offset;
        
        int nextDepth = cur.depth + 1;
        
        // If the farther branch might hold a closer point, push it onto the stack.
        if (abs(diff) < bestDist && (fartherLocal < treeNodeCount)) {
            stack[stackIndex].index = fartherLocal;
            stack[stackIndex].depth = nextDepth;
            stackIndex++;
        }
        
        // Continue down the closer branch.
        cur.index = closerLocal;
        cur.depth = nextDepth;
    } // End of tree traversal loop

    if (bestLocalIndex == -1 || bestDist >= starRadius) {
        return vec4(0.0);
    }
    
    int bestAbsIndex = treeStart + bestLocalIndex;
    int base = bestAbsIndex * NODE_SIZE;
    
    float observedDistance = starMapKDTrees[base];
    float luminosity = pow(10.0, 1.89 - 0.4 * starMapKDTrees[base + 4]);
    luminosity /= pow(observedDistance, 1.1);
    luminosity = max(luminosity, LUM_LOWER_CAP);
    
    float alpha = 1.0 - pow(bestDist / starRadius, 2.2);
    vec3 starColor = BVIndex2rgb(starMapKDTrees[base + 3]);
    
    // Return the star contribution (with pre-multiplied alpha).
    return vec4(starColor * alpha * luminosity, alpha * luminosity);
}

vec4 searchNearestStar(vec3 sphericalCoords, int layer) {
    vec4 accumulatedColor = vec4(0.0);
    float totalAlpha = 0.0;
    
    int kdTreeCount = starMapKDTreesIndices.length();
    int totalNodes = starMapKDTrees.length() / NODE_SIZE;
    
    // Determine the start offset for the current tree
    int treeStartFloat = starMapKDTreesIndices[layer];
    int treeStart = treeStartFloat / NODE_SIZE;
    
    // Determine the end of this tree.
    int treeEnd;
    if (layer < kdTreeCount - 1) {
        int nextTreeStartFloat = int(starMapKDTreesIndices[layer+1]);
        treeEnd = nextTreeStartFloat / NODE_SIZE;
    } else {
        treeEnd = totalNodes;
    }
    int treeNodeCount = treeEnd - treeStart;
    
    // Search the current tree.
    return searchTree(treeStart, treeNodeCount, sphericalCoords);
}


/**********************************************************
                        Fragment shader
***********************************************************/

void main() {

    vec4 viewCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));

    // User local input rotation of the black hole
    vec4 rotatedViewCoords = cameraRotationMatrix * viewCoords;
    
    vec2 sphericalCoords;
    vec2 envMapSphericalCoords;
    
    vec4 accumulatedColor = vec4(0.0f);
    float accumulatedWeight = 0.0f;  // Track total weight of blending

    // Apply black hole warping to spherical coordinates
    for (int l = 0; l < layerCount; ++l) {
        sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
        envMapSphericalCoords = applyBlackHoleWarp(sphericalCoords, l);

        if (isnan(envMapSphericalCoords.x)) {
            // If inside the event horizon
            finalColor = vec4(0.0f);
            return;
        }

        vec4 envMapCoords = vec4(sphericalToCartesian(envMapSphericalCoords.x, envMapSphericalCoords.y), 0.0f);

        // User world input rotation of the black hole
        envMapCoords = worldRotationMatrix * envMapCoords;

        sphericalCoords = cartesianToSpherical(envMapCoords.xyz);
        vec4 starColor = searchNearestStar(vec3(0.0f, sphericalCoords.x, sphericalCoords.y), l);

        if (starColor.a > 0.0) {
            float layerWeight = exp(-0.5 * l);  // Earlier layers have more weight

            // Blend using weighted alpha blending
            accumulatedColor.rgb = (accumulatedColor.rgb * accumulatedWeight + starColor.rgb * starColor.a * layerWeight) / (accumulatedWeight + starColor.a * layerWeight);
            accumulatedWeight += starColor.a * layerWeight;
        }
    }

    vec2 uv = sphericalToUV(sphericalCoords);
    vec4 texColor = texture(environmentTexture, uv);

    finalColor.rgb = accumulatedColor.rgb * accumulatedWeight + texColor.rgb * (1.0 - accumulatedWeight);
    finalColor.a = 1.0;
}
