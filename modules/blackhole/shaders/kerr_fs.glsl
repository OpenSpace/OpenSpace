#include "fragment.glsl"

in vec2 TexCoord;

#define SHOW_BLACK_HOLE 1

uniform sampler2D environmentTexture;
uniform sampler2D viewGrid;
uniform sampler1D colorBVMap;
uniform mat4 cameraRotationMatrix;;;
uniform mat4 worldRotationMatrix;

layout (std430) buffer ssbo_warp_table {
  float schwarzschildWarpTable[];
};

// layout (std430) buffer ssbo_star_map {
//     float starMapKDTrees[];
// };

// layout (std430) buffer ssbo_star_map_start_indices  {
//     int starMapKDTreesIndices[];
// };

const float PI = 3.1415926535897932384626433832795f;
const float VIEWGRIDZ = -1.0f;
const float INF = 1.0f/0.0f;
const float LUM_LOWER_CAP = 0.01;

const int NODE_SIZE = 6;
// const int StarArrySize = starMapKDTrees.length() / NODE_SIZE;
// const int layerCount = starMapKDTreesIndices.length();
const int layerCount = 1;
// const int tableNodeSize = starMapKDTreesIndices.length() + 1;
const float starRadius = 0.002f;
const int STACKSIZE = 32;
const int tableSize = schwarzschildWarpTable.length() / 2;
const int num_rays = schwarzschildWarpTable.length() / (layerCount + 1);


const float max_theta = PI;
const float max_phi = 2*PI;
const int num_rays_per_dim = 100;
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

vec2 getInterpolatedWarpAngles(float input_theta, float input_phi, int layer) {
    // 1) Convert to float grid indices
    float theta_f = (input_theta)
                  * float(num_rays_per_dim - 1)
                  / (max_theta);
    float phi_f   =  input_phi
                  * float(num_rays_per_dim -1)
                  / (2.0 * PI);

    // 2) Integer bounds
    int theta_low  = int(clamp(floor(theta_f), 0.0, float(num_rays_per_dim-1)));
    int theta_high = int(clamp(theta_low + 1, 0, num_rays_per_dim-1));

    int phi_low  = int(floor(phi_f)) % num_rays_per_dim;
    int phi_high = (phi_low + 1) % num_rays_per_dim;

    // 3) Flat indices
    int N = num_rays_per_dim;
    int idx00 = theta_low  * N + phi_low;
    int idx10 = theta_high * N + phi_low;
    int idx01 = theta_low  * N + phi_high;
    int idx11 = theta_high * N + phi_high;

    // 4) Record stride & per-layer offset
    int record_stride = 2 + layerCount * 2;
    int offset        = 2 + layer * 2;

    // 5) Base pointers into the table
    int base00 = idx00 * record_stride;
    int base10 = idx10 * record_stride;
    int base01 = idx01 * record_stride;
    int base11 = idx11 * record_stride;

    // 6) Extract the original theta/phi stored in the table
    float theta00 = schwarzschildWarpTable[base00    ];
    float phi00   = schwarzschildWarpTable[base00 + 1];
    float theta10 = schwarzschildWarpTable[base10    ];
    float phi01   = schwarzschildWarpTable[base01 + 1];

    // 7) Interpolation weights
    float t = (input_theta - theta00) / (theta10 - theta00);
    float u = (input_phi   - phi00)   / (phi01   - phi00);

    // 8) Fetch the four target warp‐angle pairs
    vec2 v00 = vec2(
      schwarzschildWarpTable[base00 + offset    ],
      schwarzschildWarpTable[base00 + offset + 1]
    );
    vec2 v10 = vec2(
      schwarzschildWarpTable[base10 + offset    ],
      schwarzschildWarpTable[base10 + offset + 1]
    );
    vec2 v01 = vec2(
      schwarzschildWarpTable[base01 + offset    ],
      schwarzschildWarpTable[base01 + offset + 1]
    );
    vec2 v11 = vec2(
      schwarzschildWarpTable[base11 + offset    ],
      schwarzschildWarpTable[base11 + offset + 1]
    );
    return v00;
    // 9) Bilinear interpolate
    vec2 interp_low  = mix(v00, v10, t);
    vec2 interp_high = mix(v01, v11, t);
    return mix(interp_low, interp_high, u);
}


vec2 applyBlackHoleWarp(vec2 cameraOutAngles, int layer){
    float theta = cameraOutAngles.x;
    float phi = cameraOutAngles.y;
    return getInterpolatedWarpAngles(theta, phi, layer);
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
// vec4 searchTree(int treeStart, int treeNodeCount, vec3 sphericalCoords) {
//     // Local struct for tree traversal.
//     struct TreeIndex {
//         int index; // local index (starting at 0 for this tree)
//         int depth;
//     };

//     // Initialize candidate tracking for this tree.
//     float bestDist = INF;
//     int bestLocalIndex = -1;
    
//     TreeIndex stack[STACKSIZE];
//     int stackIndex = 0;
//     TreeIndex cur;
//     cur.index = 0; // root of the tree (local index 0)
//     cur.depth = 0;
    
//     // Perform iterative search on the single KD-tree.
//     while (true) {
//         // If current index is out-of-bounds, backtrack if possible.
//         if (cur.index < 0 || cur.index >= treeNodeCount) {
//             if (stackIndex > 0) {
//                 cur = stack[--stackIndex];
//                 continue;
//             } else {
//                 break;
//             }
//         }
        
//         // Convert local index to absolute index in the flat array.
//         int absIndex = treeStart + cur.index;
//         int base = absIndex * NODE_SIZE;
        
//         float d = angularDist(sphericalCoords.yz,
//                               vec2(starMapKDTrees[base + 1], starMapKDTrees[base + 2]));
//         if (d < bestDist) {
//             bestDist = d;
//             bestLocalIndex = cur.index;
//         }
        
//         // Determine axis (1 or 2) using depth (sphericalCoords: index 1=theta, index 2=phi)
//         int axis = cur.depth % 2 + 1;
//         float diff = sphericalCoords[axis] - starMapKDTrees[base + axis];
        
//         int offset = int(diff >= 0.0);
//         int closerLocal = 2 * cur.index + 1 + offset;
//         int fartherLocal = 2 * cur.index + 2 - offset;
        
//         int nextDepth = cur.depth + 1;
        
//         // If the farther branch might hold a closer point, push it onto the stack.
//         if (abs(diff) < bestDist && (fartherLocal < treeNodeCount)) {
//             stack[stackIndex].index = fartherLocal;
//             stack[stackIndex].depth = nextDepth;
//             stackIndex++;
//         }
        
//         // Continue down the closer branch.
//         cur.index = closerLocal;
//         cur.depth = nextDepth;
//     } // End of tree traversal loop

//     if (bestLocalIndex == -1 || bestDist >= starRadius) {
//         return vec4(0.0);
//     }
    
//     int bestAbsIndex = treeStart + bestLocalIndex;
//     int base = bestAbsIndex * NODE_SIZE;
    
//     float observedDistance = starMapKDTrees[base];
//     float luminosity = pow(10.0, 1.89 - 0.4 * starMapKDTrees[base + 4]);
//     luminosity /= pow(observedDistance, 1.1);
//     luminosity = max(luminosity, LUM_LOWER_CAP);
    
//     float alpha = 1.0 - pow(bestDist / starRadius, 2.2);
//     vec3 starColor = BVIndex2rgb(starMapKDTrees[base + 3]);
    
//     // Return the star contribution (with pre-multiplied alpha).
//     return vec4(starColor * alpha * luminosity, alpha * luminosity);
// }

// vec4 searchNearestStar(vec3 sphericalCoords, int layer) {
//     vec4 accumulatedColor = vec4(0.0);
//     float totalAlpha = 0.0;
    
//     int kdTreeCount = starMapKDTreesIndices.length();
//     int totalNodes = starMapKDTrees.length() / NODE_SIZE;
    
//     // Determine the start offset for the current tree
//     int treeStartFloat = starMapKDTreesIndices[layer];
//     int treeStart = treeStartFloat / NODE_SIZE;
    
//     // Determine the end of this tree.
//     int treeEnd;
//     if (layer < kdTreeCount - 1) {
//         int nextTreeStartFloat = int(starMapKDTreesIndices[layer+1]);
//         treeEnd = nextTreeStartFloat / NODE_SIZE;
//     } else {
//         treeEnd = totalNodes;
//     }
//     int treeNodeCount = treeEnd - treeStart;
    
//     // Search the current tree.
//     return searchTree(treeStart, treeNodeCount, sphericalCoords);
// }


/**********************************************************
                        Fragment shader
***********************************************************/

Fragment getFragment() {
    Fragment frag;

    vec4 viewCoords = normalize(vec4(texture(viewGrid, TexCoord).xy, VIEWGRIDZ, 0.0f));

    // User local input rotation of the black hole
    vec4 rotatedViewCoords = cameraRotationMatrix * viewCoords;
    
    vec2 sphericalCoords;
    
    vec4 accumulatedColor = vec4(0.0f);
    float accumulatedWeight = 0.0f;  // Track total weight of blending

    // Apply black hole warping to spherical coordinates
    for (int l = 0; l < layerCount; ++l) {
        sphericalCoords = cartesianToSpherical(rotatedViewCoords.xyz);
        sphericalCoords = applyBlackHoleWarp(sphericalCoords, l);
        if (isnan(sphericalCoords.x)) {
            // If inside the event horizon
            frag.color = vec4(.5f);
            return frag;
        }

        // vec4 envMapCoords = vec4(sphericalToCartesian(sphericalCoords.x, sphericalCoords.y), 0.0f);

        // User world input rotation of the black hole
        // envMapCoords = worldRotationMatrix * envMapCoords;

        // sphericalCoords = cartesianToSpherical(envMapCoords.xyz);
        // vec4 starColor = searchNearestStar(vec3(0.0f, sphericalCoords.x, sphericalCoords.y), l);

        // if (starColor.a > 0.0) {
        //     float layerWeight = exp(-0.5 * l);  // Earlier layers have more weight

        //     // Blend using weighted alpha blending
        //     accumulatedColor.rgb = (accumulatedColor.rgb * accumulatedWeight + starColor.rgb * starColor.a * layerWeight) / (accumulatedWeight + starColor.a * layerWeight);
        //     accumulatedWeight += starColor.a * layerWeight;
        // }
    }

    vec2 uv = sphericalToUV(sphericalCoords);
    vec4 texColor = texture(environmentTexture, uv);

    // frag.color.rgb = accumulatedColor.rgb * accumulatedWeight + texColor.rgb * (1.0 - accumulatedWeight);
    frag.color.rgb = texColor.rgb;
    frag.color.a = 1.0;
    return frag;
}
