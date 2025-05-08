#ifndef STARS_GLSL
#define STARS_GLSL

#ifndef INF
#define INF 1.0 / 0.0
#endif

const float LUM_LOWER_CAP = 0.01;
const float starRadius = 0.0012f;
const int NODE_SIZE = 6;
const int STACKSIZE = 32;

// External uniforms/buffers expected by this file:
uniform sampler1D colorBVMap;
layout (std430) buffer ssbo_star_map { float starMapKDTrees[]; };
layout (std430) buffer ssbo_star_map_start_indices { int starMapKDTreesIndices[]; };

// Helpers
float angularDist(vec2 a, vec2 b) {
    float dTheta = a.x - b.x;
    float dPhi = a.y - b.y;
    return sqrt(dTheta * dTheta + sin(a.x) * sin(b.x) * dPhi * dPhi);
}

vec3 BVIndex2rgb(float color) {
    float st = (color + 0.4) / 2.4;
    return texture(colorBVMap, st).rgb;
}

// Star tree search
vec4 searchTree(int treeStart, int treeNodeCount, vec3 sphericalCoords) {
    struct TreeIndex { int index; int depth; };
    
    float bestDist = INF;
    int bestLocalIndex = -1;

    TreeIndex stack[STACKSIZE];
    int stackIndex = 0;
    TreeIndex cur = TreeIndex(0, 0);

    while (true) {
        if (cur.index < 0 || cur.index >= treeNodeCount) {
            if (stackIndex > 0) {
                cur = stack[--stackIndex];
                continue;
            } else break;
        }

        int absIndex = treeStart + cur.index;
        int base = absIndex * NODE_SIZE;

        float d = angularDist(sphericalCoords.yz, vec2(starMapKDTrees[base + 1], starMapKDTrees[base + 2]));
        if (d < bestDist) {
            bestDist = d;
            bestLocalIndex = cur.index;
        }

        int axis = cur.depth % 2 + 1;
        float diff = sphericalCoords[axis] - starMapKDTrees[base + axis];

        int offset = int(diff >= 0.0);
        int closer = 2 * cur.index + 1 + offset;
        int farther = 2 * cur.index + 2 - offset;

        if (abs(diff) < bestDist && farther < treeNodeCount) {
            stack[stackIndex++] = TreeIndex(farther, cur.depth + 1);
        }

        cur = TreeIndex(closer, cur.depth + 1);
    }

    if (bestLocalIndex == -1 || bestDist >= starRadius) return vec4(0.0);

    int bestAbsIndex = treeStart + bestLocalIndex;
    int base = bestAbsIndex * NODE_SIZE;

    float observedDistance = starMapKDTrees[base];
    float luminosity = pow(10.0, 1.89 - 0.4 * starMapKDTrees[base + 4]) / (0.9 * observedDistance);
    luminosity = max(luminosity, LUM_LOWER_CAP);

    float alpha = (1.0 - pow(bestDist / starRadius, 2.0)) * luminosity;
    vec3 starColor = BVIndex2rgb(starMapKDTrees[base + 3]);

    return vec4(starColor * alpha * luminosity, alpha);
}

vec4 searchNearestStar(vec3 sphericalCoords, int layer) {
    int treeStart = starMapKDTreesIndices[layer] / NODE_SIZE;
    int treeEnd = (layer + 1 < starMapKDTreesIndices.length())
        ? starMapKDTreesIndices[layer + 1] / NODE_SIZE
        : starMapKDTrees.length() / NODE_SIZE;

    return searchTree(treeStart, treeEnd - treeStart, sphericalCoords);
}

#endif // STARS_GLSL
