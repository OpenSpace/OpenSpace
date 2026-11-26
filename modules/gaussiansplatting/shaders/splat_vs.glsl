#version __CONTEXT__

struct GaussianSplat {
    vec4 position;    // xyz local coords
    vec4 cov3d_col0;  // (xx, xy, xz, yy)
    vec4 cov3d_col1;  // (yz, zz, opacity, pad)
    vec4 color_dc;    // rgb
};

layout(std430, binding = 0) readonly buffer SplatBuffer {
    GaussianSplat splats[];
};

layout(std430, binding = 1) readonly buffer IndexBuffer {
    uint sortedIndices[];
};

// OpenSpace passes camera-relative double-precision transforms converted to float on CPU
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;
uniform vec2 viewportSize;
uniform float focalX;
uniform float focalY;
uniform float scaleMultiplier;
uniform float globalOpacity;

out vec2 vFragPos;      // Local quad coordinates [-3, 3] sigma
out vec4 vColor;        // Base RGB + combined alpha
out vec3 vConic;        // Inverse 2D covariance (a, b, c) for Gaussian evaluation
out vec4 vViewPos;      // View-space position for OpenSpace G-buffer

// Quad corners for glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, splatCount)
const vec2 quadVertices[4] = vec2[4](
    vec2(-1.0, -1.0),
    vec2( 1.0, -1.0),
    vec2(-1.0,  1.0),
    vec2( 1.0,  1.0)
);

void main() {
    uint splatIdx = sortedIndices[gl_InstanceID];
    GaussianSplat s = splats[splatIdx];

    // 1. Transform center to camera-relative eye space
    vec4 viewPos = modelViewTransform * vec4(s.position.xyz, 1.0);

    // Near-plane / behind-camera culling (OpenGL camera looks down -Z)
    if (viewPos.z >= -0.05) {
        gl_Position = vec4(0.0, 0.0, 2.0, 1.0); // Clip outside NDC
        return;
    }

    // 2. Reconstruct symmetric 3D covariance matrix (scaled)
    float scaleSq = scaleMultiplier * scaleMultiplier;
    mat3 Vrk = mat3(
        s.cov3d_col0.x, s.cov3d_col0.y, s.cov3d_col0.z,
        s.cov3d_col0.y, s.cov3d_col0.w, s.cov3d_col1.x,
        s.cov3d_col0.z, s.cov3d_col1.x, s.cov3d_col1.y
    ) * scaleSq;

    // 3. EWA Splatting Jacobian (projecting 3D covariance to 2D ray space)
    float invZ = 1.0 / viewPos.z;
    float invZ2 = invZ * invZ;
    mat3 J = mat3(
        focalX * invZ, 0.0, -(focalX * viewPos.x) * invZ2,
        0.0, focalY * invZ, -(focalY * viewPos.y) * invZ2,
        0.0, 0.0, 0.0
    );

    mat3 W = mat3(modelViewTransform);
    mat3 T = transpose(J) * W;
    mat3 cov2d_mat = T * Vrk * transpose(T);

    // Add low-pass filter (0.3 px^2) to prevent sub-pixel aliasing
    float a = cov2d_mat[0][0] + 0.3;
    float b = cov2d_mat[0][1];
    float c = cov2d_mat[1][1] + 0.3;

    // 4. Compute inverse 2D covariance (conic) and eigenvalues for quad sizing
    float det = a * c - b * b;
    if (det <= 0.0) {
        gl_Position = vec4(0.0, 0.0, 2.0, 1.0);
        return;
    }
    float invDet = 1.0 / det;
    vConic = vec3(c * invDet, -b * invDet, a * invDet);

    float mid = 0.5 * (a + c);
    float disc = max(0.1, mid * mid - det);
    float lambda1 = mid + sqrt(disc);
    float lambda2 = mid - sqrt(disc);
    float radiusPx = ceil(3.0 * sqrt(max(lambda1, lambda2)));

    // 5. Offset quad vertex in NDC space
    vec4 clipCenter = projectionTransform * viewPos;
    vec2 ndcCenter = clipCenter.xy / clipCenter.w;

    vec2 corner = quadVertices[gl_VertexID];
    vec2 ndcOffset = (corner * radiusPx * 2.0) / viewportSize;

    gl_Position = vec4(ndcCenter + ndcOffset, clipCenter.z / clipCenter.w, 1.0);
    vFragPos = corner * radiusPx;
    vColor = vec4(s.color_dc.rgb, s.cov3d_col1.z * globalOpacity);
    vViewPos = viewPos;
}