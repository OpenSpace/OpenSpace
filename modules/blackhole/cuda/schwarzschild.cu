#include <cuda_runtime.h>
#include <vector>
#include <stdexcept>
#include "device_launch_parameters.h"
#include "schwarzschild.h"

#define HORIZION nanf("")

constexpr float PI = 3.1415926535897932384626433832795f;

__device__ float zamoconv(float r0, float psi) {
    // Compute the Schwarzschild lapse factor at r0:
    // For rs = 1, we have: N(r0) = sqrt(1 - 1/r0)
    float N_r0 = sqrtf(1.f - 1.f / r0);
    return -( N_r0 * cosf(psi) ) / ( r0 * sinf(psi) );
}

__device__ float compute_shadow_angle(float r_0) {
    // Critical impact parameter for a Schwarzschild black hole in geometric units.
    float b_crit = 3.0f * sqrtf(3.0f) / 2.0f;  // ~2.598
    // Compute the sine of the shadow angle:
    float ratio = b_crit * sqrtf(1.0f - 1.0f / r_0) / r_0;
    // Clamp to [0,1] to avoid domain errors
    ratio = fminf(fmaxf(ratio, 0.0f), 1.0f);
    return asinf(ratio);
}

__device__ void geodesic(float u, float dudphi, float& out_du_dphi, float& out_d2u_dphi2) {
    out_du_dphi = dudphi;
    out_d2u_dphi2 = -u * (1.f - 3.f / 2.f * u);
}

__device__ void rk4_step(float& u, float& dudphi, float& phi, float h) {
    float k1_u, k1_dudphi, k2_u, k2_dudphi, k3_u, k3_dudphi, k4_u, k4_dudphi;

    geodesic(u, dudphi, k1_u, k1_dudphi);
    geodesic(u + 0.5f * k1_u * h, dudphi + 0.5f * k1_dudphi * h, k2_u, k2_dudphi);
    geodesic(u + 0.5f * k2_u * h, dudphi + 0.5f * k2_dudphi * h, k3_u, k3_dudphi);
    geodesic(u + k3_u * h, dudphi + k3_dudphi * h, k4_u, k4_dudphi);

    phi += h;
    u = u + (k1_u + 2.f * k2_u + 2.f * k3_u + k4_u) * h / 6.f;
    dudphi = dudphi + (k1_dudphi + 2.f * k2_dudphi + 2.f * k3_dudphi + k4_dudphi) * h / 6.f;
}

__global__ void solveGeodesicKernel(float u_0, float h, float* envmap_r_values, size_t num_envmaps,
    size_t num_rays, size_t num_steps, float* angles_out) {
    size_t idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= num_rays) return;

    size_t const outNodeSize = num_envmaps + 1;

    float r_0 = 1.0f / u_0;

    float theta_shadow = compute_shadow_angle(r_0);  // edge of the blackhole shadow

    const float delta = 0.01f;

    // Set the lower bound slightly inside the shadow:
    float lower_bound = theta_shadow - delta;

    float s = idx / static_cast<float>(num_rays - 1);
    float alpha = (PI - lower_bound) * (1.0f - s) + delta * s;

    angles_out[idx * outNodeSize] = alpha;

    // Compute initial derivative for this ray using the selected alpha.
    float dudphi = zamoconv(r_0, alpha); //compute_initial_dudphi(r_0, alpha);

    float u = u_0;
    float phi = 0.0f;

    auto out_of_bounds = [&u, &envmap_r_values](unsigned int index) -> bool {
        return (1.0f > envmap_r_values[index] * u);
        };

    auto inside_singularity = [&u]() -> bool {
        return (1.0f < u);
        };

    float* entry = &angles_out[idx * outNodeSize];

    unsigned int idx_entry = 0;
    entry[idx_entry++] = alpha;
    for (size_t step = 0; step < num_steps; step++) {
        rk4_step(u, dudphi, phi, h);

        bool in_singularity = inside_singularity();
        bool is_out_of_bounds = out_of_bounds(idx_entry - 1);
        if (in_singularity | is_out_of_bounds) {
            if (in_singularity) {
                while (idx_entry < outNodeSize) {
                    entry[idx_entry++] = HORIZION;
                }
                break;
            }
            if (is_out_of_bounds) {
                entry[idx_entry++] = phi;
                if (idx_entry > num_envmaps) {
                    break;
                }
            }
        }
    }
}

void schwarzschild(std::vector<float> const& envmap_r_values, size_t const num_rays, size_t const num_steps, float const r_0, float const h, std::vector<float>& angle_out) {
    float* d_angle_values;
    float* d_envmap_r_values;

    float u_0 = 1.0f / r_0;
    size_t const outValuesPerRay = envmap_r_values.size() + 1;

    cudaMalloc(&d_angle_values, num_rays * outValuesPerRay * sizeof(float));
    cudaMalloc(&d_envmap_r_values, envmap_r_values.size() * sizeof(float));
    cudaMemcpy(d_envmap_r_values, envmap_r_values.data(), envmap_r_values.size() * sizeof(float), cudaMemcpyHostToDevice);

    int threadsPerBlock = 256;
    int numBlocks = (num_rays + threadsPerBlock - 1) / threadsPerBlock;

    // Solve geodesics on GPU
    solveGeodesicKernel << <numBlocks, threadsPerBlock >> > (u_0, h, d_envmap_r_values, envmap_r_values.size(), num_rays, num_steps, d_angle_values);
    cudaDeviceSynchronize();

    angle_out.resize(num_rays * outValuesPerRay);
    cudaMemcpy(angle_out.data(), d_angle_values, num_rays * outValuesPerRay * sizeof(float), cudaMemcpyDeviceToHost);

    cudaFree(d_envmap_r_values);
    cudaFree(d_angle_values);
}
