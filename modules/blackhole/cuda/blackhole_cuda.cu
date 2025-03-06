#include <cuda_runtime.h>
#include <vector>
#include "device_launch_parameters.h"

__constant__ float PI = 3.1415926535897932384626433832795f;

__device__ void geodesic(float u, float dudphi, float& out_du_dphi, float& out_d2u_dphi2, float rs) {
    out_du_dphi = dudphi;
    out_d2u_dphi2 = -u * (1.f - 3.f / 2.f * rs * u);
}

__device__ void rk4_step(float& u, float& dudphi, float& phi, float h, float rs) {
    float k1_u, k1_dudphi, k2_u, k2_dudphi, k3_u, k3_dudphi, k4_u, k4_dudphi;

    geodesic(u, dudphi, k1_u, k1_dudphi, rs);
    geodesic(u + 0.5f * k1_u * h, dudphi + 0.5f * k1_dudphi * h, k2_u, k2_dudphi, rs);
    geodesic(u + 0.5f * k2_u * h, dudphi + 0.5f * k2_dudphi * h, k3_u, k3_dudphi, rs);
    geodesic(u + k3_u * h, dudphi + k3_dudphi * h, k4_u, k4_dudphi, rs);

    phi += h;
    u = u + (k1_u + 2.f * k2_u + 2.f * k3_u + k4_u) * h / 6.f;
    dudphi = dudphi + (k1_dudphi + 2.f * k2_dudphi + 2.f * k3_dudphi + k4_dudphi) * h / 6.f;
}

__global__ void solveGeodesicKernel(float rs, float envmap_r, float u_0, float* dudphi_0_values, float h, int num_paths, int num_steps, float* angles_out) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= num_paths) return;

    float u = u_0;
    float dudphi = dudphi_0_values[idx];
    float phi = 0.0f;

    // Perform the first RK4 step
    rk4_step(u, dudphi, phi, h, rs);

    float r = 1.0f / u;
    float r_0 = 1.0f / u_0;
    float a = r * sin(phi);
    float b = r * cos(phi) - r_0;

    // Store starting angle (angle is local to camera)
    angles_out[idx * 2] = atan2(a, b);

    auto out_of_bounds = [&u, &envmap_r]() -> bool {
        return (1.0f / u > envmap_r);
        };

    auto inside_singularity = [&u, &rs]() -> bool {
        return (1.0f / u <= rs);
        };

    for (int step = 1; step < num_steps && !out_of_bounds() && !inside_singularity(); step++) {
        rk4_step(u, dudphi, phi, h, rs);
    }

    angles_out[idx * 2 + 1] = !inside_singularity() ? phi : nan("");
}

void generate_du(float* d_du_0_values, float min, float max, int count) {
    float step = (max - min) / (count - 1);
    for (int i = 0; i < count; ++i) {
        d_du_0_values[i] = min + step * i;
    }
}

void schwarzchild(
    float rs, float envmap_r, int num_paths, int num_steps, float u_0, float h, float* angle_out) {

    float* d_dudphi_0_values;
    float* d_angle_values;

    // Allocate device memory
    cudaMalloc(&d_dudphi_0_values, num_paths * sizeof(float));
    cudaMalloc(&d_angle_values, num_paths * 2 * sizeof(float));

    // Copy initial velocity values to device
    std::vector<float> dudphi_0_values(num_paths, 0.f);
    generate_du(dudphi_0_values.data(), 1, -1, num_paths);
    cudaMemcpy(d_dudphi_0_values, dudphi_0_values.data(), num_paths * sizeof(float), cudaMemcpyHostToDevice);

    // Launch kernel
    int threadsPerBlock = 256;
    int numBlocks = (num_paths + threadsPerBlock - 1) / threadsPerBlock;
    solveGeodesicKernel<<<numBlocks, threadsPerBlock>>>(rs, envmap_r, u_0, d_dudphi_0_values, h, num_paths, num_steps, d_angle_values);

    cudaMemcpy(angle_out, d_angle_values, num_paths * 2 * sizeof(float), cudaMemcpyDeviceToHost);

    cudaFree(d_dudphi_0_values);
    cudaFree(d_angle_values);
}
