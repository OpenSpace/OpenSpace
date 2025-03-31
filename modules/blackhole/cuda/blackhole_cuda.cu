#include <cuda_runtime.h>
#include <vector>
#include "device_launch_parameters.h"
#include "blackhole_cuda.h"

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

__global__ void solveGeodesicKernel(float rs, float u_0, float* dudphi_0_values, float h, float* envmap_r_values, size_t num_envmaps, size_t num_rays, size_t num_steps, float* angles_out) {
    size_t idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= num_rays) return;

    size_t const outNodeSize = num_envmaps + 1;

    float u = u_0;
    float dudphi = dudphi_0_values[idx];
    float phi = 0.0f;

    // Perform the first RK4 step
    rk4_step(u, dudphi, phi, h, rs);

    float r = 1.0f / u;
    float const r_0 = 1.0f / u_0;
    float a = r * sin(phi);
    float b = r * cos(phi) - r_0;

    // Store starting angle (angle is local to camera)
    angles_out[idx * outNodeSize] = atan2(a, b);

    auto out_of_bounds = [&u, &envmap_r_values](size_t index) -> bool {
        return (1.0f / u > envmap_r_values[index]);
        };

    auto inside_singularity = [&u, &rs]() -> bool {
        return (1.0f / u <= rs);
        };

    size_t env_index = 0;
    for (size_t step = 1; step < num_steps && !out_of_bounds(num_envmaps-1) && !inside_singularity(); step++) {
        rk4_step(u, dudphi, phi, h, rs);
        if (out_of_bounds(env_index)) {
            angles_out[idx * outNodeSize + 1 + env_index++] = phi;
        }
    }
    angles_out[idx * outNodeSize + num_envmaps] = phi;
    for (size_t i = 0; i < num_envmaps; ++i) {
        angles_out[idx * outNodeSize + 1 + i] = !inside_singularity() ? angles_out[idx * outNodeSize + 1 + i] : nan("");
    }
}

void generate_du(float* d_du_0_values, float min, float max, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        float t = 2.0f * (i / static_cast<float>(count - 1)) - 1.0f;
        float smooth_t = (t < 0.0f ? -1.0f : 1.0f) * (powf(fabsf(t), 3.0f));
        d_du_0_values[i] = min + (smooth_t + 1.0f) * (max - min) / 2.0f;
    }  
}

void schwarzchild(
    float rs, std::vector<float> envmap_r_values, size_t num_rays, size_t num_steps, float u_0, float h, std::vector<float>& angle_out) {

    float* d_dudphi_0_values;
    float* d_angle_values;
    float* d_envmap_r_values;

    size_t const outValuesPerRay = (envmap_r_values.size() + 1);
    // Allocate device memory
    cudaMalloc(&d_dudphi_0_values, num_rays * sizeof(float));
    cudaMalloc(&d_angle_values, num_rays * outValuesPerRay * sizeof(float));
    cudaMalloc(&d_envmap_r_values, envmap_r_values.size() * sizeof(float));

    // Copy initial velocity values to device
    std::vector<float> dudphi_0_values(num_rays, 0.f);
    generate_du(dudphi_0_values.data(), 50000, -50000, num_rays);
    cudaMemcpy(d_dudphi_0_values, dudphi_0_values.data(), num_rays * sizeof(float), cudaMemcpyHostToDevice);
    cudaMemcpy(d_envmap_r_values, envmap_r_values.data(), envmap_r_values.size() * sizeof(float), cudaMemcpyHostToDevice);

    // Launch kernel
    int threadsPerBlock = 256;
    int numBlocks = (num_rays + threadsPerBlock - 1) / threadsPerBlock;
    solveGeodesicKernel<<<numBlocks, threadsPerBlock>>>(rs, u_0, d_dudphi_0_values, h, d_envmap_r_values, envmap_r_values.size(), num_rays, num_steps, d_angle_values);
    angle_out.resize(num_rays * outValuesPerRay, std::numeric_limits<double>::quiet_NaN());
    cudaMemcpy(angle_out.data(), d_angle_values, num_rays * outValuesPerRay * sizeof(float), cudaMemcpyDeviceToHost);

    // Add handeling of special case straight backwards
    //for (int i = 0; i < envmap_r_values.size(); ++i) {
    //    angle_out[(num_rays - 1) * outValusPerRay] = 0.0f;
    //    angle_out[(num_rays - 1) * outValusPerRay + 1 + i] = 0.0f;
    //}

    cudaFree(d_dudphi_0_values);
    cudaFree(d_envmap_r_values);
    cudaFree(d_angle_values);
}
