#include <cuda_runtime.h>
#include <vector>
#include "device_launch_parameters.h"

__constant__ double rs = 1;

__device__ void geodesic(double u, double du, double& out_u, double& out_du) {
    double temp_u = u;
    out_u = du;
    out_du = -temp_u * (1 - (3 / 2) * rs * temp_u);
}

__device__ void rk4_step(double& u, double& du, double& phi, double h) {
    double k1_u, k1_du, k2_u, k2_du, k3_u, k3_du, k4_u, k4_du;

    geodesic(u, du, k1_u, k1_du);
    geodesic(u + 0.5 * k1_u * h, du + 0.5 * k1_du * h, k2_u, k2_du);
    geodesic(u + 0.5 * k2_u * h, du + 0.5 * k2_du * h, k3_u, k3_du);
    geodesic(u + k3_u * h, du + k3_du * h, k4_u, k4_du);

    phi += h;
    u = u + (k1_u + 2 * k2_u + 2 * k3_u + k4_u) * h / 6;
    du = du + (k1_du + 2 * k2_du + 2 * k3_du + k4_du) * h / 6;
}

__global__ void solve_geodesic_kernel(double u_0, double* du_0_values, double h, int num_paths, int num_steps, double* u_values, double* du_values, double* phi_values) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= num_paths) return;

    double u = u_0;
    double du = du_0_values[idx];
    double phi = 0.0;

    for (int step = 0; step < num_steps; step++) {
        u_values[idx * num_steps + step] = u;
        du_values[idx * num_steps + step] = du;
        phi_values[idx * num_steps + step] = phi;

        rk4_step(u, du, phi, h);
    }
}

extern "C" {
    __declspec(dllexport) void cuda_test(
        int num_paths, int num_steps, double u_0,
        double* du_0_values, double h, double* u_out, double* phi_out) {

        // Allocate device memory
        double* d_du_0_values;
        double* d_u_values;
        double* d_du_values;
        double* d_phi_values;

        cudaMalloc(&d_du_0_values, num_paths * sizeof(double));
        cudaMalloc(&d_u_values, num_paths * num_steps * sizeof(double));
        cudaMalloc(&d_du_values, num_paths * num_steps * sizeof(double));
        cudaMalloc(&d_phi_values, num_paths * num_steps * sizeof(double));


        // Copy initial velocity values to device
        cudaMemcpy(d_du_0_values, du_0_values, num_paths * sizeof(double), cudaMemcpyHostToDevice);

        // Launch kernel
        int threadsPerBlock = 256;
        int numBlocks = (num_paths + threadsPerBlock - 1) / threadsPerBlock;
        solve_geodesic_kernel << <numBlocks, threadsPerBlock >> > (u_0, d_du_0_values, h, num_paths, num_steps, d_u_values, d_du_values, d_phi_values);

        // Copy results back to host
        cudaMemcpy(u_out, d_u_values, num_paths * num_steps * sizeof(double), cudaMemcpyDeviceToHost);
        cudaMemcpy(phi_out, d_phi_values, num_paths * num_steps * sizeof(double), cudaMemcpyDeviceToHost);

        cudaFree(d_du_0_values);
        cudaFree(d_u_values);
        cudaFree(d_du_values);
        cudaFree(d_phi_values);
    }
}
