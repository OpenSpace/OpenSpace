#include "blackhole_cuda.h"
#include <iostream>
#include <cuda_runtime.h>

__global__ void test_kernal() {
    printf("HelLo form GPU!!!");
}

void openspace::cuda_test()
{
    test_kernal<<<1, 1>>>();
}
