#ifndef __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#include <vector>

void cuda_test(int num_paths, int num_steps, double u_0, std::vector<double>& du_0_values, std::vector<double>& u_out, std::vector<double>& phi_out);


#endif
