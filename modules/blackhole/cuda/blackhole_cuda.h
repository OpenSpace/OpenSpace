#ifndef __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#include <vector>

void schwarzchild(float rs, std::vector<float> envmap_r_values, size_t num_rays, size_t num_steps, float u_0, float h, std::vector<float>& angle_out);

#endif
