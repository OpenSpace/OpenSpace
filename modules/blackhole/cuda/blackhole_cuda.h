#ifndef __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#include <vector>

void schwarzchild(std::vector<float> const& envmap_r_values, size_t const num_rays, size_t const num_steps, float const r_0, float const h, std::vector<float>& angle_out);

#endif
