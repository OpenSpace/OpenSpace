#ifndef __OPENSPACE_MODULE_BLACKHOLE___KERR_CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___KERR_CUDA___H__
#include <vector>
void kerr(float x, float y, float z, float rs, float kerr, float env_map_r, size_t num_rays_per_dim, size_t num_steps, std::vector<float>& lookup_table_host);
#endif
