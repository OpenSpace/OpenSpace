#ifndef __OPENSPACE_MODULE_BLACKHOLE___KERR_CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___KERR_CUDA___H__
#include <vector>
#include <glm/vec3.hpp>

void traceKerr(glm::vec3 position, float rs, float kerr, std::vector<float> env_r_values, size_t num_rays_per_dim, size_t num_steps, std::vector<float>& lookup_table_host);
#endif
