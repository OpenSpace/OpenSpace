#ifndef __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#include <vector>

void schwarzchild(float rs, float envmap_r, size_t num_paths, size_t num_steps, float u_0, float h, float* angle_out);

#endif
