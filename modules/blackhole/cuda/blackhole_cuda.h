#ifndef __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#include <vector>

void schwarzchild(float rs, float envmap_r, int num_paths, int num_steps, float u_0, float h, float* angle_out);

#endif
