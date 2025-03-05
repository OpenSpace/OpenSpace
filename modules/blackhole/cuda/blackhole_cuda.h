#ifndef __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___CUDA___H__
#include <vector>

void schwarzchild(double rs, double envmap_r, int num_paths, int num_steps, double u_0, double h, double* angle_out);

#endif
