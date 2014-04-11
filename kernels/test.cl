
#ifndef OFFSET
#define OFFSET 0
#endif

__kernel void hello(__global int * out)
{
size_t tid = get_global_id(0);
out[tid] = tid + OFFSET;
}
