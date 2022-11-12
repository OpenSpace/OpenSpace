#pragma once

#include <core/md_str.h>

struct md_allocator_i;
struct md_molecule_t;
struct md_molecule_api;
struct md_trajectory_i;
struct md_trajectory_api;
struct md_bitfield_t;

namespace load {
namespace mol {
    md_molecule_api* get_api(str_t filename);
}

namespace traj {
    md_trajectory_api* get_api(str_t filename);

    md_trajectory_i* open_file(str_t filename, const md_molecule_t* mol, md_allocator_i* alloc);
    bool close(md_trajectory_i* traj);

    bool set_recenter_target(md_trajectory_i* traj, const md_bitfield_t* atom_mask);
    bool clear_cache(md_trajectory_i* traj);
}

}  // namespace load
