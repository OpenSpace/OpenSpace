#pragma once

#include <string_view>
#include <span>
#include <md_types.h>

struct md_molecule_t;
struct md_trajectory_i;
struct md_trajectory_frame_header_t;
struct md_frame_cache_lock_t;

// @NOTE(Robin) If you want to go full blown cplusplus bananas mode, you can use smart pointers (shared external + weak internal) to keep some type of reference counting going
// and free resources when they are not held by any external pointers anymore. I'm not going to care about that right now.

struct FrameData {
    ~FrameData();
    const md_trajectory_frame_header_t* header;
    std::span<const float> x;
    std::span<const float> y;
    std::span<const float> z;
    std::span<const md_secondary_structure_t> ss;
    md_frame_cache_lock_t* lock = nullptr;
};

namespace mol_manager {
    const md_molecule_t*   load_molecule(std::string_view path_to_file, bool coarse_grained = false);
    const md_trajectory_i* load_trajectory(std::string_view path_to_file, const md_molecule_t* mol = nullptr, bool deperiodize_on_load = false);
    
    // This will kick of work into worker threads and thus not stall the calling thread
    void prefetch_frames(const md_trajectory_i* traj, std::span<int64_t> frames);
    
    // FrameCoords also holds an optional lock for the frame if it is a cached trajectory.
    // This is released upon destruction of the object.
    // So don't attempt to store and access its x,y,z data after the object has been destroyed.
    FrameData get_frame_data(const md_trajectory_i* traj, int64_t frame);

    //std::span<const md_secondary_structure_t> get_frame_secondary_structures(const md_trajectory_i* traj, int64_t frame);
}
