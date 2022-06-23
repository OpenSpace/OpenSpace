
#include "loader.h"

#include <core/md_allocator.h>
#include <core/md_array.inl>
#include <core/md_log.h>
#include <core/md_simd.h>
#include <core/md_bitfield.h>
#include <md_pdb.h>
#include <md_gro.h>
#include <md_xtc.h>
#include <md_xyz.h>
#include <md_trajectory.h>
#include <md_frame_cache.h>
#include <md_util.h>

// #include <string_util.h>
#include <string.h>

// #include "task_system.h"

struct LoadedMolecule {
    uint64_t key;
    md_allocator_i* alloc;
};

struct LoadedTrajectory {
    uint64_t key;
    const md_molecule_t* mol;
    md_trajectory_api* api;
    md_trajectory_i* traj;
    md_frame_cache_t cache;
    md_allocator_i* alloc;
    md_bitfield_t recenter_target;
};

static LoadedMolecule loaded_molecules[8] = {};
static int64_t num_loaded_molecules = 0;

static LoadedTrajectory loaded_trajectories[8] = {};
static int64_t num_loaded_trajectories = 0;

static inline LoadedMolecule* find_loaded_molecule(uint64_t key) {
    for (int64_t i = 0; i < num_loaded_molecules; ++i) {
        if (loaded_molecules[i].key == key) return &loaded_molecules[i];
    }
    return nullptr;
}

static inline void add_loaded_molecule(LoadedMolecule obj) {
    ASSERT(!find_loaded_molecule(obj.key));
    ASSERT(num_loaded_molecules < (int64_t)ARRAY_SIZE(loaded_molecules));
    loaded_molecules[num_loaded_molecules++] = obj;
}

static inline void remove_loaded_molecule(uint64_t key) {
    for (int64_t i = 0; i < num_loaded_molecules; ++i) {
        if (loaded_molecules[i].key == key) {
            loaded_molecules[i] = loaded_molecules[--num_loaded_molecules];
            return;
        }
    }
    ASSERT(false);
}

static inline LoadedTrajectory* find_loaded_trajectory(uint64_t key) {
    for (int64_t i = 0; i < num_loaded_trajectories; ++i) {
        if (loaded_trajectories[i].key == key) return &loaded_trajectories[i];
    }
    return nullptr;
}

static inline LoadedTrajectory* alloc_loaded_trajectory(uint64_t key) {
    ASSERT(find_loaded_trajectory(key) == nullptr);
    ASSERT(num_loaded_trajectories < (int64_t)ARRAY_SIZE(loaded_trajectories));
    LoadedTrajectory* traj = &loaded_trajectories[num_loaded_trajectories++];
    *traj = {0}; // Clear
    traj->key = key;
    return traj;
}

static inline void remove_loaded_trajectory(uint64_t key) {
    for (int64_t i = 0; i < num_loaded_trajectories; ++i) {
        if (loaded_trajectories[i].key == key) {
            md_frame_cache_free(&loaded_trajectories[i].cache);
            loaded_trajectories[i].api->destroy(loaded_trajectories[i].traj);
            // Swap back and pop
            loaded_trajectories[i] = loaded_trajectories[--num_loaded_trajectories];
            return;
        }
    }
    ASSERT(false);
}

namespace load {

namespace mol {

md_molecule_api* get_api(str_t filename) {
    str_t ext = extract_ext(filename);
    if (compare_str_cstr(ext, "pdb")) return md_pdb_molecule_api();
    if (compare_str_cstr(ext, "gro")) return md_gro_molecule_api();
    if (compare_str_cstr(ext, "xyz")) return md_xyz_molecule_api();
    if (compare_str_cstr(ext, "xmol")) return md_xyz_molecule_api();
    if (compare_str_cstr(ext, "arc")) return md_xyz_molecule_api();

    return nullptr;
}

}  // namespace mol

namespace traj {

md_trajectory_api* get_api(str_t filename) {
    str_t ext = extract_ext(filename);
    if (compare_str_cstr(ext, "pdb")) return md_pdb_trajectory_api();
    if (compare_str_cstr(ext, "xtc")) return md_xtc_trajectory_api();
    if (compare_str_cstr(ext, "xyz")) return md_xyz_trajectory_api();
    if (compare_str_cstr(ext, "xmol")) return md_xyz_trajectory_api();
    if (compare_str_cstr(ext, "arc")) return md_xyz_trajectory_api();

    return nullptr;
}

bool get_header(struct md_trajectory_o* inst, md_trajectory_header_t* header) {
    LoadedTrajectory* loaded_traj = (LoadedTrajectory*)inst;
    return md_trajectory_get_header(loaded_traj->traj, header);
}

int64_t fetch_frame_data(struct md_trajectory_o*, int64_t idx, void* data_ptr) {
    if (data_ptr) {
        *((int64_t*)data_ptr) = idx;
    }
    return sizeof(int64_t);
}

bool decode_frame_data(struct md_trajectory_o* inst, const void* data_ptr, [[maybe_unused]] int64_t data_size, md_trajectory_frame_header_t* header, float* x, float* y, float* z) {
    LoadedTrajectory* loaded_traj = (LoadedTrajectory*)inst;
    ASSERT(loaded_traj);
    ASSERT(data_size == sizeof(int64_t));

    int64_t idx = *((int64_t*)data_ptr);
    ASSERT(0 <= idx && idx < md_trajectory_num_frames(loaded_traj->traj));

    md_frame_data_t* frame_data;
    md_frame_cache_lock_t* lock = nullptr;
    bool result = true;
    bool in_cache = md_frame_cache_find_or_reserve(&loaded_traj->cache, idx, &frame_data, &lock);
    if (!in_cache) {
        md_allocator_i* alloc = default_allocator;
        const int64_t frame_data_size = md_trajectory_fetch_frame_data(loaded_traj->traj, idx, 0);
        void* frame_data_ptr = md_alloc(alloc, frame_data_size);
        md_trajectory_fetch_frame_data(loaded_traj->traj, idx, frame_data_ptr);
        result = md_trajectory_decode_frame_data(loaded_traj->traj, frame_data_ptr, frame_data_size, &frame_data->header, frame_data->x, frame_data->y, frame_data->z);

        if (result) {
            bool have_box = (frame_data->header.box[0][0] + frame_data->header.box[1][1] + frame_data->header.box[2][2]) > 0;
            vec3_t box_ext = {0,0,0};
            if (have_box) {
                box_ext = {frame_data->header.box[0][0], frame_data->header.box[1][1], frame_data->header.box[2][2]};
            }

            // If we have a recenter target, then compute and apply that transformation
            if (!md_bitfield_empty(&loaded_traj->recenter_target)) {
                int64_t count = md_bitfield_popcount(&loaded_traj->recenter_target);
                if (count > 0) {
                    // Allocate data for substructure
                    int64_t stride = ROUND_UP(count, md_simd_widthf);
                    const int64_t mem_size = stride * 4 * sizeof(float);
                    void* mem_ptr = md_alloc(alloc, mem_size);
                    float* tmp_x = (float*)mem_ptr + 0 * stride;
                    float* tmp_y = (float*)mem_ptr + 1 * stride;
                    float* tmp_z = (float*)mem_ptr + 2 * stride;
                    float* tmp_w = (float*)mem_ptr + 3 * stride;

                    // Extract fields for substructure
                    int64_t dst_idx = 0;
                    int64_t beg_bit = loaded_traj->recenter_target.beg_bit;
                    int64_t end_bit = loaded_traj->recenter_target.end_bit;
                    while ((beg_bit = md_bitfield_scan(&loaded_traj->recenter_target, beg_bit, end_bit)) != 0) {
                        int64_t src_idx = beg_bit - 1;
                        tmp_x[dst_idx] = frame_data->x[src_idx];
                        tmp_y[dst_idx] = frame_data->y[src_idx];
                        tmp_z[dst_idx] = frame_data->z[src_idx];
                        tmp_w[dst_idx] = loaded_traj->mol->atom.mass[src_idx];
                        dst_idx += 1;
                    }

                    // Compute deperiodized com for substructure
                    vec3_t com;
                    if (have_box) {
                        com = md_util_compute_com_periodic(tmp_x, tmp_y, tmp_z, tmp_w, count, box_ext);
                    }
                    else {
                        com = md_util_compute_com(tmp_x, tmp_y, tmp_z, tmp_w, count);
                    }

                    // Translate all
                    vec3_t trans = box_ext * 0.5f - com;
                    for (int64_t i = 0; i < frame_data->header.num_atoms; ++i) {
                        frame_data->x[i] += trans.x;
                        frame_data->y[i] += trans.y;
                        frame_data->z[i] += trans.z;
                    }

                    md_free(alloc, mem_ptr, mem_size);
                }
            }

            // Deperiodize
            if (have_box) {
                md_molecule_t mol = *loaded_traj->mol;
                mol.atom.x = frame_data->x;
                mol.atom.y = frame_data->y;
                mol.atom.z = frame_data->z;
                md_util_apply_pbc(&mol, box_ext);
            }
        }

        md_free(alloc, frame_data_ptr, frame_data_size);
    }

    if (result) {
        const int64_t num_atoms = frame_data->header.num_atoms;
        if (header) *header = frame_data->header;
        if (x) memcpy(x, frame_data->x, sizeof(float) * num_atoms);
        if (y) memcpy(y, frame_data->y, sizeof(float) * num_atoms);
        if (z) memcpy(z, frame_data->z, sizeof(float) * num_atoms);
    }

    if (lock) {
        md_frame_cache_frame_lock_release(lock);
    }

    return result;
}

bool load_frame(struct md_trajectory_o* inst, int64_t idx, md_trajectory_frame_header_t* header, float* x, float* y, float* z) {
    void* frame_data = &idx;
    return decode_frame_data(inst, frame_data, sizeof(int64_t), header, x, y, z);
}

md_trajectory_i* open_file(str_t filename, const md_molecule_t* mol, md_allocator_i* alloc) {
    ASSERT(mol);
    ASSERT(alloc);

    md_trajectory_api* api = get_api(filename);
    if (!api) {
        md_printf(MD_LOG_TYPE_ERROR, "Unsupported file extension: '%.*s'", filename.len, filename.ptr);
        return nullptr;
    }

    md_trajectory_i* internal_traj = api->create(filename, alloc);
    if (!internal_traj) {
        return nullptr;
    }
    
    if (md_trajectory_num_atoms(internal_traj) != mol->atom.count) {
        md_printf(MD_LOG_TYPE_ERROR, "Trajectory is not compatible with the loaded molecule.");
        api->destroy(internal_traj);
        return nullptr;
    }

    md_trajectory_i* traj = (md_trajectory_i*)md_alloc(alloc, sizeof(md_trajectory_i));
    memset(traj, 0, sizeof(md_trajectory_i));

    LoadedTrajectory* inst = alloc_loaded_trajectory((uint64_t)traj);
    inst->mol = mol;
    inst->api = api;
    inst->traj = internal_traj;
    inst->cache = {0};
    inst->recenter_target = {0};
    inst->alloc = alloc;
    
    md_frame_cache_init(&inst->cache, inst->traj, alloc, md_trajectory_num_frames(inst->traj));
    md_bitfield_init(&inst->recenter_target, alloc);

    // We only overload load frame and decode frame data to apply PBC upon loading data
    traj->inst = (struct md_trajectory_o*)inst;
    traj->get_header = get_header;
    traj->load_frame = load_frame;
    traj->fetch_frame_data = fetch_frame_data;
    traj->decode_frame_data = decode_frame_data;
    
    return traj;
}

bool close(md_trajectory_i* traj) {
    ASSERT(traj);

    LoadedTrajectory* loaded_traj = find_loaded_trajectory((uint64_t)traj);
    if (loaded_traj) {
        remove_loaded_trajectory(loaded_traj->key);
        memset(traj, 0, sizeof(md_trajectory_i));
        return true;
    }
    md_print(MD_LOG_TYPE_ERROR, "Attempting to free trajectory which was not loaded with loader");
    ASSERT(false);
    return false;
}

bool set_recenter_target(md_trajectory_i* traj, const md_bitfield_t* atom_mask) {
    ASSERT(traj);

    LoadedTrajectory* loaded_traj = find_loaded_trajectory((uint64_t)traj);
    if (loaded_traj) {
        if (atom_mask) {
            md_bitfield_copy(&loaded_traj->recenter_target, atom_mask);
        }
        else {
            md_bitfield_clear(&loaded_traj->recenter_target);
        }
        return true;
    }
    md_print(MD_LOG_TYPE_ERROR, "Supplied trajectory was not loaded with loader");
    return false;
}

bool clear_cache(md_trajectory_i* traj) {
    ASSERT(traj);

    LoadedTrajectory* loaded_traj = find_loaded_trajectory((uint64_t)traj);
    if (loaded_traj) {
        md_frame_cache_clear(&loaded_traj->cache);
        return true;
    }
    md_print(MD_LOG_TYPE_ERROR, "Supplied trajectory was not loaded with loader");
    return false;
}

}  // namespace traj

}  // namespace load
