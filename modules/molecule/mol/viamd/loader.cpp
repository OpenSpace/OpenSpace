#include "loader.h"

#include <core/md_allocator.h>
#include <core/md_array.h>
#include <core/md_log.h>
#include <core/md_simd.h>
#include <core/md_bitfield.h>
#include <core/md_os.h>
#include <md_pdb.h>
#include <md_gro.h>
#include <md_xtc.h>
#include <md_trr.h>
#include <md_xyz.h>
#include <md_trajectory.h>
#include <md_frame_cache.h>
#include <md_util.h>

#include <string.h>

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
    bool deperiodize;
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
    ASSERT(find_loaded_trajectory(key) == NULL);
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

static const str_t extensions[] = {
    STR("pdb"),
    STR("gro"),
    STR("xtc"),
    STR("trr"),
    STR("xyz"),
    STR("xmol"),
    STR("arc"),
};

uint32_t get_supported_extension_count() {
    return (uint32_t)ARRAY_SIZE(extensions);
}

const str_t* get_supported_extensions() {
    return extensions;
}

namespace mol {

md_molecule_api* get_api(str_t filename) {
    str_t ext = extract_ext(filename);
    if (str_equal_cstr(ext, "pdb"))  return md_pdb_molecule_api();
    if (str_equal_cstr(ext, "gro"))  return md_gro_molecule_api();
    if (str_equal_cstr(ext, "xyz"))  return md_xyz_molecule_api();
    if (str_equal_cstr(ext, "xmol")) return md_xyz_molecule_api();
    if (str_equal_cstr(ext, "arc"))  return md_xyz_molecule_api();

    return NULL;
}

}  // namespace mol

namespace traj {

md_trajectory_api* get_api(str_t filename) {
    str_t ext = extract_ext(filename);
    if (str_equal_cstr(ext, "pdb"))  return md_pdb_trajectory_api();
    if (str_equal_cstr(ext, "xtc"))  return md_xtc_trajectory_api();
    if (str_equal_cstr(ext, "trr"))  return md_trr_trajectory_api();
    if (str_equal_cstr(ext, "xyz"))  return md_xyz_trajectory_api();
    if (str_equal_cstr(ext, "xmol")) return md_xyz_trajectory_api();
    if (str_equal_cstr(ext, "arc"))  return md_xyz_trajectory_api();

    return NULL;
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

bool decode_frame_data(struct md_trajectory_o* inst, const void* data_ptr, [[maybe_unused]] int64_t data_size, md_trajectory_frame_header_t* header, float* out_x, float* out_y, float* out_z) {
    LoadedTrajectory* loaded_traj = (LoadedTrajectory*)inst;
    ASSERT(loaded_traj);
    ASSERT(data_size == sizeof(int64_t));

    int64_t idx = *((int64_t*)data_ptr);
    ASSERT(0 <= idx && idx < md_trajectory_num_frames(loaded_traj->traj));

    md_frame_data_t* frame_data;
    md_frame_cache_lock_t* lock = 0;
    bool result = true;
    bool in_cache = md_frame_cache_find_or_reserve(&loaded_traj->cache, idx, &frame_data, &lock);
    if (!in_cache) {
        md_allocator_i* alloc = default_allocator;
        const int64_t frame_data_size = md_trajectory_fetch_frame_data(loaded_traj->traj, idx, 0);
        void* frame_data_ptr = md_alloc(alloc, frame_data_size);
        md_trajectory_fetch_frame_data(loaded_traj->traj, idx, frame_data_ptr);
        result = md_trajectory_decode_frame_data(loaded_traj->traj, frame_data_ptr, frame_data_size, &frame_data->header, frame_data->x, frame_data->y, frame_data->z);

        if (result) {
            const md_unit_cell_t* cell = &frame_data->header.cell;
            const bool have_cell = cell->flags != 0;

            const md_molecule_t* mol = loaded_traj->mol;
            float* x = frame_data->x;
            float* y = frame_data->y;
            float* z = frame_data->z;
            const int64_t num_atoms = frame_data->header.num_atoms;

            // If we have a recenter target, then compute the com and apply that transformation
            if (!md_bitfield_empty(&loaded_traj->recenter_target)) {
                const md_bitfield_t* bf = &loaded_traj->recenter_target;
                const int64_t count = md_bitfield_popcount(bf);
                
                if (count > 0) {
                    int32_t* indices = (int32_t*)md_alloc(alloc, sizeof(int32_t) * count);
                    defer { md_free(alloc, indices, sizeof(int32_t) * count); };
                        
                    md_bitfield_extract_indices(indices, bf);

                    const vec3_t box_ext = mat3_mul_vec3(header->cell.basis, vec3_set1(1.0f));

                    const vec3_t com = have_cell ?
                        vec3_deperiodize(md_util_compute_com_indexed_soa_ortho(x, y, z, mol->atom.mass, indices, count, box_ext), box_ext * 0.5f, box_ext) :
                        md_util_compute_com_indexed_soa(x, y, z, mol->atom.mass, indices, count);

                    // Translate all
                    const vec3_t trans = have_cell ? box_ext * 0.5f - com : -com;
                    for (int64_t i = 0; i < num_atoms; ++i) {
                        x[i] += trans.x;
                        y[i] += trans.y;
                        z[i] += trans.z;
                    }
                }
            }

            if (loaded_traj->deperiodize && have_cell) {
                md_util_deperiodize_system(x, y, z, cell, mol);
            }
        }

        md_free(alloc, frame_data_ptr, frame_data_size);
    }

    if (result) {
        const int64_t num_atoms = frame_data->header.num_atoms;
        if (header) *header = frame_data->header;
        if (out_x) MEMCPY(out_x, frame_data->x, sizeof(float) * num_atoms);
        if (out_y) MEMCPY(out_y, frame_data->y, sizeof(float) * num_atoms);
        if (out_z) MEMCPY(out_z, frame_data->z, sizeof(float) * num_atoms);
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

md_trajectory_i* open_file(str_t filename, const md_molecule_t* mol, md_allocator_i* alloc, bool deperiodize_on_load) {
    ASSERT(alloc);
    ASSERT(mol);

    md_trajectory_api* api = get_api(filename);
    if (!api) {
        MD_LOG_ERROR("Unsupported file extension: '%.*s'", filename.len, filename.ptr);
        return NULL;
    }

    md_trajectory_i* internal_traj = api->create(filename, alloc);
    if (!internal_traj) {
        return NULL;
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
    inst->deperiodize = deperiodize_on_load;
    
    const uint64_t num_atoms            = md_trajectory_num_atoms(internal_traj);
    const uint64_t num_traj_frames      = md_trajectory_num_frames(internal_traj);
    const uint64_t frame_cache_size     = MEGABYTES(16);
    const uint64_t approx_frame_size    = num_atoms * 3 * sizeof(float);
    const uint64_t max_num_cache_frames = MAX(16, frame_cache_size / approx_frame_size);

    const int64_t num_cache_frames   = MIN(num_traj_frames, max_num_cache_frames);
    
    MD_LOG_DEBUG("Initializing frame cache with %i frames.", (int)num_cache_frames);
    md_frame_cache_init(&inst->cache, inst->traj, alloc, num_cache_frames);
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
    MD_LOG_ERROR("Attempting to free trajectory which was not loaded with loader");
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
    MD_LOG_ERROR("Supplied trajectory was not loaded with loader");
    return false;
}

bool clear_cache(md_trajectory_i* traj) {
    ASSERT(traj);

    LoadedTrajectory* loaded_traj = find_loaded_trajectory((uint64_t)traj);
    if (loaded_traj) {
        md_frame_cache_clear(&loaded_traj->cache);
        return true;
    }
    MD_LOG_ERROR("Supplied trajectory was not loaded with loader");
    return false;
}

int64_t num_cache_frames(md_trajectory_i* traj) {
    ASSERT(traj);

    LoadedTrajectory* loaded_traj = find_loaded_trajectory((uint64_t)traj);
    if (loaded_traj) {
        return md_frame_cache_num_frames(&loaded_traj->cache);
    }
    MD_LOG_ERROR("Supplied trajectory was not loaded with loader");
    return 0;
}

}  // namespace traj

}  // namespace load
