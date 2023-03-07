#include <md_molecule.h>
#include <md_trajectory.h>
#include <md_util.h>
#include <core/md_allocator.h>
#include "viamd/loader.h"

#include <string>
#include <unordered_map>
#include <ghoul/logging/logmanager.h>

// We have to do this dance since the type stored is std:string and we want to
// do lookups with std::string_view
struct string_hash {
    using is_transparent = void;
    using hash_type = std::hash<std::string_view>;  // just a helper local type

    size_t operator()(std::string_view txt) const   { return hash_type{}(txt); }
    size_t operator()(const std::string& txt) const { return hash_type{}(txt); }
    size_t operator()(const char* txt) const        { return hash_type{}(txt); }
};

static std::unordered_map<std::string, md_molecule_t,    string_hash, std::equal_to<>> molecules;
static std::unordered_map<std::string, md_trajectory_i*, string_hash, std::equal_to<>> trajectories;

namespace mol_manager {

constexpr const char* _loggerCat = "MoleculeManager";

const md_molecule_t* get_molecule(std::string_view path_to_file) {

    // @TODO: Remove string when C++ 20 is supported
    std::string path(path_to_file);
    auto entry = molecules.find(path);
    if (entry != molecules.end()) {
        return &entry->second;
    }

    if (path_to_file.empty() || path_to_file == "") {
        return nullptr;
    }

    str_t str = {path_to_file.data(), (int64_t)path_to_file.length()};
    md_molecule_api* api = load::mol::get_api(str);
    if (!api) {
        LERROR("Failed to find appropriate molecule loader api for file");
        return nullptr;
    }

    md_molecule_t mol = {};
    if (!api->init_from_file(&mol, str, default_allocator)) {
        LERROR("Failed to load molecule");
        return nullptr;
    }

    md_util_postprocess_molecule(&mol, default_allocator, MD_UTIL_POSTPROCESS_ALL);

    return &(molecules[path] = mol);
}

struct md_mem_trajecory_t {
    md_trajectory_header_t header;
    md_array(float) x;
    md_array(float) y;
    md_array(float) z;
    md_array(md_trajectory_frame_header_t) frame_headers;
    md_allocator_i* alloc;
};

static bool get_header(struct md_trajectory_o* inst, md_trajectory_header_t* header) {
    ASSERT(inst);
    md_mem_trajecory_t* mem = (md_mem_trajecory_t*)inst;
    *header = mem->header;
    return true;
}

static bool load_frame(struct md_trajectory_o* inst, int64_t idx, md_trajectory_frame_header_t* header, float* x, float* y, float* z) {
    ASSERT(inst);
    md_mem_trajecory_t* mem = (md_mem_trajecory_t*)inst;
    if (idx < 0 || mem->header.num_frames <= idx) {
        return false;
    }
    
    if (header) {
        MEMCPY(header, &mem->frame_headers[idx], sizeof(md_trajectory_frame_header_t));
    }
    if (x) {
        MEMCPY(x, &mem->x[idx * mem->header.num_atoms], sizeof(float) * mem->header.num_atoms);
    }
    if (y) {
        MEMCPY(y, &mem->y[idx * mem->header.num_atoms], sizeof(float) * mem->header.num_atoms);
    }
    if (z) {
        MEMCPY(z, &mem->z[idx * mem->header.num_atoms], sizeof(float) * mem->header.num_atoms);
    }

    return true;
}

static int64_t fetch_frame_data([[maybe_unused]] struct md_trajectory_o* inst, int64_t idx, void* data_ptr) {
    ASSERT(inst);
    if (data_ptr) {
        MEMCPY(data_ptr, &idx, sizeof(int64_t));
    }
    return sizeof(int64_t);
}

static bool decode_frame_data(struct md_trajectory_o* inst, const void* data_ptr, [[maybe_unused]] int64_t data_size, md_trajectory_frame_header_t* header, float* x, float* y, float* z) {
    ASSERT(inst);
    ASSERT(data_ptr);
    ASSERT(data_size == sizeof(int64_t));

    const int64_t idx = *(int64_t*)data_ptr;
    return load_frame(inst, idx, header, x, y, z);
}

static md_trajectory_i* mem_trajectory_create(const md_trajectory_i* backing_traj, md_allocator_i* alloc) {
    ASSERT(backing_traj);
    ASSERT(alloc);

    void* data = (md_trajectory_i*)md_alloc(alloc, sizeof(md_trajectory_i) + sizeof(md_mem_trajecory_t));
    memset(data, 0, sizeof(md_trajectory_i) + sizeof(md_mem_trajecory_t));

    md_trajectory_i* traj = (md_trajectory_i*)data;
    md_mem_trajecory_t* mem = (md_mem_trajecory_t*)(traj + 1);

    traj->inst = (md_trajectory_o*)mem;
    
    traj->get_header = get_header;
    traj->load_frame = load_frame;
    traj->fetch_frame_data = fetch_frame_data;
    traj->decode_frame_data = decode_frame_data;
    
    const int64_t num_frames = md_trajectory_num_frames(backing_traj);
    const int64_t num_atoms = md_trajectory_num_atoms(backing_traj);
    
    md_trajectory_get_header(backing_traj, &mem->header);
    md_array_resize(mem->frame_headers, num_frames, alloc);
    md_array_resize(mem->x, num_atoms * num_frames, alloc);
    md_array_resize(mem->y, num_atoms * num_frames, alloc);
    md_array_resize(mem->z, num_atoms * num_frames, alloc);
    
    for (int64_t i = 0; i < num_frames; ++i) {
        md_trajectory_load_frame(backing_traj, i, &mem->frame_headers[i], &mem->x[i * num_atoms], &mem->y[i * num_atoms], &mem->z[i * num_atoms]);
    }
    
    mem->alloc = alloc;
    
    return traj;
}

static void mem_trajectory_destroy(md_trajectory_i* traj) {
    ASSERT(traj);
    md_mem_trajecory_t* mem = (md_mem_trajecory_t*)traj->inst;
    
    md_array_free(mem->x, mem->alloc);
    md_array_free(mem->y, mem->alloc);
    md_array_free(mem->z, mem->alloc);
    md_array_free(mem->frame_headers, mem->alloc);
    
    md_free(mem->alloc, traj, sizeof(md_trajectory_i) + sizeof(md_mem_trajecory_t));
    memset(traj, 0, sizeof(md_trajectory_i));
}

const md_trajectory_i* get_trajectory(std::string_view path_to_file) {

    // @TODO: Remove string when C++ 20 is supported
    std::string path(path_to_file);
    auto entry = trajectories.find(path);
    if (entry != trajectories.end()) {
        return entry->second;
    }

    if (path_to_file.empty() || path_to_file == "") {
        return nullptr;
    }

    str_t str = {path_to_file.data(), (int64_t)path_to_file.length()};
    md_trajectory_api* api = load::traj::get_api(str);
    if (!api) {
        LERROR("Failed to find appropriate trajectory loader api for file");
        return nullptr;
    }

    md_trajectory_i* traj = api->create(str, default_allocator);
    if (!traj) {
        LERROR("Failed to load trajectory");
        return nullptr;
    }

    const int64_t num_frames = md_trajectory_num_frames(traj);
    const int64_t num_atoms  = md_trajectory_num_atoms(traj);
    const int64_t est_traj_size = num_frames * num_atoms * sizeof(float) * 3;

    if (est_traj_size < MEGABYTES(32)) {
        md_trajectory_i* mem_traj = mem_trajectory_create(traj, default_allocator);
        api->destroy(traj);
        traj = mem_traj;
    }

    return (trajectories[path] = traj);
}

}
