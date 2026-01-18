/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/molecule/mol/cache.h>

#include <md_molecule.h>
#include <md_trajectory.h>
#include <md_util.h>
#include <md_frame_cache.h>
#include <core/md_allocator.h>
#include "viamd/loader.h"

#include <string>
#include <unordered_map>
#include <ghoul/logging/logmanager.h>

#include <openspace/util/threadpool.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

#include <modules/molecule/moleculemodule.h>

#define MD_CACHED_TRAJ_MAGIC    0x234236423674DBA2
#define MD_MEM_TRAJ_MAGIC       0x1289371265F17256

struct ss_data {
    md_array(md_secondary_structure_t) ss = 0;
    uint64_t stride = 0;
};

static size_t mol_hash(std::string_view path_to_file, bool coarse_grained) {
    size_t hash = std::hash<std::string_view>{}(path_to_file);
    return (hash << 1) | (size_t)coarse_grained;
}

static uint64_t traj_hash(std::string_view path_to_file, bool deperiodize) {
    size_t hash = std::hash<std::string_view>{}(path_to_file);
    return (hash << 1) | (size_t)deperiodize;
}

static std::unordered_map<size_t, md_molecule_t> molecules;
static std::unordered_map<size_t, md_trajectory_i*> trajectories;
static std::unordered_map<const md_trajectory_i*, ss_data> ss_table;

namespace mol_manager {

constexpr const char* _loggerCat = "MoleculeManager";

const md_molecule_t* load_molecule(std::string_view path_to_file, bool coarse_grained) {

    const uint64_t hash = mol_hash(path_to_file, coarse_grained);
    auto entry = molecules.find(hash);
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

    const md_util_postprocess_flags_t flags = coarse_grained ? MD_UTIL_POSTPROCESS_COARSE_GRAINED : MD_UTIL_POSTPROCESS_ALL;
    md_util_postprocess_molecule(&mol, default_allocator, flags);

    return &(molecules[hash] = mol);
}

struct md_mem_trajectory_t {
    uint64_t magic;
    md_trajectory_header_t header;
    md_array(float) x;
    md_array(float) y;
    md_array(float) z;
    md_array(md_trajectory_frame_header_t) frame_headers;
    md_array(md_secondary_structure_t) secondary_structures;
    size_t secondary_structure_stride;
    md_allocator_i* alloc;
};

static bool mem_trajectory_get_header(struct md_trajectory_o* inst, md_trajectory_header_t* header) {
    ASSERT(inst);
    md_mem_trajectory_t* mem = (md_mem_trajectory_t*)inst;
    *header = mem->header;
    return true;
}

static bool mem_trajectory_load_frame(struct md_trajectory_o* inst, int64_t idx, md_trajectory_frame_header_t* header, float* x, float* y, float* z) {
    ASSERT(inst);
    md_mem_trajectory_t* mem = (md_mem_trajectory_t*)inst;
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

static int64_t mem_trajectory_fetch_frame([[maybe_unused]] struct md_trajectory_o* inst, int64_t idx, void* data_ptr) {
    ASSERT(inst);
    if (data_ptr) {
        MEMCPY(data_ptr, &idx, sizeof(int64_t));
    }
    return sizeof(int64_t);
}

static bool mem_trajectory_decode_frame(struct md_trajectory_o* inst, const void* data_ptr, [[maybe_unused]] int64_t data_size, md_trajectory_frame_header_t* header, float* x, float* y, float* z) {
    ASSERT(inst);
    ASSERT(data_ptr);
    ASSERT(data_size == sizeof(int64_t));

    const int64_t idx = *(int64_t*)data_ptr;
    return mem_trajectory_load_frame(inst, idx, header, x, y, z);
}

static md_trajectory_i* mem_trajectory_create(const md_trajectory_i* backing_traj, md_allocator_i* alloc, bool deperiodize_on_load, const md_molecule_t* mol) {
    ASSERT(backing_traj);
    ASSERT(alloc);

    void* data = (md_trajectory_i*)md_alloc(alloc, sizeof(md_trajectory_i) + sizeof(md_mem_trajectory_t));
    memset(data, 0, sizeof(md_trajectory_i) + sizeof(md_mem_trajectory_t));

    md_trajectory_i* traj = (md_trajectory_i*)data;
    md_mem_trajectory_t* mem = (md_mem_trajectory_t*)(traj + 1);

    traj->inst = (md_trajectory_o*)mem;
    
    traj->get_header = mem_trajectory_get_header;
    traj->load_frame = mem_trajectory_load_frame;
    traj->fetch_frame_data = mem_trajectory_fetch_frame;
    traj->decode_frame_data = mem_trajectory_decode_frame;
    
    const int64_t num_frames = md_trajectory_num_frames(backing_traj);
    const int64_t num_atoms = md_trajectory_num_atoms(backing_traj);
    
    md_trajectory_get_header(backing_traj, &mem->header);
    md_array_resize(mem->frame_headers, num_frames, alloc);
    md_array_resize(mem->x, num_atoms * num_frames, alloc);
    md_array_resize(mem->y, num_atoms * num_frames, alloc);
    md_array_resize(mem->z, num_atoms * num_frames, alloc);

    if (mol && mol->backbone.count) {
        md_array_resize(mem->secondary_structures, mol->backbone.count * num_frames, alloc);
        mem->secondary_structure_stride = mol->backbone.count;
        for (int64_t i = 0; i < md_array_size(mem->secondary_structures); ++i) {
            mem->secondary_structures[i] = MD_SECONDARY_STRUCTURE_COIL;
        }
    }

    for (int64_t i = 0; i < num_frames; ++i) {
        float* x = &mem->x[i * num_atoms];
        float* y = &mem->y[i * num_atoms];
        float* z = &mem->z[i * num_atoms];
        md_trajectory_load_frame(backing_traj, i, &mem->frame_headers[i], x, y, z);
        if (deperiodize_on_load && mol) {
            const md_unit_cell_t* cell = &mem->frame_headers[i].cell;
            md_util_deperiodize_system(x, y, z, cell, mol);
        }
    }
    
    mem->magic = MD_MEM_TRAJ_MAGIC;
    mem->alloc = alloc;
    
    return traj;
}

static void mem_trajectory_destroy(md_trajectory_i* traj) {
    ASSERT(traj);
    md_mem_trajectory_t* mem = (md_mem_trajectory_t*)traj->inst;
    
    md_array_free(mem->x, mem->alloc);
    md_array_free(mem->y, mem->alloc);
    md_array_free(mem->z, mem->alloc);
    md_array_free(mem->frame_headers, mem->alloc);
    
    md_free(mem->alloc, traj, sizeof(md_trajectory_i) + sizeof(md_mem_trajectory_t));
    memset(traj, 0, sizeof(md_trajectory_i));
}

static bool load_cache_frame_data(md_frame_data_t* frame_data, const md_trajectory_i* traj, int64_t frame, const md_molecule_t* mol, bool deperiodize_on_load) {
    md_allocator_i* alloc = default_allocator;
    const int64_t frame_data_size = md_trajectory_fetch_frame_data(traj, frame, 0);
    void* frame_data_ptr = md_alloc(alloc, frame_data_size);
    md_trajectory_fetch_frame_data(traj, frame, frame_data_ptr);
    bool result = md_trajectory_decode_frame_data(traj, frame_data_ptr, frame_data_size, &frame_data->header, frame_data->x, frame_data->y, frame_data->z);

    if (result) {
        const md_unit_cell_t* cell = &frame_data->header.cell;
        const bool have_cell = cell->flags != 0;

        if (deperiodize_on_load && have_cell) {
            md_util_deperiodize_system(frame_data->x, frame_data->y, frame_data->z, cell, mol);
        }
    }

    md_free(alloc, frame_data_ptr, frame_data_size);
    return result;
}

struct md_cached_trajectory_t {
    uint64_t magic;
    md_trajectory_header_t header;
    md_frame_cache_t cache;
    md_array(md_secondary_structure_t) secondary_structures;
    size_t secondary_structure_stride;
    const md_molecule_t* mol;
    bool deperiodize_on_load;
};

static bool cached_trajectory_get_header(struct md_trajectory_o* inst, md_trajectory_header_t* header) {
    md_cached_trajectory_t* cached_traj = (md_cached_trajectory_t*)inst;
    MEMCPY(header, &cached_traj->header, sizeof(md_trajectory_header_t));
    return true;
}

int64_t cached_trajectory_fetch_frame(struct md_trajectory_o*, int64_t idx, void* data_ptr) {
    if (data_ptr) {
        *((int64_t*)data_ptr) = idx;
    }
    return sizeof(int64_t);
}

bool cached_trajectory_decode_frame(struct md_trajectory_o* inst, const void* data_ptr, [[maybe_unused]] int64_t data_size, md_trajectory_frame_header_t* header, float* out_x, float* out_y, float* out_z) {
    ASSERT(inst);
    ASSERT(data_size == sizeof(int64_t));
    md_cached_trajectory_t* cached_traj = (md_cached_trajectory_t*)inst;

    int64_t idx = *((int64_t*)data_ptr);
    ASSERT(0 <= idx && idx < md_trajectory_num_frames(cached_traj->cache.traj));

    md_frame_data_t* frame_data;
    md_frame_cache_lock_t* lock = 0;
    bool result = true;
    bool in_cache = md_frame_cache_find_or_reserve(&cached_traj->cache, idx, &frame_data, &lock);
    if (!in_cache) {
        result = load_cache_frame_data(frame_data, cached_traj->cache.traj, idx, cached_traj->mol, cached_traj->deperiodize_on_load);
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

bool cached_trajectory_load_frame(struct md_trajectory_o* inst, int64_t idx, md_trajectory_frame_header_t* header, float* x, float* y, float* z) {
    void* frame_data = &idx;
    return cached_trajectory_decode_frame(inst, frame_data, sizeof(int64_t), header, x, y, z);
}

md_trajectory_i* cached_trajectory_create(md_trajectory_i* backing_traj, md_allocator_i* alloc, bool deperiodize_on_load, const md_molecule_t* mol) {
    ASSERT(backing_traj);
    ASSERT(alloc);
    
    void* data = md_alloc(alloc, sizeof(md_trajectory_i) + sizeof(md_cached_trajectory_t));
    MEMSET(data, 0, sizeof(md_trajectory_i) + sizeof(md_cached_trajectory_t));
    md_trajectory_i* traj = (md_trajectory_i*)data;
    md_cached_trajectory_t* cached_traj = (md_cached_trajectory_t*)(traj + 1);

    const int64_t num_frames = md_trajectory_num_frames(backing_traj);
    const int64_t num_cache_frames = MIN(32, num_frames);
    
    cached_traj->magic = MD_CACHED_TRAJ_MAGIC;
    cached_traj->deperiodize_on_load = deperiodize_on_load;
    cached_traj->mol = mol;
    if (mol && mol->backbone.count) {
        md_array_resize(cached_traj->secondary_structures, num_frames * mol->backbone.count, alloc);
        cached_traj->secondary_structure_stride = mol->backbone.count;
        for (int64_t i = 0; i < md_array_size(cached_traj->secondary_structures); ++i) {
            cached_traj->secondary_structures[i] = MD_SECONDARY_STRUCTURE_COIL;
        }
    }
    md_trajectory_get_header(backing_traj, &cached_traj->header);
    md_frame_cache_init(&cached_traj->cache, backing_traj, alloc, num_cache_frames);
    
    traj->inst = (md_trajectory_o*)cached_traj;
    traj->get_header = cached_trajectory_get_header;
    traj->fetch_frame_data = cached_trajectory_fetch_frame;
    traj->decode_frame_data = cached_trajectory_decode_frame;
    traj->load_frame = cached_trajectory_load_frame;
    
    return traj;
}

static void load_secondary_structure_data(md_trajectory_i* traj, const md_molecule_t* mol) {
    ASSERT(traj);
    ASSERT(traj->inst);
    ASSERT(mol);

    if (mol->backbone.range_count) {
        /*
        ss_data data{};
        
        md_array_resize(data.ss, mol->backbone.count * md_trajectory_num_frames(traj), default_allocator);
        for (int64_t i = 0; i < md_array_size(data.ss); ++i) {
            data.ss[i] = MD_SECONDARY_STRUCTURE_COIL;
        }
        data.stride = mol->backbone.count;
        ss_table[traj] = data;
        */

        uint64_t ss_stride = mol->backbone.count;
        md_secondary_structure_t* ss_data = nullptr;

        const uint64_t magic = *(uint64_t*)(traj->inst);
        switch (magic) {
        case MD_CACHED_TRAJ_MAGIC:
            ss_data = ((md_cached_trajectory_t*)traj->inst)->secondary_structures;
            break;
        case MD_MEM_TRAJ_MAGIC:
            ss_data = ((md_mem_trajectory_t*)traj->inst)->secondary_structures;
            break;
        }
        
        openspace::ThreadPool& threadPool = openspace::global::moduleEngine->module<openspace::MoleculeModule>()->threadPool();
        
        const int64_t num_chunks = 8;
        const int64_t num_frames = md_trajectory_num_frames(traj);
        for (int64_t i = 0; i < num_chunks; ++i) {
            const int64_t beg = i * num_frames / num_chunks;
            const int64_t end = (i == num_chunks - 1) ? num_frames : (i + 1) * num_frames / num_chunks;
            threadPool.enqueue([ss_data, ss_stride, traj, old_mol = mol, beg, end](){
                // @TODO(Robin) This is stupid and the interface to compute secondary structure should be changed
                md_molecule_t mol = *old_mol;
                const int64_t stride = ALIGN_TO(mol.atom.count, 16);
                const int64_t bytes = stride * sizeof(float) * 3;
                float* coords = (float*)md_alloc(default_allocator, bytes);
                defer { md_free(default_allocator, coords, bytes); };

                // Overwrite the coordinate section, since we will load trajectory frame data into these
                mol.atom.x = coords + stride * 0;
                mol.atom.y = coords + stride * 1;
                mol.atom.z = coords + stride * 2;

                for (int64_t i = beg; i < end; ++i) {
                    md_trajectory_load_frame(traj, i, NULL, mol.atom.x, mol.atom.y, mol.atom.z);
                    md_util_backbone_secondary_structure_compute(ss_data + ss_stride * i, ss_stride, &mol);
                }
            });
        }
    }
}

const md_trajectory_i* load_trajectory(std::string_view path_to_file, const md_molecule_t* mol, bool deperiodize_on_load) {
    const size_t hash = traj_hash(path_to_file, deperiodize_on_load);
    auto entry = trajectories.find(hash);
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

    if (deperiodize_on_load && !mol) {
        LERROR("Deperiodize trajectory was set, but no valid was molecule provided");
        return nullptr;
    }

    const int64_t num_frames = md_trajectory_num_frames(traj);
    const int64_t num_atoms  = md_trajectory_num_atoms(traj);
    const int64_t est_traj_size = num_frames * num_atoms * sizeof(float) * 3;

    if (est_traj_size < MEGABYTES(64)) {
        md_trajectory_i* mem_traj = mem_trajectory_create(traj, default_allocator, deperiodize_on_load, mol);
        traj = mem_traj;
        api->destroy(traj);
    } else {
        md_trajectory_i* cached_traj = cached_trajectory_create(traj, default_allocator, deperiodize_on_load, mol);
        traj = cached_traj;
    }

    if (traj && mol) {
        load_secondary_structure_data(traj, mol);
    }

    trajectories[hash] = traj;
    return traj;
}

void prefetch_frames(const md_trajectory_i* traj, std::span<int64_t> frames) {
    ASSERT(traj);
    
    if (traj->inst) {
        md_cached_trajectory_t* cached_traj = (md_cached_trajectory_t*)traj->inst;
        if (cached_traj->magic == MD_CACHED_TRAJ_MAGIC) {
            openspace::ThreadPool& threadPool = openspace::global::moduleEngine->module<openspace::MoleculeModule>()->threadPool();
            
            const int64_t num_frames = md_trajectory_num_frames(traj);
            for (int64_t i : frames) {
                if (i < 0 || num_frames <= i) continue;
                threadPool.enqueue([traj, i](){
                    md_trajectory_load_frame(traj, i, nullptr, nullptr, nullptr, nullptr);
                });
            }
        }
    }
}



FrameData get_frame_data(const md_trajectory_i* traj, int64_t frame) {
    FrameData data = {};
    if (!traj || !traj->inst) {
        LERROR("Invalid trajectory");
        return data;
    }
    
    if (0 <= frame && frame < md_trajectory_num_frames(traj)) {
        uint64_t magic = *(const uint64_t*)(traj->inst);
        switch (magic) {
        case MD_MEM_TRAJ_MAGIC:
        {
            const md_mem_trajectory_t* inst = (const md_mem_trajectory_t*)traj->inst;
            const size_t stride = inst->header.num_atoms;
            data.header = inst->frame_headers + frame;
            data.x = {inst->x + frame * stride, stride};
            data.y = {inst->y + frame * stride, stride};
            data.z = {inst->z + frame * stride, stride};
            data.ss = {inst->secondary_structures + frame * inst->secondary_structure_stride, inst->secondary_structure_stride};
            break;
        }
        case MD_CACHED_TRAJ_MAGIC:
        {
            md_cached_trajectory_t* inst = (md_cached_trajectory_t*)traj->inst;
            md_frame_data_t* frame_data;
            md_frame_cache_lock_t* lock = nullptr;
            bool result = true;
            bool in_cache = md_frame_cache_find_or_reserve(&inst->cache, frame, &frame_data, &lock);
            if (!in_cache) {
                result = load_cache_frame_data(frame_data, inst->cache.traj, frame, inst->mol, inst->deperiodize_on_load);
            }
            if (result) {
                data.header = &frame_data->header;
                data.x = {frame_data->x, (size_t)frame_data->header.num_atoms};
                data.y = {frame_data->y, (size_t)frame_data->header.num_atoms};
                data.z = {frame_data->z, (size_t)frame_data->header.num_atoms};
                data.ss = {inst->secondary_structures + frame * inst->secondary_structure_stride, inst->secondary_structure_stride};
                data.lock = lock;
            }
            break;
        }
        default:
            LERROR("Invalid trajectory");
        }
    } else {
        LERROR(std::format("Invalid trajectory index: {}, valid range: [0,{}]", frame, md_trajectory_num_frames(traj)));
    }
    return data;
}

std::span<const md_secondary_structure_t> get_secondary_structure_frame_data(const md_trajectory_i* traj, int64_t frame) {
    if (0 < frame && frame < md_trajectory_num_frames(traj)) {
        auto it = ss_table.find(traj);
        if (it != ss_table.end()) {
            const ss_data& data = it->second;
            const uint64_t ss_stride = data.stride;
            const md_secondary_structure_t* ss_src = data.ss + frame * ss_stride;
            return {ss_src, ss_stride};
        }
    }

    return {};
}

}

FrameData::~FrameData() {
    if (lock) {
        md_frame_cache_frame_lock_release(lock);
    }
}
