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

#include <modules/molecule/src/cache.h>

#include <modules/molecule/moleculemodule.h>
#include <modules/molecule/src/loader.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/util/threadpool.h>
#include <ghoul/logging/logmanager.h>
#include <core/md_allocator.h>
#include <md_molecule.h>
#include <md_trajectory.h>
#include <md_util.h>
#include <md_frame_cache.h>
#include <string>
#include <string_view>
#include <unordered_map>

namespace {
    constexpr std::string_view _loggerCat = "MoleculeManager";

    constexpr uint64_t MdCachedTrajMagic = 0x234236423674DBA2;
    constexpr uint64_t MdMemTrajMagic = 0x1289371265F17256;

    struct SecondaryStructureData {
        md_array(md_secondary_structure_t) ss = 0;
        uint64_t stride = 0;
    };

    struct md_mem_trajectory_t {
        uint64_t magic;
        md_trajectory_header_t header;
        md_array(float) x;
        md_array(float) y;
        md_array(float) z;
        md_array(md_trajectory_frame_header_t) frameHeaders;
        md_array(md_secondary_structure_t) secondaryStructures;
        size_t secondaryStructureStride;
        md_allocator_i* alloc;
    };

    struct md_cached_trajectory_t {
        uint64_t magic;
        md_trajectory_header_t header;
        md_frame_cache_t cache;
        md_array(md_secondary_structure_t) secondaryStructures;
        size_t secondaryStructureStride;
        const md_molecule_t* mol;
        bool deperiodizeOnLoad;
    };


    std::unordered_map<size_t, md_molecule_t> molecules;
    std::unordered_map<size_t, md_trajectory_i*> trajectories;
    std::unordered_map<const md_trajectory_i*, SecondaryStructureData> ssTable;

    size_t molHash(std::filesystem::path file, bool isCoarseGrained) {
        size_t hash = std::hash<std::filesystem::path>{}(file);
        return (hash << 1) | static_cast<size_t>(isCoarseGrained);
    }

    uint64_t trajHash(std::filesystem::path file, bool deperiodize) {
        size_t hash = std::hash<std::filesystem::path>{}(file);
        return (hash << 1) | static_cast<size_t>(deperiodize);
    }

    bool memTrajectoryGetHeader(md_trajectory_o* inst, md_trajectory_header_t* header) {
        ghoul_assert(inst, "No instance");
        md_mem_trajectory_t* mem = reinterpret_cast<md_mem_trajectory_t*>(inst);
        *header = mem->header;
        return true;
    }

    bool memTrajectoryLoadFrame(md_trajectory_o* inst, int64_t idx,
                                md_trajectory_frame_header_t* header, float* x, float* y,
                                float* z)
    {
        ghoul_assert(inst, "No instance");
        md_mem_trajectory_t* mem = reinterpret_cast<md_mem_trajectory_t*>(inst);
        if (idx < 0 || mem->header.num_frames <= idx) {
            return false;
        }

        if (header) {
            std::memcpy(
                header,
                &mem->frameHeaders[idx],
                sizeof(md_trajectory_frame_header_t)
            );
        }
        if (x) {
            std::memcpy(
                x,
                &mem->x[idx * mem->header.num_atoms],
                mem->header.num_atoms * sizeof(float)
            );
        }
        if (y) {
            std::memcpy(
                y,
                &mem->y[idx * mem->header.num_atoms],
                mem->header.num_atoms * sizeof(float)
            );
        }
        if (z) {
            std::memcpy(
                z,
                &mem->z[idx * mem->header.num_atoms],
                mem->header.num_atoms * sizeof(float)
            );
        }

        return true;
    }

    int64_t memTrajectoryFetchFrame(md_trajectory_o*, int64_t idx, void* data) {
        if (data) {
            std::memcpy(data, &idx, sizeof(int64_t));
        }
        return sizeof(int64_t);
    }

    bool memTrajectoryDecodeFrame(md_trajectory_o* inst, const void* data,
                                  [[maybe_unused]] int64_t dataSize,
                                  md_trajectory_frame_header_t* header, float* x,
                                  float* y, float* z)
    {
        ghoul_assert(inst, "No instance");
        ghoul_assert(data, "No data");
        ghoul_assert(dataSize == sizeof(int64_t), "Invalid data size");

        const int64_t idx = *reinterpret_cast<const int64_t*>(data);
        return memTrajectoryLoadFrame(inst, idx, header, x, y, z);
    }

    md_trajectory_i* memTrajectoryCreate(const md_trajectory_i* backingTraj,
                                         md_allocator_i* alloc, bool deperiodizeOnLoad,
                                         const md_molecule_t* mol)
    {
        ghoul_assert(backingTraj, "No backing trajectory");
        ghoul_assert(alloc, "No allocator");

        void* data = reinterpret_cast<md_trajectory_i*>(
            md_alloc(alloc, sizeof(md_trajectory_i) + sizeof(md_mem_trajectory_t))
        );
        std::memset(data, 0, sizeof(md_trajectory_i) + sizeof(md_mem_trajectory_t));

        md_trajectory_i* traj = reinterpret_cast<md_trajectory_i*>(data);
        md_mem_trajectory_t* mem = reinterpret_cast<md_mem_trajectory_t*>(traj + 1);

        traj->inst = reinterpret_cast<md_trajectory_o*>(mem);
        traj->get_header = memTrajectoryGetHeader;
        traj->load_frame = memTrajectoryLoadFrame;
        traj->fetch_frame_data = memTrajectoryFetchFrame;
        traj->decode_frame_data = memTrajectoryDecodeFrame;

        const int64_t numFrames = md_trajectory_num_frames(backingTraj);
        const int64_t numAtoms = md_trajectory_num_atoms(backingTraj);

        md_trajectory_get_header(backingTraj, &mem->header);
        md_array_resize(mem->frameHeaders, numFrames, alloc);
        md_array_resize(mem->x, numAtoms * numFrames, alloc);
        md_array_resize(mem->y, numAtoms * numFrames, alloc);
        md_array_resize(mem->z, numAtoms * numFrames, alloc);

        if (mol && mol->backbone.count) {
            md_array_resize(
                mem->secondaryStructures,
                mol->backbone.count * numFrames,
                alloc
            );
            mem->secondaryStructureStride = mol->backbone.count;
            for (int64_t i = 0; i < md_array_size(mem->secondaryStructures); ++i) {
                mem->secondaryStructures[i] = MD_SECONDARY_STRUCTURE_COIL;
            }
        }

        for (int64_t i = 0; i < numFrames; i++) {
            float* x = &mem->x[i * numAtoms];
            float* y = &mem->y[i * numAtoms];
            float* z = &mem->z[i * numAtoms];
            md_trajectory_load_frame(backingTraj, i, &mem->frameHeaders[i], x, y, z);
            if (deperiodizeOnLoad && mol) {
                const md_unit_cell_t* cell = &mem->frameHeaders[i].cell;
                md_util_deperiodize_system(x, y, z, cell, mol);
            }
        }

        mem->magic = MdMemTrajMagic;
        mem->alloc = alloc;

        return traj;
    }

    void memTrajectoryDestroy(md_trajectory_i* traj) {
        ghoul_assert(traj, "No trajectory");

        md_mem_trajectory_t* mem = reinterpret_cast<md_mem_trajectory_t*>(traj->inst);
        md_array_free(mem->x, mem->alloc);
        md_array_free(mem->y, mem->alloc);
        md_array_free(mem->z, mem->alloc);
        md_array_free(mem->frameHeaders, mem->alloc);
        md_free(mem->alloc, traj, sizeof(md_trajectory_i) + sizeof(md_mem_trajectory_t));
        std::memset(traj, 0, sizeof(md_trajectory_i));
    }

    bool loadCacheFrameData(md_frame_data_t* frameData, const md_trajectory_i* traj,
                            int64_t frame, const md_molecule_t* mol,
                            bool deperiodizeOnLoad)
    {
        const int64_t frameDataSize = md_trajectory_fetch_frame_data(traj, frame, 0);
        void* frameDataPtr = md_alloc(default_allocator, frameDataSize);
        md_trajectory_fetch_frame_data(traj, frame, frameDataPtr);
        bool result = md_trajectory_decode_frame_data(
            traj,
            frameDataPtr,
            frameDataSize,
            &frameData->header,
            frameData->x,
            frameData->y,
            frameData->z
        );

        if (result) {
            const md_unit_cell_t* cell = &frameData->header.cell;
            const bool have_cell = cell->flags != 0;

            if (deperiodizeOnLoad && have_cell) {
                md_util_deperiodize_system(
                    frameData->x,
                    frameData->y,
                    frameData->z,
                    cell,
                    mol
                );
            }
        }

        md_free(default_allocator, frameDataPtr, frameDataSize);
        return result;
    }

    bool cachedTrajectoryGetHeader(md_trajectory_o* inst, md_trajectory_header_t* header)
    {
        md_cached_trajectory_t* cached = reinterpret_cast<md_cached_trajectory_t*>(inst);
        std::memcpy(header, &cached->header, sizeof(md_trajectory_header_t));
        return true;
    }

    int64_t cachedTrajectoryFetchFrame(md_trajectory_o*, int64_t idx, void* data) {
        if (data) {
            *(reinterpret_cast<int64_t*>(data)) = idx;
        }
        return sizeof(int64_t);
    }

    bool cachedTrajectoryDecodeFrame(md_trajectory_o* inst, const void* data,
                                     [[maybe_unused]] int64_t dataSize,
                                     md_trajectory_frame_header_t* header, float* outX,
                                     float* outY, float* outZ)
    {
        ghoul_assert(inst, "No instance");
        ghoul_assert(dataSize == sizeof(int64_t), "Invalid data size");

        md_cached_trajectory_t* cached = reinterpret_cast<md_cached_trajectory_t*>(inst);

        int64_t idx = *(reinterpret_cast<const int64_t*>(data));
        ghoul_assert(
            idx >= 0 && idx < md_trajectory_num_frames(cached->cache.traj),
            "Invalid index"
        );

        md_frame_data_t* frameData;
        md_frame_cache_lock_t* lock = 0;
        bool result = true;
        bool inCache = md_frame_cache_find_or_reserve(
            &cached->cache,
            idx,
            &frameData,
            &lock
        );
        if (!inCache) {
            result = loadCacheFrameData(
                frameData,
                cached->cache.traj,
                idx,
                cached->mol,
                cached->deperiodizeOnLoad
            );
        }

        if (result) {
            const int64_t numAtoms = frameData->header.num_atoms;
            if (header) {
                *header = frameData->header;
            }
            if (outX) {
                std::memcpy(outX, frameData->x, numAtoms * sizeof(float));
            }
            if (outY) {
                std::memcpy(outY, frameData->y, numAtoms * sizeof(float));
            }
            if (outZ) {
                std::memcpy(outZ, frameData->z, numAtoms * sizeof(float));
            }
        }

        if (lock) {
            md_frame_cache_frame_lock_release(lock);
        }

        return result;
    }

    bool cachedTrajectoryLoadFrame(md_trajectory_o* inst, int64_t idx,
                                   md_trajectory_frame_header_t* header, float* x,
                                   float* y, float* z)
    {
        return cachedTrajectoryDecodeFrame(inst, &idx, sizeof(int64_t), header, x, y, z);
    }

    md_trajectory_i* cachedTrajectoryCreate(md_trajectory_i* backingTraj,
                                            md_allocator_i* alloc, bool deperiodizeOnLoad,
                                            const md_molecule_t* mol)
    {
        ghoul_assert(backingTraj, "No backing trajectory");
        ghoul_assert(alloc, "No allocator");

        void* data = md_alloc(
            alloc,
            sizeof(md_trajectory_i) + sizeof(md_cached_trajectory_t)
        );
        std::memset(data, 0, sizeof(md_trajectory_i) + sizeof(md_cached_trajectory_t));
        md_trajectory_i* traj = reinterpret_cast<md_trajectory_i*>(data);
        md_cached_trajectory_t* cachedTraj =
            reinterpret_cast<md_cached_trajectory_t*>(traj + 1);

        const int64_t numFrames = md_trajectory_num_frames(backingTraj);
        const int64_t numCacheFrames = std::min(32LL, numFrames);

        cachedTraj->magic = MdCachedTrajMagic;
        cachedTraj->deperiodizeOnLoad = deperiodizeOnLoad;
        cachedTraj->mol = mol;
        if (mol && mol->backbone.count) {
            md_array_resize(cachedTraj->secondaryStructures, numFrames * mol->backbone.count, alloc);
            cachedTraj->secondaryStructureStride = mol->backbone.count;
            for (int64_t i = 0; i < md_array_size(cachedTraj->secondaryStructures); i++) {
                cachedTraj->secondaryStructures[i] = MD_SECONDARY_STRUCTURE_COIL;
            }
        }
        md_trajectory_get_header(backingTraj, &cachedTraj->header);
        md_frame_cache_init(&cachedTraj->cache, backingTraj, alloc, numCacheFrames);

        traj->inst = reinterpret_cast<md_trajectory_o*>(cachedTraj);
        traj->get_header = cachedTrajectoryGetHeader;
        traj->fetch_frame_data = cachedTrajectoryFetchFrame;
        traj->decode_frame_data = cachedTrajectoryDecodeFrame;
        traj->load_frame = cachedTrajectoryLoadFrame;

        return traj;
    }

    void loadSecondaryStructureData(md_trajectory_i* traj, const md_molecule_t* mol) {
        ghoul_assert(traj, "Missing trajectory");
        ghoul_assert(traj->inst, "Trajectory has not data");
        ghoul_assert(mol, "Missing molecular data");

        using namespace openspace;

        if (mol->backbone.range_count) {
            uint64_t ssStride = mol->backbone.count;
            md_secondary_structure_t* ssData = nullptr;

            const uint64_t magic = *reinterpret_cast<uint64_t*>(traj->inst);
            switch (magic) {
                case MdCachedTrajMagic:
                {
                    md_cached_trajectory_t* t =
                        reinterpret_cast<md_cached_trajectory_t*>(traj->inst);
                    ssData = t->secondaryStructures;
                    break;
                }
                case MdMemTrajMagic:
                {
                    md_mem_trajectory_t* t =
                        reinterpret_cast<md_mem_trajectory_t*>(traj->inst);
                    ssData = t->secondaryStructures;
                    break;
                }
            }

            ThreadPool& pool =
                global::moduleEngine->module<MoleculeModule>()->threadPool();

            const int64_t numChunks = 8;
            const int64_t numFrames = md_trajectory_num_frames(traj);
            for (int64_t i = 0; i < numChunks; i++) {
                const int64_t beg = i * numFrames / numChunks;
                const int64_t end =
                    (i == numChunks - 1) ? numFrames : (i + 1) * numFrames / numChunks;
                pool.enqueue([ssData, ssStride, traj, old_mol = mol, beg, end]() {
                    // @TODO(Robin) This is stupid and the interface to compute secondary
                    // structure should be changed
                    md_molecule_t mol = *old_mol;
                    const int64_t stride = ALIGN_TO(mol.atom.count, 16);
                    const int64_t bytes = stride * sizeof(float) * 3;
                    float* coords = reinterpret_cast<float*>(
                        md_alloc(default_allocator, bytes)
                    );
                    defer {
                        md_free(default_allocator, coords, bytes);
                    };

                    // Overwrite the coordinate section, since we will load trajectory
                    // frame data into these
                    mol.atom.x = coords + stride * 0;
                    mol.atom.y = coords + stride * 1;
                    mol.atom.z = coords + stride * 2;

                    for (int64_t i = beg; i < end; i++) {
                        md_trajectory_load_frame(
                            traj,
                            i,
                            nullptr,
                            mol.atom.x,
                            mol.atom.y,
                            mol.atom.z
                        );
                        md_util_backbone_secondary_structure_compute(
                            ssData + ssStride * i,
                            ssStride,
                            &mol
                        );
                    }
                });
            }
        }
    }

    std::span<const md_secondary_structure_t> getSecondaryStructureFrameData(
                                                              const md_trajectory_i* traj,
                                                                            int64_t frame)
    {
        if (frame >= 0 && frame < md_trajectory_num_frames(traj)) {
            auto it = ssTable.find(traj);
            if (it != ssTable.end()) {
                const SecondaryStructureData& data = it->second;
                const uint64_t ssStride = data.stride;
                const md_secondary_structure_t* ssSrc = data.ss + frame * ssStride;
                return { ssSrc, ssStride };
            }
        }

        return {};
    }
} // namespace

FrameData::~FrameData() {
    if (lock) {
        md_frame_cache_frame_lock_release(lock);
    }
}

namespace mol {

const md_molecule_t* loadMolecule(std::filesystem::path file, bool isCoarseGrained) {
    ghoul_assert(!file.empty(), "No file provided");

    const uint64_t hash = molHash(file, isCoarseGrained);
    auto entry = molecules.find(hash);
    if (entry != molecules.end()) {
        return &entry->second;
    }

    md_molecule_api* api = load::mol::api(file);
    if (!api) {
        throw ghoul::RuntimeError(
            "Failed to find appropriate molecule loader api",
            "MOLD"
        );
    }

    md_molecule_t mol = {};
    std::string f = file.string();
    str_t str = { f.data(), static_cast<int64_t>(f.length()) };
    if (!api->init_from_file(&mol, str, default_allocator)) {
        throw ghoul::RuntimeError("Failed to load molecule", "MOLD");
    }

    const md_util_postprocess_flags_t flags =
        isCoarseGrained ? MD_UTIL_POSTPROCESS_COARSE_GRAINED : MD_UTIL_POSTPROCESS_ALL;
    md_util_postprocess_molecule(&mol, default_allocator, flags);

    return &(molecules[hash] = mol);
}

const md_trajectory_i* loadTrajectory(std::filesystem::path file,
                                      const md_molecule_t* mol, bool deperiodizeOnLoad)
{
    ghoul_assert(!file.empty(), "No file provided");

    const size_t hash = trajHash(file, deperiodizeOnLoad);
    auto entry = trajectories.find(hash);
    if (entry != trajectories.end()) {
        return entry->second;
    }

    md_trajectory_api* api = load::traj::api(file);
    if (!api) {
        throw ghoul::RuntimeError(
            "Failed to find appropriate trajectory loader api",
            "MOLD"
        );
    }

    std::string f = file.string();
    str_t str = { f.data(), static_cast<int64_t>(f.length()) };
    md_trajectory_i* traj = api->create(str, default_allocator);
    if (!traj) {
        throw ghoul::RuntimeError("Failed to load trajectory", "MOLD");
    }

    if (deperiodizeOnLoad && !mol) {
        throw ghoul::RuntimeError(
            "Deperiodize trajectory was set, but no valid was molecule provided",
            "MOLD"
        );
    }

    const int64_t numFrames = md_trajectory_num_frames(traj);
    const int64_t numAtoms = md_trajectory_num_atoms(traj);
    const int64_t estTrajSize = numFrames * numAtoms * 3 * sizeof(float);

    if (estTrajSize < MEGABYTES(64)) {
        md_trajectory_i* memTraj = memTrajectoryCreate(
            traj,
            default_allocator,
            deperiodizeOnLoad,
            mol
        );
        md_trajectory_i* oldTraj = traj;
        traj = memTraj;
        api->destroy(oldTraj);
    }
    else {
        md_trajectory_i* cachedTraj = cachedTrajectoryCreate(
            traj,
            default_allocator,
            deperiodizeOnLoad,
            mol
        );
        traj = cachedTraj;
    }

    if (traj && mol) {
        loadSecondaryStructureData(traj, mol);
    }

    trajectories[hash] = traj;
    return traj;
}

void prefetchFrames(const md_trajectory_i* traj, std::span<int64_t> frames) {
    ghoul_assert(traj, "No trajectory provided");

    using namespace openspace;
    
    if (!traj->inst) {
        return;
    }

    md_cached_trajectory_t* cachedTraj =
        reinterpret_cast<md_cached_trajectory_t*>(traj->inst);
    if (cachedTraj->magic == MdCachedTrajMagic) {
        ThreadPool& pool = global::moduleEngine->module<MoleculeModule>()->threadPool();
            
        const int64_t numFrames = md_trajectory_num_frames(traj);
        for (int64_t i : frames) {
            if (i < 0 || numFrames <= i) {
                continue;
            }
            pool.enqueue([traj, i](){
                md_trajectory_load_frame(traj, i, nullptr, nullptr, nullptr, nullptr);
            });
        }
    }
}

FrameData frameData(const md_trajectory_i* traj, int64_t frame) {
    FrameData data = {};
    if (!traj || !traj->inst) {
        LERROR("Invalid trajectory");
        return data;
    }
    
    if (frame < 0 || frame >= md_trajectory_num_frames(traj)) {
        LERROR(std::format(
            "Invalid trajectory index: {}, valid range: [0,{}]",
            frame, md_trajectory_num_frames(traj)
        ));
        return data;
    }

    uint64_t magic = *reinterpret_cast<const uint64_t*>(traj->inst);
    switch (magic) {
        case MdMemTrajMagic:
        {
            const md_mem_trajectory_t* inst =
                reinterpret_cast<const md_mem_trajectory_t*>(traj->inst);
            const size_t stride = inst->header.num_atoms;
            data.header = inst->frameHeaders + frame;
            data.x = { inst->x + frame * stride, stride };
            data.y = { inst->y + frame * stride, stride };
            data.z = { inst->z + frame * stride, stride };
            data.ss = {
                inst->secondaryStructures + frame * inst->secondaryStructureStride,
                inst->secondaryStructureStride
            };
            break;
        }
        case MdCachedTrajMagic:
        {
            md_cached_trajectory_t* inst =
                reinterpret_cast<md_cached_trajectory_t*>(traj->inst);
            md_frame_data_t* frameData;
            md_frame_cache_lock_t* lock = nullptr;
            bool result = true;
            bool inCache = md_frame_cache_find_or_reserve(
                &inst->cache,
                frame,
                &frameData,
                &lock
            );
            if (!inCache) {
                result = loadCacheFrameData(
                    frameData,
                    inst->cache.traj,
                    frame,
                    inst->mol,
                    inst->deperiodizeOnLoad
                );
            }
            if (result) {
                data.header = &frameData->header;
                data.x = {
                    frameData->x,
                    static_cast<size_t>(frameData->header.num_atoms)
                };
                data.y = {
                    frameData->y,
                    static_cast<size_t>(frameData->header.num_atoms)
                };
                data.z = {
                    frameData->z,
                    static_cast<size_t>(frameData->header.num_atoms)
                };
                data.ss = {
                    inst->secondaryStructures + frame * inst->secondaryStructureStride,
                    inst->secondaryStructureStride
                };
                data.lock = lock;
            }
            break;
        }
        default:
            LERROR("Invalid trajectory");
    }

    return data;
}

} // namespace mol
