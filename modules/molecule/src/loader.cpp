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

#include <modules/molecule/src/loader.h>

#include <ghoul/logging/logmanager.h>
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
#include <cstring>
#include <string>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "MOLD";

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
        md_bitfield_t recenterTarget;
        bool deperiodize;
    };

    LoadedMolecule loadedMolecules[8] = {};
    int64_t numLoadedMolecules = 0;

    LoadedTrajectory loadedTrajectories[8] = {};
    int64_t numLoadedTrajectories = 0;

    LoadedTrajectory* findLoadedTrajectory(uint64_t key) {
        for (int64_t i = 0; i < numLoadedTrajectories; ++i) {
            if (loadedTrajectories[i].key == key) {
                return &loadedTrajectories[i];
            }
        }
        return nullptr;
    }

    LoadedTrajectory* allocLoadedTrajectory(uint64_t key) {
        ghoul_assert(findLoadedTrajectory(key) != nullptr, "Trajectory loaded");
        ghoul_assert(
            numLoadedTrajectories < static_cast<int64_t>(ARRAY_SIZE(loadedTrajectories)),
            "Loaded too many trajectories"
        );
        LoadedTrajectory* traj = &loadedTrajectories[numLoadedTrajectories++];
        *traj = { 0 }; // Clear
        traj->key = key;
        return traj;
    }

    void removeLoadedTrajectory(uint64_t key) {
        for (int64_t i = 0; i < numLoadedTrajectories; ++i) {
            if (loadedTrajectories[i].key == key) {
                md_frame_cache_free(&loadedTrajectories[i].cache);
                loadedTrajectories[i].api->destroy(loadedTrajectories[i].traj);
                // Swap back and pop
                loadedTrajectories[i] = loadedTrajectories[--numLoadedTrajectories];
                return;
            }
        }
        ghoul_assert(false, "Did not find trajectory");
    }

    bool getHeader(struct md_trajectory_o* inst, md_trajectory_header_t* header) {
        LoadedTrajectory* loadedTraj = reinterpret_cast<LoadedTrajectory*>(inst);
        return md_trajectory_get_header(loadedTraj->traj, header);
    }

    int64_t fetchFrameData(struct md_trajectory_o*, int64_t idx, void* data) {
        if (data) {
            *(reinterpret_cast<int64_t*>(data)) = idx;
        }
        return sizeof(int64_t);
    }

    bool decodeFrameData(struct md_trajectory_o* inst, const void* data,
                         [[maybe_unused]] int64_t dataSize,
                         md_trajectory_frame_header_t* header, float* outX, float* outY,
                         float* outZ)
    {
        LoadedTrajectory* trajectory = reinterpret_cast<LoadedTrajectory*>(inst);
        ghoul_assert(trajectory, "Could not find trajectory");
        ghoul_assert(dataSize == sizeof(int64_t), "Wrong data size");

        int64_t idx = *(reinterpret_cast<const int64_t*>(data));
        ghoul_assert(
            0 <= idx && idx < md_trajectory_num_frames(trajectory->traj),
            "Invalid index"
        );

        md_frame_data_t* frameData = nullptr;
        md_frame_cache_lock_t* lock = nullptr;
        const bool inCache = md_frame_cache_find_or_reserve(
            &trajectory->cache,
            idx,
            &frameData,
            &lock
        );
        defer {
            if (lock) {
                md_frame_cache_frame_lock_release(lock);
            };
        };

        if (!inCache) {
            const int64_t frameDataSize = md_trajectory_fetch_frame_data(
                trajectory->traj,
                idx,
                0
            );
            void* frameDataPtr = md_alloc(default_allocator, frameDataSize);
            defer {
                md_free(default_allocator, frameDataPtr, frameDataSize);
            };

            md_trajectory_fetch_frame_data(trajectory->traj, idx, frameDataPtr);
            bool result = md_trajectory_decode_frame_data(
                trajectory->traj,
                frameDataPtr,
                frameDataSize,
                &frameData->header,
                frameData->x,
                frameData->y,
                frameData->z
            );

            if (!result) {
                return false;
            }

            const md_unit_cell_t* cell = &frameData->header.cell;
            const bool haveCell = cell->flags != 0;

            const md_molecule_t* mol = trajectory->mol;
            float* x = frameData->x;
            float* y = frameData->y;
            float* z = frameData->z;
            const int64_t numAtoms = frameData->header.num_atoms;

            // If we have a recenter target, then compute the center of mass and
            // apply that transformation
            if (!md_bitfield_empty(&trajectory->recenterTarget)) {
                const md_bitfield_t* bf = &trajectory->recenterTarget;
                const int64_t count = md_bitfield_popcount(bf);

                if (count > 0) {
                    int32_t* indices = reinterpret_cast<int32_t*>(
                        md_alloc(default_allocator, sizeof(int32_t) * count)
                    );
                    defer {
                        md_free(default_allocator, indices, sizeof(int32_t) * count);
                    };

                    md_bitfield_extract_indices(indices, bf);

                    const vec3_t boxExt =
                        mat3_mul_vec3(header->cell.basis, vec3_set1(1.f));

                    const vec3_t com = haveCell ?
                        vec3_deperiodize(
                            md_util_compute_com_indexed_soa_ortho(
                                x,
                                y,
                                z,
                                mol->atom.mass,
                                indices,
                                count,
                                boxExt
                            ),
                            boxExt * 0.5f,
                            boxExt
                        ) :
                        md_util_compute_com_indexed_soa(
                            x,
                            y,
                            z,
                            mol->atom.mass,
                            indices,
                            count
                        );

                    // Translate all
                    const vec3_t trans = haveCell ? boxExt * 0.5f - com : -com;
                    for (int64_t i = 0; i < numAtoms; i++) {
                        x[i] += trans.x;
                        y[i] += trans.y;
                        z[i] += trans.z;
                    }
                }
            }

            if (trajectory->deperiodize && haveCell) {
                md_util_deperiodize_system(x, y, z, cell, mol);
            }
        }

        const int64_t num_atoms = frameData->header.num_atoms;
        if (header) {
            *header = frameData->header;
        }
        if (outX) {
            std::memcpy(outX, frameData->x, sizeof(float) * num_atoms);
        }
        if (outY) {
            std::memcpy(outY, frameData->y, sizeof(float) * num_atoms);
        }
        if (outZ) {
            std::memcpy(outZ, frameData->z, sizeof(float) * num_atoms);
        }

        return true;
    }

    bool loadFrame(md_trajectory_o* inst, int64_t idx,
                   md_trajectory_frame_header_t* header, float* x, float* y, float* z)
    {
        return decodeFrameData(inst, &idx, sizeof(int64_t), header, x, y, z);
    }
} // namespace

namespace load::mol {

md_molecule_api* api(std::filesystem::path filename) {
    std::filesystem::path ext = filename.extension();
    if (ext == ".pdb") {
        return md_pdb_molecule_api();
    }
    else if (ext == ".gro") {
        return md_gro_molecule_api();
    }
    else if (ext == ".xyz") {
        return md_xyz_molecule_api();
    }
    else if (ext == ".xmol") {
        return md_xyz_molecule_api();
    }
    else if (ext == ".arc") {
        return md_xyz_molecule_api();
    }
    else {
        return nullptr;
    }
}

}  // namespace load::mol

namespace load::traj {

md_trajectory_api* api(std::filesystem::path filename) {
    std::filesystem::path ext = filename.extension();
    if (ext == ".pdb") {
        return md_pdb_trajectory_api();
    }
    else if (ext == ".xtc") {
        return md_xtc_trajectory_api();
    }
    else if (ext == ".trr") {
        return md_trr_trajectory_api();
    }
    else if (ext == ".xyz") {
        return md_xyz_trajectory_api();
    }
    else if (ext == ".xmol") {
        return md_xyz_trajectory_api();
    }
    else if (ext == ".arc") {
        return md_xyz_trajectory_api();
    }
    else {
        return nullptr;
    }
}

md_trajectory_i* openFile(std::filesystem::path filename, const md_molecule_t* mol,
                          md_allocator_i* alloc, bool deperiodizeOnLoad)
{
    ghoul_assert(alloc, "No allocator provided");
    ghoul_assert(mol, "No molecule provided");

    md_trajectory_api* api = traj::api(filename);
    if (!api) {
        LERROR(std::format("Unsupported file extension: '{}'", filename));
        return nullptr;
    }

    std::string f = filename.string();
    md_trajectory_i* internalTraj = api->create(str_from_cstr(f.c_str()), alloc);
    if (!internalTraj) {
        return nullptr;
    }

    md_trajectory_i* traj = reinterpret_cast<md_trajectory_i*>(
        md_alloc(alloc, sizeof(md_trajectory_i))
    );
    std::memset(traj, 0, sizeof(md_trajectory_i));

    LoadedTrajectory* inst = allocLoadedTrajectory(reinterpret_cast<uint64_t>(traj));
    inst->mol = mol;
    inst->api = api;
    inst->traj = internalTraj;
    inst->cache = { 0 };
    inst->recenterTarget = { 0 };
    inst->alloc = alloc;
    inst->deperiodize = deperiodizeOnLoad;
    
    const uint64_t numAtoms = md_trajectory_num_atoms(internalTraj);
    const uint64_t numTrajFrames = md_trajectory_num_frames(internalTraj);
    const uint64_t frameCacheSize = MEGABYTES(16);
    const uint64_t approxFrameSize = numAtoms * 3 * sizeof(float);
    const uint64_t maxNumCacheFrames = std::max(16ULL, frameCacheSize / approxFrameSize);

    const int64_t numCacheFrames = std::min(numTrajFrames, maxNumCacheFrames);
    
    LERROR(std::format("Initializing frame cache with {} frames.", numCacheFrames));
    md_frame_cache_init(&inst->cache, inst->traj, alloc, numCacheFrames);
    md_bitfield_init(&inst->recenterTarget, alloc);

    // We only overload load frame and decode frame data to apply PBC upon loading data
    traj->inst = reinterpret_cast<md_trajectory_o*>(inst);
    traj->get_header = getHeader;
    traj->load_frame = loadFrame;
    traj->fetch_frame_data = fetchFrameData;
    traj->decode_frame_data = decodeFrameData;
    
    return traj;
}

bool close(md_trajectory_i* traj) {
    ghoul_assert(traj, "No trajectory provided");

    LoadedTrajectory* loadedTraj = findLoadedTrajectory(reinterpret_cast<uint64_t>(traj));
    if (loadedTraj) {
        removeLoadedTrajectory(loadedTraj->key);
        std::memset(traj, 0, sizeof(md_trajectory_i));
        return true;
    }
    throw ghoul::RuntimeError(
        "Attempting to free trajectory which was not loaded with loader"
    );
}

bool setRecenterTarget(md_trajectory_i* traj, const md_bitfield_t* atomMask) {
    ghoul_assert(traj, "No trajectory provided");

    LoadedTrajectory* loadedTraj = findLoadedTrajectory(reinterpret_cast<uint64_t>(traj));
    if (loadedTraj) {
        if (atomMask) {
            md_bitfield_copy(&loadedTraj->recenterTarget, atomMask);
        }
        else {
            md_bitfield_clear(&loadedTraj->recenterTarget);
        }
        return true;
    }
    LERROR("Supplied trajectory was not loaded with loader");
    return false;
}

bool clearCache(md_trajectory_i* traj) {
    ghoul_assert(traj, "No trajectory provided");

    LoadedTrajectory* loadedTraj = findLoadedTrajectory(reinterpret_cast<uint64_t>(traj));
    if (loadedTraj) {
        md_frame_cache_clear(&loadedTraj->cache);
        return true;
    }
    LERROR("Supplied trajectory was not loaded with loader");
    return false;
}

int64_t numCacheFrames(md_trajectory_i* traj) {
    ghoul_assert(traj, "No trajectory provided");

    LoadedTrajectory* loadedTraj = findLoadedTrajectory(reinterpret_cast<uint64_t>(traj));
    if (loadedTraj) {
        return md_frame_cache_num_frames(&loadedTraj->cache);
    }
    LERROR("Supplied trajectory was not loaded with loader");
    return 0;
}

}  // namespace load::traj
