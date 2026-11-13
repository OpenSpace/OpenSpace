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
#include <openspace/logging/logmanager.h>
#include <openspace/misc/assert.h>
#include <openspace/util/threadpool.h>
#include <core/md_allocator.h>
#include <core/md_arena_allocator.h>
#include <md_system.h>
#include <md_util.h>
#include <md_dssp.h>
#include <algorithm>
#include <array>
#include <format>
#include <mutex>
#include <string>
#include <string_view>
#include <unordered_map>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "MoleculeManager";

    size_t molHash(const std::filesystem::path& file, bool isCoarseGrained) {
        size_t hash = std::hash<std::filesystem::path>{}(file);
        return (hash << 1) | static_cast<size_t>(isCoarseGrained);
    }

    uint64_t trajHash(const std::filesystem::path& file, bool deperiodize) {
        size_t hash = std::hash<std::filesystem::path>{}(file);
        return (hash << 1) | static_cast<size_t>(deperiodize);
    }
} // namespace

namespace molecule {

struct Trajectory {
    const md_system_t* system = nullptr;
    md_system_t standaloneSystem = {};
    std::string runName;
    size_t numFrames = 0;
    size_t numAtoms = 0;
    size_t atomStride = 0;
    bool deperiodizeOnLoad = false;

    std::vector<md_secondary_structure_t> secondaryStructures;
    size_t ssStride = 0;

    bool isMemoryTrajectory = false;
    std::vector<vec3_t> memCoordinates;
    std::vector<md_unitcell_t> memUnitcells;

    struct Slot {
        int64_t frame = -1;
        md_unitcell_t unitcell = {};
        std::vector<vec3_t> xyz;
        std::vector<md_secondary_structure_t> ss;
        uint64_t accessCount = 0;
    };
    mutable std::mutex cacheMutex;
    mutable std::array<Slot, 64> cacheSlots;
    mutable uint64_t currentAccess = 0;

    ~Trajectory() {
        if (standaloneSystem.alloc) {
            md_system_free(&standaloneSystem);
        }
    }
};

namespace {
    std::unordered_map<size_t, std::unique_ptr<Molecule>> molecules;
    std::unordered_map<uint64_t, std::unique_ptr<Trajectory>> trajectories;
} // namespace

const Molecule* loadMolecule(std::filesystem::path file, bool isCoarseGrained) {
    assert_msg(!file.empty(), "No file provided");

    const size_t hash = molHash(file, isCoarseGrained);
    auto entry = molecules.find(hash);
    if (entry != molecules.end()) {
        return entry->second.get();
    }

    auto mol = std::make_unique<Molecule>();
    mol->system.alloc = md_get_heap_allocator();
    mol->state.alloc = md_get_heap_allocator();

    if (!loadSystemFromFile(&mol->system, &mol->state, file)) {
        throw RuntimeError(
            std::format("Failed to load molecule from file '{}'", file.string()),
            "MOLD"
        );
    }

    const md_infer_flags_t flags = isCoarseGrained ?
        static_cast<md_infer_flags_t>(MD_UTIL_INFER_ALL & ~MD_UTIL_INFER_CHEMISTRY_BIT) :
        MD_UTIL_INFER_ALL;
    md_util_system_infer(&mol->system, &mol->state, flags);

    const Molecule* ptr = mol.get();
    molecules[hash] = std::move(mol);
    return ptr;
}

const Trajectory* loadTrajectory(std::filesystem::path file,
                                 const Molecule* mol, bool deperiodizeOnLoad)
{
    assert_msg(!file.empty(), "No file provided");

    const uint64_t hash = trajHash(file, deperiodizeOnLoad);
    auto entry = trajectories.find(hash);
    if (entry != trajectories.end()) {
        return entry->second.get();
    }

    auto traj = std::make_unique<Trajectory>();
    if (mol) {
        traj->system = &mol->system;
    }
    else {
        traj->standaloneSystem.alloc = md_get_heap_allocator();
        traj->system = &traj->standaloneSystem;
    }

    traj->runName = std::format("run_{}", hash);
    str_t run = { traj->runName.data(), traj->runName.length() };

    if (!publishTrajectoryRun(const_cast<md_system_t*>(traj->system), file, run)) {
        throw RuntimeError(
            std::format("Failed to load trajectory from file '{}'", file.string()),
            "MOLD"
        );
    }

    if (deperiodizeOnLoad && !mol) {
        throw RuntimeError(
            "Deperiodize trajectory was set, but no valid molecule was provided",
            "MOLD"
        );
    }

    char buf[512];
    const md_attribute_t* timeAttr = md_attributes_find(
        &traj->system->attributes,
        md_run_path(buf, sizeof(buf), run, STR_LIT("time"))
    );
    traj->numFrames = timeAttr ? timeAttr->format.shape[0] : 0;

    const md_attribute_t* posAttr = md_attributes_find(
        &traj->system->attributes,
        md_run_path(buf, sizeof(buf), run, STR_LIT("atom/position"))
    );
    traj->numAtoms = posAttr ? posAttr->format.shape[1] : 0;
    traj->atomStride = ALIGN_TO(traj->numAtoms, 16);
    traj->deperiodizeOnLoad = deperiodizeOnLoad;

    if (traj->system->protein_backbone.segment.count > 0) {
        traj->ssStride = traj->system->protein_backbone.segment.count;
        traj->secondaryStructures.resize(
            traj->numFrames * traj->ssStride,
            MD_SECONDARY_STRUCTURE_COIL
        );
    }

    const size_t estTrajSize = traj->numFrames * traj->atomStride * sizeof(vec3_t);
    constexpr size_t MemLimit = 128 * 1024 * 1024; // 128 MB

    if (estTrajSize <= MemLimit) {
        traj->isMemoryTrajectory = true;
        traj->memCoordinates.resize(traj->numFrames * traj->atomStride);
        traj->memUnitcells.resize(traj->numFrames);

        const str_t paths[] = { STR_INIT("atom/position"), STR_INIT("unitcell") };
        md_system_extract_t* ex = md_system_extract_begin(
            traj->system,
            run,
            paths,
            2,
            md_get_heap_allocator()
        );

        if (ex) {
            for (size_t f = 0; f < traj->numFrames; ++f) {
                vec3_t* coords = traj->memCoordinates.data() + f * traj->atomStride;
                md_system_state_t st = {
                    .num_atoms = traj->numAtoms,
                    .xyz = coords,
                    .unitcell = {},
                    .alloc = nullptr
                };

                if (md_system_extract_frame(ex, static_cast<int64_t>(f), &st)) {
                    traj->memUnitcells[f] = st.unitcell;
                    if (deperiodizeOnLoad && mol) {
                        md_util_unwrap_system(&st, &mol->system);
                    }
                }
            }
            md_system_extract_end(ex);
        }

        // Secondary structure computation in parallel chunks
        if (traj->ssStride > 0 && traj->numFrames > 0) {
            ThreadPool& pool =
                global::moduleEngine->module<MoleculeModule>()->threadPool();
            const int64_t numChunks = std::min<int64_t>(8, traj->numFrames);
            for (int64_t i = 0; i < numChunks; ++i) {
                const int64_t beg = i * traj->numFrames / numChunks;
                const int64_t end = (i == numChunks - 1) ?
                    traj->numFrames :
                    (i + 1) * traj->numFrames / numChunks;

                pool.enqueue([trajPtr = traj.get(), beg, end]() {
                    for (int64_t j = beg; j < end; ++j) {
                        const vec3_t* coords =
                            trajPtr->memCoordinates.data() + j * trajPtr->atomStride;
                        const md_unitcell_t* cell = &trajPtr->memUnitcells[j];
                        md_dssp_compute(
                            trajPtr->secondaryStructures.data() + j * trajPtr->ssStride,
                            trajPtr->ssStride,
                            coords,
                            cell,
                            &trajPtr->system->protein_backbone
                        );
                    }
                });
            }
        }
    }

    const Trajectory* ptr = traj.get();
    trajectories[hash] = std::move(traj);
    return ptr;
}

int64_t trajectoryNumFrames(const Trajectory* traj) {
    return traj ? static_cast<int64_t>(traj->numFrames) : 0;
}

int64_t trajectoryNumAtoms(const Trajectory* traj) {
    return traj ? static_cast<int64_t>(traj->numAtoms) : 0;
}

const md_system_t* trajectorySystem(const Trajectory* traj) {
    return traj ? traj->system : nullptr;
}

void prefetchFrames(const Trajectory* traj, std::span<const int64_t> frames) {
    if (!traj || traj->isMemoryTrajectory) {
        return;
    }

    ThreadPool& pool = global::moduleEngine->module<MoleculeModule>()->threadPool();
    for (int64_t frame : frames) {
        if (frame < 0 || static_cast<size_t>(frame) >= traj->numFrames) {
            continue;
        }
        pool.enqueue([traj, frame]() {
            frameData(traj, frame);
        });
    }
}

FrameData frameData(const Trajectory* traj, int64_t frame) {
    FrameData data = {};
    if (!traj) {
        LERROR("Invalid trajectory");
        return data;
    }

    if (frame < 0 || static_cast<size_t>(frame) >= traj->numFrames) {
        LERROR(std::format(
            "Invalid trajectory index: {}, valid range: [0, {}]",
            frame, traj->numFrames
        ));
        return data;
    }

    if (traj->isMemoryTrajectory) {
        data.unitcell = traj->memUnitcells[frame];
        data.xyz = {
            traj->memCoordinates.data() + frame * traj->atomStride,
            traj->numAtoms
        };
        if (traj->ssStride > 0 && !traj->secondaryStructures.empty()) {
            data.ss = {
                traj->secondaryStructures.data() + frame * traj->ssStride,
                traj->ssStride
            };
        }
        return data;
    }

    std::lock_guard<std::mutex> lock(traj->cacheMutex);
    traj->currentAccess++;

    for (Trajectory::Slot& slot : traj->cacheSlots) {
        if (slot.frame == frame) {
            slot.accessCount = traj->currentAccess;
            data.unitcell = slot.unitcell;
            data.xyz = { slot.xyz.data(), traj->numAtoms };
            if (!slot.ss.empty()) {
                data.ss = { slot.ss.data(), slot.ss.size() };
            }
            return data;
        }
    }

    size_t evictIdx = 0;
    uint64_t oldest = traj->cacheSlots[0].accessCount;
    for (size_t i = 1; i < traj->cacheSlots.size(); ++i) {
        if (traj->cacheSlots[i].frame == -1) {
            evictIdx = i;
            break;
        }
        if (traj->cacheSlots[i].accessCount < oldest) {
            oldest = traj->cacheSlots[i].accessCount;
            evictIdx = i;
        }
    }

    Trajectory::Slot& target = traj->cacheSlots[evictIdx];
    target.frame = frame;
    target.accessCount = traj->currentAccess;
    if (target.xyz.size() < traj->atomStride) {
        target.xyz.resize(traj->atomStride);
    }

    str_t run = { traj->runName.data(), traj->runName.length() };
    const str_t paths[] = { STR_INIT("atom/position"), STR_INIT("unitcell") };
    md_system_extract_t* ex = md_system_extract_begin(
        traj->system,
        run,
        paths,
        2,
        md_get_heap_allocator()
    );

    if (ex) {
        md_system_state_t st = {
            .num_atoms = traj->numAtoms,
            .xyz = target.xyz.data(),
            .unitcell = {},
            .alloc = nullptr
        };
        if (md_system_extract_frame(ex, frame, &st)) {
            target.unitcell = st.unitcell;
            if (traj->deperiodizeOnLoad && traj->system) {
                md_util_unwrap_system(&st, traj->system);
            }
            if (traj->ssStride > 0) {
                target.ss.resize(traj->ssStride, MD_SECONDARY_STRUCTURE_COIL);
                md_dssp_compute(
                    target.ss.data(),
                    traj->ssStride,
                    target.xyz.data(),
                    &target.unitcell,
                    &traj->system->protein_backbone
                );
            }
        }
        md_system_extract_end(ex);
    }

    data.unitcell = target.unitcell;
    data.xyz = { target.xyz.data(), traj->numAtoms };
    if (!target.ss.empty()) {
        data.ss = { target.ss.data(), target.ss.size() };
    }
    return data;
}

} // namespace molecule
