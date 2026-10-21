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

#ifndef __OPENSPACE_MODULE_MOLECULE___CACHE___H__
#define __OPENSPACE_MODULE_MOLECULE___CACHE___H__

#include <core/md_vec_math.h>
#include <md_system.h>
#include <md_unitcell.h>
#include <filesystem>
#include <span>
#include <vector>

namespace molecule {

struct Molecule {
    md_system_t system = {};
    md_system_state_t state = {};
};

struct Trajectory;

struct FrameData {
    md_unitcell_t unitcell = {};
    std::span<const vec3_t> xyz;
    std::span<const md_secondary_structure_t> ss;
};

const Molecule* loadMolecule(std::filesystem::path file, bool isCoarseGrained = false);

const Trajectory* loadTrajectory(std::filesystem::path file,
    const Molecule* mol = nullptr, bool deperiodizeOnLoad = false);

int64_t trajectoryNumFrames(const Trajectory* traj);
int64_t trajectoryNumAtoms(const Trajectory* traj);
const md_system_t* trajectorySystem(const Trajectory* traj);

void prefetchFrames(const Trajectory* traj, std::span<const int64_t> frames);

FrameData frameData(const Trajectory* traj, int64_t frame);

} // namespace molecule

#endif // __OPENSPACE_MODULE_MOLECULE___CACHE___H__
