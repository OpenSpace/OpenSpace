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

#include <md_types.h>
#include <span>
#include <string_view>

struct md_molecule_t;
struct md_trajectory_i;
struct md_trajectory_frame_header_t;
struct md_frame_cache_lock_t;

// @NOTE(Robin) If you want to go full blown cplusplus bananas mode, you can use smart
// pointers (shared external + weak internal) to keep some type of reference counting
// going and free resources when they are not held by any external pointers anymore. I'm
// not going to care about that right now.

struct FrameData {
    ~FrameData();

    const md_trajectory_frame_header_t* header;
    std::span<float> x;
    std::span<float> y;
    std::span<float> z;
    std::span<const md_secondary_structure_t> ss;
    md_frame_cache_lock_t* lock = nullptr;
};

namespace mol {

const md_molecule_t* loadMolecule(std::string_view file, bool isCoarseGrained = false);
const md_trajectory_i* loadTrajectory(std::string_view file,
    const md_molecule_t* mol = nullptr, bool deperiodizeOnLoad = false);
    
// This will kick of work into worker threads and thus not stall the calling thread
void prefetchFrames(const md_trajectory_i* traj, std::span<int64_t> frames);
    
// FrameCoords also holds an optional lock for the frame if it is a cached trajectory.
// This is released upon destruction of the object. So don't attempt to store and
// access its x,y,z data after the object has been destroyed
FrameData frameData(const md_trajectory_i* traj, int64_t frame);

} // namespace mol

#endif // __OPENSPACE_MODULE_MOLECULE___CACHE___H__
