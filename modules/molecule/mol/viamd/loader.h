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

#ifndef __OPENSPACE_MODULE_MOLECULE___LOADER___H__
#define __OPENSPACE_MODULE_MOLECULE___LOADER___H__

#include <core/md_str.h>

struct md_allocator_i;
struct md_molecule_t;
struct md_molecule_api;
struct md_trajectory_i;
struct md_trajectory_api;
struct md_bitfield_t;

namespace load::mol {
    md_molecule_api* api(str_t filename);
} // namespace load::mol

namespace load::traj {
    md_trajectory_api* api(str_t filename);

    md_trajectory_i* openFile(str_t filename, const md_molecule_t* mol,
        md_allocator_i* alloc, bool deperiodize_on_load);
    bool close(md_trajectory_i* traj);

    bool setRecenterTarget(md_trajectory_i* traj, const md_bitfield_t* atom_mask);
    bool clearCache(md_trajectory_i* traj);
    int64_t numCacheFrames(md_trajectory_i* traj);
} // namespace load::traj

#endif // __OPENSPACE_MODULE_MOLECULE___LOADER___H__
