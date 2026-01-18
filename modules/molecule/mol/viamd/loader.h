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

namespace load {
    uint32_t     get_supported_extension_count();
    const str_t* get_supported_extensions();

namespace mol {
    md_molecule_api* get_api(str_t filename);
}

namespace traj {
    md_trajectory_api* get_api(str_t filename);

    md_trajectory_i* open_file(str_t filename, const md_molecule_t* mol, md_allocator_i* alloc, bool deperiodize_on_load);
    bool close(md_trajectory_i* traj);

    bool set_recenter_target(md_trajectory_i* traj, const md_bitfield_t* atom_mask);
    bool clear_cache(md_trajectory_i* traj);
    int64_t num_cache_frames(md_trajectory_i* traj);
}

}  // namespace load

#endif // __OPENSPACE_MODULE_MOLECULE___LOADER___H__
