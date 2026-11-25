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

#include <openspace/logging/logmanager.h>
#include <core/md_str.h>
#include <md_pdb.h>
#include <md_gro.h>
#include <md_xyz.h>
#include <md_mmcif.h>
#include <md_xtc.h>
#include <md_trr.h>
#include <md_dcd.h>
#include <md_system.h>
#include <string>

namespace molecule {

bool loadSystemFromFile(md_system_t* sys, md_system_state_t* state,
                        const std::filesystem::path& file)
{
    const std::string ext = file.extension().string();
    const std::string f = file.string();
    const str_t path = { f.data(), static_cast<size_t>(f.length()) };

    if (ext == ".pdb") {
        return md_pdb_system_init_from_file(sys, state, path, MD_PDB_OPTION_NONE);
    }
    else if (ext == ".gro") {
        return md_gro_system_init_from_file(sys, state, path);
    }
    else if (ext == ".xyz" || ext == ".xmol" || ext == ".arc") {
        return md_xyz_system_init_from_file(sys, state, path, MD_XYZ_OPTION_NONE);
    }
    else if (ext == ".cif" || ext == ".mmcif") {
        return md_mmcif_system_init_from_file(sys, state, path);
    }
    return false;
}

bool publishTrajectoryRun(md_system_t* sys, const std::filesystem::path& file, str_t run)
{
    const std::string ext = file.extension().string();
    const std::string f = file.string();
    const str_t path = { f.data(), static_cast<size_t>(f.length()) };

    if (ext == ".xtc") {
        return md_xtc_system_publish_run(sys, path, run, 0);
    }
    else if (ext == ".trr") {
        return md_trr_system_publish_run(sys, path, run, 0);
    }
    else if (ext == ".dcd") {
        return md_dcd_system_publish_run(sys, path, run, 0);
    }
    else if (ext == ".xyz" || ext == ".xmol" || ext == ".arc") {
        return md_xyz_system_publish_run(sys, path, run, 0);
    }
    else if (ext == ".pdb") {
        return md_pdb_system_publish_run(sys, path, run, 0);
    }
    return false;
}

} // namespace molecule
