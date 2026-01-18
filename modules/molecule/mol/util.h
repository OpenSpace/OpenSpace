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

#ifndef __OPENSPACE_MODULE_MOLECULE___UTIL___H__
#define __OPENSPACE_MODULE_MOLECULE___UTIL___H__

#include <modules/molecule/mol/def.h>
#include <ghoul/glm.h>

struct md_bitfield_t;
struct md_gl_representation_t;
struct md_molecule_t;
struct md_trajectory_i;

namespace mol::util {

enum class InterpolationType {
    Nearest,
    Linear,
    Cubic
};

void updateRepType(md_gl_representation_t& rep, mol::rep::Type type, float scale);
void updateRepColor(md_gl_representation_t& rep, const md_molecule_t& mol,
    mol::rep::Color color, const md_bitfield_t& mask,
    const glm::vec4& uniformColor = glm::vec4(0));

// ensure_pbc enforces that atoms are wrapped properly within the given simulation box
// after interpolation. This operation can be quite heavy as it attempts to keep residues
// and chains within the same period
void interpolateFrame(md_molecule_t& mol, const md_trajectory_i* traj,
    InterpolationType interp, double frame, bool ensurePbc = false);

} // namespace mol::util

#endif // __OPENSPACE_MODULE_MOLECULE___UTIL___H__
