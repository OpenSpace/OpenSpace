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

#include <modules/molecule/src/def.h>
#include <openspace/glm.h>
#include <core/md_bitset.h>
#include <md_gl.h>
#include <md_types.h>

struct md_system_t;
struct md_system_state_t;

namespace molecule {

struct Trajectory;

enum class InterpolationType {
    Nearest,
    Linear,
    Cubic
};

void setDrawOpTypeAndArgs(md_gl_draw_op_t& op, rep::Type type, float scale);

void updateRepColor(md_gl_rep_t rep, const md_system_t& sys,
    rep::Color color, const md_bitmask_t& mask,
    const glm::vec4& uniformColor = glm::vec4(0),
    const md_secondary_structure_t* ss = nullptr);

// ensure_pbc enforces that atoms are unwrapped properly within the given simulation box
// after interpolation.
void interpolateFrame(md_system_state_t& state, const Trajectory* traj,
    InterpolationType interp, double frame, bool ensurePbc = false);

} // namespace molecule

#endif // __OPENSPACE_MODULE_MOLECULE___UTIL___H__
