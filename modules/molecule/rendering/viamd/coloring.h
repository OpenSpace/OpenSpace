/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <cstdint>
#include <core/md_bitfield.h>
#include <core/md_vec_math.h>
#include <md_molecule.h>

void color_atoms_uniform(uint32_t* colors, int64_t count, vec4_t color, const md_bitfield_t* mask);
void color_atoms_cpk(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_idx(uint32_t* colors, int64_t count, const md_molecule_t&);
void color_atoms_residue_id(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_residue_index(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_chain_id(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_chain_index(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_secondary_structure(uint32_t* colors, int64_t count, const md_molecule_t& mol);

