#pragma once

#include "def.h"

#include <glm/vec4.hpp>

struct md_gl_representation_t;
struct md_molecule_t;
struct md_trajectory_i;
struct md_bitfield_t;

namespace mol::util {

enum class InterpolationType {
    Nearest,
    Linear,
    Cubic
};

void update_rep_type(md_gl_representation_t& rep, mol::rep::Type type, float scale);
void update_rep_color(md_gl_representation_t& rep, const md_molecule_t& mol, mol::rep::Color color, const md_bitfield_t& mask, const glm::vec4& uniform_color = glm::vec4(0));

// ensure_pbc enforces that atoms are wrapped properly within the given simulation box after interpolation.
// This operation can be quite heavy as it attempts to keep residues and chains within the same period

void interpolate_frame(md_molecule_t& mol, const md_trajectory_i* traj, InterpolationType interp, double frame, bool ensure_pbc = false);
//void interpolate_secondary_structure(md_molecule_t&mol, const md_trajectory_i* traj, InterpolationType interp, double time);

}
