#pragma once

#include "def.h"

#include <string_view>
#include <glm/vec4.hpp>

struct md_gl_representation_t;
struct md_molecule_t;
struct md_trajectory_i;

namespace mol::util {

enum class InterpolationType {
    Nearest,
    Linear,
    Cubic
};

void update_rep_type(md_gl_representation_t& rep, mol::rep::Type type, float scale);
void update_rep_color(md_gl_representation_t& rep, const md_molecule_t& mol, mol::rep::Color color, std::string_view filter, const glm::vec4& uniform_color = glm::vec4(0));

// ensure_pbc enforces that atoms are wrapped properly within the given simulation box after interpolation.
// This operation can be quite heavy as it attempts to keep residues and chains within the same period

void interpolate_coords(md_molecule_t& mol, const md_trajectory_i* traj, InterpolationType interp, double time, bool ensure_pbc = false);

}
