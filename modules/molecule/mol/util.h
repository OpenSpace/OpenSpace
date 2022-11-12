#pragma once

#include "def.h"

#include <string_view>

struct md_gl_representation_t;
struct md_molecule_t;
struct md_trajectory_i;

namespace mol::util {

enum class Interpolation {
    Nearest,
    Linear,
    Cubic
};

void update_rep_type(md_gl_representation_t& rep, mol::rep::Type type, float scale);
void update_rep_colors(md_gl_representation_t& rep, const md_molecule_t& mol, mol::rep::Color color, std::string_view filter);
void interpolate_coords(float* x, float* y, float* z, int64_t count, double time, Interpolation interp, const md_trajectory_i* traj);

}
