#include "util.h"

#include <ghoul/logging/logmanager.h>

#include <core/md_allocator.h>
#include <core/md_bitfield.h>
#include <md_molecule.h>
#include <md_trajectory.h>
#include <md_filter.h>
#include <md_gl.h>
#include <md_util.h>
#include "viamd/coloring.h"

namespace {
    constexpr const char* _loggerCat = "MolUtil";
};

namespace mol::util {

void update_rep_type(md_gl_representation_t& rep, mol::rep::Type type, float scale) {
    md_gl_representation_args_t rep_args = {};
    md_gl_representation_type_t rep_type = 0;

    switch (type) {
    case mol::rep::Type::Licorice:
        rep_args.licorice.radius = scale * 0.5f;
        rep_type = MD_GL_REP_LICORICE;
        break;
    case mol::rep::Type::Ribbons:
        rep_args.ribbons.width_scale = scale * 5;
        rep_args.ribbons.thickness_scale = scale * 5;
        rep_type = MD_GL_REP_RIBBONS;
        break;
    case mol::rep::Type::Cartoon:
        rep_args.cartoon.width_scale = scale * 5;
        rep_args.cartoon.thickness_scale = scale * 5;
        rep_type = MD_GL_REP_CARTOON;
        break;
    case mol::rep::Type::SpaceFill:
    default:
        rep_args.space_fill.radius_scale = scale;
        rep_type = MD_GL_REP_SPACE_FILL;
        break;
    }

    md_gl_representation_set_type_and_args(&rep, rep_type, rep_args);
}

void update_rep_color(md_gl_representation_t& rep, const md_molecule_t& mol, mol::rep::Color color, std::string_view filter, const glm::vec4& uniform_color) {
    uint32_t count = static_cast<uint32_t>(mol.atom.count);

    uint32_t* colors = (uint32_t*)md_alloc(default_allocator, sizeof(uint32_t) * count);
    md_bitfield_t mask = {0};
    md_bitfield_init(&mask, default_allocator);
    md_bitfield_reserve_range(&mask, 0, count);

    defer {
        md_bitfield_free(&mask);
        if (colors) md_free(default_allocator, colors, sizeof(md_flags_t) * count);
    };

    switch(color) {
    case mol::rep::Color::Cpk:
        color_atoms_cpk(colors, count, mol);
        break;
    case mol::rep::Color::AtomIndex:
        color_atoms_idx(colors, count, mol);
        break;
    case mol::rep::Color::ResId:
        color_atoms_residue_id(colors, count, mol);
        break;
    case mol::rep::Color::ResIndex:
        color_atoms_residue_index(colors, count, mol);
        break;
    case mol::rep::Color::ChainId:
        color_atoms_chain_id(colors, count, mol);
        break;
    case mol::rep::Color::ChainIndex:
        color_atoms_chain_index(colors, count, mol);
        break;
    case mol::rep::Color::SecondaryStructure:
        color_atoms_secondary_structure(colors, count, mol);
        break;
    case mol::rep::Color::Uniform:
    {
        uint32_t rgba = 0;
        rgba |= ((uint32_t)(CLAMP(uniform_color.x, 0.0f, 1.0f) * 255.0f + 0.5f)) << 0;
        rgba |= ((uint32_t)(CLAMP(uniform_color.y, 0.0f, 1.0f) * 255.0f + 0.5f)) << 8;
        rgba |= ((uint32_t)(CLAMP(uniform_color.z, 0.0f, 1.0f) * 255.0f + 0.5f)) << 16;
        rgba |= ((uint32_t)(CLAMP(uniform_color.w, 0.0f, 1.0f) * 255.0f + 0.5f)) << 24;
        color_atoms_uniform(colors, count, rgba, nullptr);
    }
    default:
        ghoul_assert(false, "unexpected molecule color");
        break;
    }

    if (!filter.empty() && filter != "" && filter != "all") {
        str_t str = {filter.data(), (int64_t)filter.length()};
        char err_buf[1024];

        if (!md_filter(&mask, str, &mol, NULL, NULL, err_buf, sizeof(err_buf))) {
            LERROR(fmt::format("Invalid filter expression: {}", err_buf));
            return;
        }

        for (uint32_t i = 0; i < count; ++i) {
            uint32_t bitmask = md_bitfield_test_bit(&mask, i) ? 0xFFFFFFFF : 0x00FFFFFF;
            colors[i] &= bitmask;
        }
    }

    md_gl_representation_set_color(&rep, 0, count, colors, 0);
}

void interpolate_coords(md_molecule_t& mol, const md_trajectory_i* traj, InterpolationType interp, double time, bool ensure_pbc) {
    if (!mol.atom.count) {
        LERROR("Cannot interpolate coords: Molecule is empty");
        return;
    }

    if (!traj) {
        LERROR("Cannot interpolate coords: Missing trajectory");
        return;
    }

    int64_t num_frames = md_trajectory_num_frames(traj);
    if (!num_frames) {
        LERROR("Cannot interpolate coords: Trajectory is empty");
        return;
    }

    const float tension = 1.0f;

    const int64_t last_frame = MAX(0LL, md_trajectory_num_frames(traj) - 1);
    // This is not actually time, but the fractional frame representation
    time = CLAMP(time, 0.0, double(last_frame));

    const float t = (float)fract(time);
    const int64_t frame = (int64_t)time;

    const int64_t frames[4] = {
        MAX(0LL, frame - 1),
        MAX(0LL, frame),
        MIN(frame + 1, last_frame),
        MIN(frame + 2, last_frame)
    };

    // The interploation uses SIMD vectorization without bounds, so we make sure there is no overlap between the data segments
    const int64_t stride = ALIGN_TO(mol.atom.count, md_simd_f32_width);
    const int64_t bytes = stride * sizeof(float) * 3 * 4;
    void* mem = md_alloc(default_allocator, bytes);
    defer { md_free(default_allocator, mem, bytes); };

    md_vec3_soa_t src[4] = {
        {(float*)mem + stride * 0, (float*)mem + stride *  1, (float*)mem + stride *  2},
        {(float*)mem + stride * 3, (float*)mem + stride *  4, (float*)mem + stride *  5},
        {(float*)mem + stride * 6, (float*)mem + stride *  7, (float*)mem + stride *  8},
        {(float*)mem + stride * 9, (float*)mem + stride * 10, (float*)mem + stride * 11},
    };

    md_vec3_soa_t dst = { mol.atom.x, mol.atom.y, mol.atom.z };

    vec3_t box_ext = {0,0,0};

    const InterpolationType mode = (frames[1] != frames[2]) ? interp : InterpolationType::Nearest;
    switch (mode) {
    case InterpolationType::Nearest:
    {
        const int64_t nearest_frame = CLAMP((int64_t)(time + 0.5), 0LL, last_frame);
        md_trajectory_frame_header_t header = {0};
        md_trajectory_load_frame(traj, nearest_frame, &header, mol.atom.x, mol.atom.y, mol.atom.z);
        box_ext = header.cell.basis * vec3_set1(1);
        break;
    }
    case InterpolationType::Linear:
    {
        md_trajectory_frame_header_t header[2] = {0};
        md_trajectory_load_frame(traj, frames[1], &header[0], src[0].x, src[0].y, src[0].z);
        md_trajectory_load_frame(traj, frames[2], &header[1], src[1].x, src[1].y, src[1].z);
        box_ext = vec3_lerp(header[0].cell.basis * vec3_set1(1), header[1].cell.basis * vec3_set1(1), t);
        md_util_linear_interpolation(dst, src, mol.atom.count, box_ext, t);
        break;
    }
    case InterpolationType::Cubic:
    {
        md_trajectory_frame_header_t header[4] = {0};
        md_trajectory_load_frame(traj, frames[0], &header[0], src[0].x, src[0].y, src[0].z);
        md_trajectory_load_frame(traj, frames[1], &header[1], src[1].x, src[1].y, src[1].z);
        md_trajectory_load_frame(traj, frames[2], &header[2], src[2].x, src[2].y, src[2].z);
        md_trajectory_load_frame(traj, frames[3], &header[3], src[3].x, src[3].y, src[3].z);
        box_ext = cubic_spline(header[0].cell.basis * vec3_set1(1), header[1].cell.basis * vec3_set1(1), header[2].cell.basis * vec3_set1(1), header[3].cell.basis * vec3_set1(1), t, tension);
        md_util_cubic_spline_interpolation(dst, src, mol.atom.count, box_ext, t, 1.0f);
        break;
    }
    default:
        ASSERT(false);
    }

    if (ensure_pbc) {
        md_util_deperiodize_system(mol.atom.x, mol.atom.y, mol.atom.z, &mol.cell, &mol);
    }
}

}
