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

#include "cache.h"

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

void update_rep_color(md_gl_representation_t& rep, const md_molecule_t& mol, mol::rep::Color color, const md_bitfield_t& mask, const glm::vec4& uniform_color) {
    uint32_t count = static_cast<uint32_t>(mol.atom.count);

    uint32_t* colors = (uint32_t*)md_alloc(default_allocator, sizeof(uint32_t) * count);
    defer {
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

    for (uint32_t i = 0; i < count; ++i) {
        uint32_t bitmask = md_bitfield_test_bit(&mask, i) ? 0xFFFFFFFF : 0x00FFFFFF;
        colors[i] &= bitmask;
    }

    md_gl_representation_set_color(&rep, 0, count, colors, 0);
}

constexpr inline vec4_t convert_color(uint32_t rgba) {
    return { (float)((rgba >> 0) & 0xFF) / 255.f, (float)((rgba >> 8) & 0xFF) / 255.f, (float)((rgba >> 16) & 0xFF) / 255.f, (float)((rgba >> 24) & 0xFF) / 255.f };
}

constexpr inline uint32_t convert_color(vec4_t color) {
    uint32_t out = 0;
    out |= ((uint32_t)(CLAMP(color.x, 0.0f, 1.0f) * 255.0f + 0.5f)) << 0;
    out |= ((uint32_t)(CLAMP(color.y, 0.0f, 1.0f) * 255.0f + 0.5f)) << 8;
    out |= ((uint32_t)(CLAMP(color.z, 0.0f, 1.0f) * 255.0f + 0.5f)) << 16;
    out |= ((uint32_t)(CLAMP(color.w, 0.0f, 1.0f) * 255.0f + 0.5f)) << 24;
    return out;
}

void interpolate_frame(md_molecule_t& mol, const md_trajectory_i* traj, InterpolationType interp, double time, bool ensure_pbc) {
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

    const float   t = (float)fract(time);
    const int64_t f = (int64_t)time;

    const int64_t fidx[4] = {
        MAX(0LL, f - 1),
        MAX(0LL, f),
        MIN(f + 1, last_frame),
        MIN(f + 2, last_frame)
    };

    // The interploation uses SIMD vectorization without bounds, so we make sure there is no overlap between the data segments
    /*const int64_t stride = ALIGN_TO(mol.atom.count, md_simd_f32_width);
    const int64_t bytes = stride * sizeof(float) * 3 * 4;
    void* mem = md_alloc(default_allocator, bytes);
    defer { md_free(default_allocator, mem, bytes); };

    md_vec3_soa_t src[4] = {
        {(float*)mem + stride * 0, (float*)mem + stride *  1, (float*)mem + stride *  2},
        {(float*)mem + stride * 3, (float*)mem + stride *  4, (float*)mem + stride *  5},
        {(float*)mem + stride * 6, (float*)mem + stride *  7, (float*)mem + stride *  8},
        {(float*)mem + stride * 9, (float*)mem + stride * 10, (float*)mem + stride * 11},
    };
    */

    md_vec3_soa_t dst = { mol.atom.x, mol.atom.y, mol.atom.z };

    const InterpolationType mode = (fidx[1] != fidx[2]) ? interp : InterpolationType::Nearest;
    switch (mode) {
    case InterpolationType::Nearest:
    {
        const int64_t nearest_frame = CLAMP((int64_t)(time + 0.5), 0LL, last_frame);
        FrameData frame = mol_manager::get_frame_data(traj, nearest_frame);
        
        mol.cell = frame.header->cell;
        MEMCPY(dst.x, frame.x.data(), frame.x.size_bytes());
        MEMCPY(dst.y, frame.y.data(), frame.y.size_bytes());
        MEMCPY(dst.z, frame.z.data(), frame.z.size_bytes());

        if (mol.backbone.count) {
            MEMCPY(mol.backbone.secondary_structure, frame.ss.data(), frame.ss.size_bytes());
        }
        break;
    }
    case InterpolationType::Linear:
    {
        FrameData frame[2] = {
            mol_manager::get_frame_data(traj, fidx[1]),
            mol_manager::get_frame_data(traj, fidx[2]),
        };
        //md_trajectory_frame_header_t header[2] = {0};
        //md_trajectory_load_frame(traj, frames[1], &header[0], src[0].x, src[0].y, src[0].z);
        //md_trajectory_load_frame(traj, frames[2], &header[1], src[1].x, src[1].y, src[1].z);
        //mol.cell = header[0].cell;

        if (mol.backbone.count) {
            ASSERT(frame[0].ss);
            ASSERT(frame[1].ss);

            for (int64_t i = 0; i < mol.backbone.count; ++i) {
                const vec4_t ss_f[2] = {
                    convert_color((uint32_t)frame[0].ss[i]),
                    convert_color((uint32_t)frame[1].ss[i]),
                };
                const vec4_t ss_res = vec4_lerp(ss_f[0], ss_f[1], t);
                mol.backbone.secondary_structure[i] = (md_secondary_structure_t)convert_color(ss_res);
            }
        }
        
        // @NOTE(Robin), It is ugly as shit to interpolate a matrix
        // It works in this case because its the extent of each axis that will change
        // Not the angles between them.
        mol.cell.basis     = lerp(frame[0].header->cell.basis,     frame[0].header->cell.basis,     t);
        mol.cell.inv_basis = lerp(frame[0].header->cell.inv_basis, frame[1].header->cell.inv_basis, t);
        
        md_vec3_soa_t src[2] = {
            {(float*)frame[0].x.data(), (float*)frame[0].y.data(), (float*)frame[0].z.data()},
            {(float*)frame[1].x.data(), (float*)frame[1].y.data(), (float*)frame[1].z.data()},
        };
        md_util_linear_interpolation(dst, src, mol.atom.count, mol.cell.basis * vec3_set1(1), t);
        break;
    }
    case InterpolationType::Cubic:
    {
        FrameData frame[4] = {
            mol_manager::get_frame_data(traj, fidx[0]),
            mol_manager::get_frame_data(traj, fidx[1]),
            mol_manager::get_frame_data(traj, fidx[2]),
            mol_manager::get_frame_data(traj, fidx[3]),
        };

        if (mol.backbone.count) {
            ASSERT(frame[0].ss);
            ASSERT(frame[1].ss);
            ASSERT(frame[2].ss);
            ASSERT(frame[3].ss);

            for (int64_t i = 0; i < mol.backbone.count; ++i) {
                const vec4_t ss_f[4] = {
                    convert_color((uint32_t)frame[0].ss[i]),
                    convert_color((uint32_t)frame[1].ss[i]),
                    convert_color((uint32_t)frame[2].ss[i]),
                    convert_color((uint32_t)frame[3].ss[i]),
                };
                const vec4_t ss_res = cubic_spline(ss_f[0], ss_f[1], ss_f[2], ss_f[3], t, 1.0f);
                mol.backbone.secondary_structure[i] = (md_secondary_structure_t)convert_color(ss_res);
            }
        }

        mol.cell.basis     = cubic_spline(frame[0].header->cell.basis,     frame[1].header->cell.basis,     frame[2].header->cell.basis,     frame[3].header->cell.basis,     t, tension);
        mol.cell.inv_basis = cubic_spline(frame[0].header->cell.inv_basis, frame[1].header->cell.inv_basis, frame[2].header->cell.inv_basis, frame[3].header->cell.inv_basis, t, tension);

        md_vec3_soa_t src[4] = {
            {(float*)frame[0].x.data(), (float*)frame[0].y.data(), (float*)frame[0].z.data()},
            {(float*)frame[1].x.data(), (float*)frame[1].y.data(), (float*)frame[1].z.data()},
            {(float*)frame[2].x.data(), (float*)frame[2].y.data(), (float*)frame[2].z.data()},
            {(float*)frame[3].x.data(), (float*)frame[3].y.data(), (float*)frame[3].z.data()},
        };
        md_util_cubic_spline_interpolation(dst, src, mol.atom.count, mol.cell.basis * vec3_set1(1), t, tension);
        break;
    }
    default:
        ASSERT(false);
    }

    if (ensure_pbc) {
        md_util_deperiodize_system(mol.atom.x, mol.atom.y, mol.atom.z, &mol.cell, &mol);
    }
}

/*
void interpolate_secondary_structure(md_molecule_t &mol, const md_trajectory_i* traj, InterpolationType interp, double time) {
    if (mol.backbone.secondary_structure) {

        const float t = (float)fract(time);
        const int64_t frame = (int64_t)time;
        const int64_t last_frame = md_trajectory_num_frames(traj) - 1;

        const int64_t frames[4] = {
            MAX(0LL, frame - 1),
            MAX(0LL, frame),
            MIN(frame + 1, last_frame),
            MIN(frame + 2, last_frame)
        };
        
        std::span<const md_secondary_structure_t> src_ss[4] = {
            mol_manager::get_secondary_structure_frame_data(traj, frames[0]),
            mol_manager::get_secondary_structure_frame_data(traj, frames[1]),
            mol_manager::get_secondary_structure_frame_data(traj, frames[2]),
            mol_manager::get_secondary_structure_frame_data(traj, frames[3]),
        };

        if (src_ss[0].empty() || src_ss[1].empty() || src_ss[2].empty() || src_ss[3].empty()) {
            return;
        }

        switch (interp) {
        case InterpolationType::Nearest: {
            std::span<const md_secondary_structure_t> ss = t < 0.5f ? src_ss[1] : src_ss[2];
            MEMCPY(mol.backbone.secondary_structure, ss.data(), mol.backbone.count * sizeof(md_secondary_structure_t));
            break;
        }
        case InterpolationType::Linear: {
            for (int64_t i = 0; i < mol.backbone.count; ++i) {
                const vec4_t ss_f[2] = {
                    convert_color((uint32_t)src_ss[0][i]),
                    convert_color((uint32_t)src_ss[1][i]),
                };
                const vec4_t ss_res = vec4_lerp(ss_f[0], ss_f[1], t);
                mol.backbone.secondary_structure[i] = (md_secondary_structure_t)convert_color(ss_res);
            }
            break;
        }
        case InterpolationType::Cubic: {
            for (int64_t i = 0; i < mol.backbone.count; ++i) {
                const vec4_t ss_f[4] = {
                    convert_color((uint32_t)src_ss[0][i]),
                    convert_color((uint32_t)src_ss[1][i]),
                    convert_color((uint32_t)src_ss[2][i]),
                    convert_color((uint32_t)src_ss[3][i]),
                };
                const vec4_t ss_res = cubic_spline(ss_f[0], ss_f[1], ss_f[2], ss_f[3], t, 1.0f);
                mol.backbone.secondary_structure[i] = (md_secondary_structure_t)convert_color(ss_res);
            }
            break;
        }
        default:
            ASSERT(false);
        }
    }
}
*/

}
