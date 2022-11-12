#include "util.h"

#include <core/md_allocator.h>
#include <core/md_bitfield.h>
#include <md_molecule.h>
#include <md_trajectory.h>
#include <md_filter.h>
#include <md_gl.h>
#include <md_util.h>
#include "viamd/coloring.h"

namespace mol::util {

void update_rep_type(md_gl_representation_t& rep, mol::rep::Type type, float scale) {
    md_gl_representation_args_t rep_args = {};
    md_gl_representation_type_t rep_type = 0;

    switch (type) {
    case mol::rep::Type::SpaceFill:
        rep_args.space_fill.radius_scale = scale;
        rep_type = MD_GL_REP_SPACE_FILL;
        break;
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
    default:
        rep_args.space_fill.radius_scale = 1.0f;
        rep_type = MD_GL_REP_SPACE_FILL;
    }

    md_gl_representation_set_type_and_args(&rep, rep_type, rep_args);
}

void update_rep_colors(md_gl_representation_t& rep, const md_molecule_t& mol, mol::rep::Color color, std::string_view filter) {
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
    default:
        ghoul_assert(false, "unexpected molecule color");
        break;
    }

    if (!filter.empty() && filter != "") {
        str_t str = {filter.data(), (int64_t)filter.length()};
        char err_buf[1024];

        if (!md_filter(&mask, str, &mol, NULL, err_buf, sizeof(err_buf))) {
            // TODO: log error to some appropriate channel
            return;
        }

        for (uint32_t i = 0; i < count; ++i) {
            uint32_t bitmask = md_bitfield_test_bit(&mask, i) ? 0xFFFFFFFF : 0x00FFFFFF;
            colors[i] &= bitmask;
        }
    }

    md_gl_representation_set_color(&rep, 0, count, colors, 0);
}

void interpolate_coords(float* x, float* y, float* z, int64_t count, double time, Interpolation interp, const md_trajectory_i* traj) {
    (void)interp; // We always go for cubic

    const float tension = 1.0f;

    int64_t nFrames = md_trajectory_num_frames(traj);
    if (nFrames >= 4) {
        double t = fract(time);
        int64_t frames[4];

        // The animation is played forward and back (bouncing), the first and last
        // frame are repeated.

        if ((int64_t(time) / nFrames) % 2 == 0) { // animation forward
            int64_t frame = int64_t(time) % nFrames;
            if (frame < 0) frame += nFrames;
            frames[0] = std::max<int64_t>(0, frame - 1);
            frames[1] = frame;
            frames[2] = std::min<int64_t>(nFrames - 1, frame + 1);
            frames[3] = std::min<int64_t>(nFrames - 1, frame + 2);
        }
        else { // animation backward
            t = 1.0 - t;
            int64_t frame = nFrames - 1 - (int64_t(time) % nFrames);
            if (frame >= nFrames) frame -= nFrames;
            frames[0] = std::max<int64_t>(0, frame - 2);
            frames[1] = std::max<int64_t>(0, frame - 1);
            frames[2] = frame;
            frames[3] = std::min<int64_t>(nFrames - 1, frame + 1);
        }

        // cubic
        md_trajectory_frame_header_t header[4];
        mat3_t boxes[4];
        int64_t stride = ROUND_UP(count, md_simd_widthf);    // The interploation uses SIMD vectorization without bounds, so we make sure there is no overlap between the data segments
        int64_t bytes = stride * sizeof(float) * 3 * 4;
        float* mem = static_cast<float*>(malloc(bytes));
        defer { free(mem); };

        md_vec3_soa_t src[4] = {
            {mem + stride * 0, mem + stride * 1, mem + stride * 2},
            {mem + stride * 3, mem + stride * 4, mem + stride * 5},
            {mem + stride * 6, mem + stride * 7, mem + stride * 8},
            {mem + stride * 9, mem + stride * 10, mem + stride * 11},
        };

        md_trajectory_load_frame(traj, frames[0], &header[0], src[0].x, src[0].y, src[0].z);
        md_trajectory_load_frame(traj, frames[1], &header[1], src[1].x, src[1].y, src[1].z);
        md_trajectory_load_frame(traj, frames[2], &header[2], src[2].x, src[2].y, src[2].z);
        md_trajectory_load_frame(traj, frames[3], &header[3], src[3].x, src[3].y, src[3].z);

        memcpy(&boxes[0], header[0].box, sizeof(boxes[0]));
        memcpy(&boxes[1], header[1].box, sizeof(boxes[1]));
        memcpy(&boxes[2], header[2].box, sizeof(boxes[2]));
        memcpy(&boxes[3], header[3].box, sizeof(boxes[3]));

        vec3_t ones = vec3_t{1.0, 1.0, 1.0};
        vec3_t pbc_ext = cubic_spline(boxes[0] * ones, boxes[1] * ones, boxes[2] * ones, boxes[3] * ones, static_cast<float>(t), tension);

        md_util_cubic_interpolation({x,y,z}, src, count, pbc_ext, static_cast<float>(t), tension);
    }
}

}
