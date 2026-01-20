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

#include <modules/molecule/mol/util.h>

#include <modules/molecule/mol/cache.h>
#include <modules/molecule/mol/viamd/coloring.h>
#include <ghoul/logging/logmanager.h>
#include <core/md_allocator.h>
#include <core/md_bitfield.h>
#include <md_molecule.h>
#include <md_trajectory.h>
#include <md_filter.h>
#include <md_gl.h>
#include <md_util.h>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "MolUtil";

    constexpr vec4_t convertColor(uint32_t rgba) {
        return {
            static_cast<float>((rgba >> 0) & 0xFF) / 255.f,
            static_cast<float>((rgba >> 8) & 0xFF) / 255.f,
            static_cast<float>((rgba >> 16) & 0xFF) / 255.f,
            static_cast<float>((rgba >> 24) & 0xFF) / 255.f
        };
    }

    constexpr uint32_t convertColor(const glm::vec4& color) {
        const glm::vec4 c =
            glm::clamp(color, glm::vec4(0.f), glm::vec4(1.f)) * 255.f + 0.5f;

        uint32_t out = 0;
        out |= static_cast<uint32_t>(c.x) << 0;
        out |= static_cast<uint32_t>(c.y) << 8;
        out |= static_cast<uint32_t>(c.z) << 16;
        out |= static_cast<uint32_t>(c.w) << 24;
        return out;
    }

    constexpr uint32_t convertColor(vec4_t color) {
        return convertColor(glm::vec4(color.x, color.y, color.z, color.w));
    }
} // namespace

namespace mol::util {

void updateRepType(md_gl_representation_t& rep, mol::rep::Type type, float scale) {
    md_gl_representation_args_t repArgs = {};
    md_gl_representation_type_t repType = 0;

    switch (type) {
        case rep::Type::Licorice:
            repArgs.licorice.radius = scale * 0.5f;
            repType = MD_GL_REP_LICORICE;
            break;
        case mol::rep::Type::Ribbons:
            repArgs.ribbons.width_scale = scale * 5;
            repArgs.ribbons.thickness_scale = scale * 5;
            repType = MD_GL_REP_RIBBONS;
            break;
        case rep::Type::Cartoon:
            repArgs.cartoon.width_scale = scale * 5;
            repArgs.cartoon.thickness_scale = scale * 5;
            repType = MD_GL_REP_CARTOON;
            break;
        case rep::Type::SpaceFill:
            repArgs.space_fill.radius_scale = scale;
            repType = MD_GL_REP_SPACE_FILL;
            break;
    }

    md_gl_representation_set_type_and_args(&rep, repType, repArgs);
}

void updateRepColor(md_gl_representation_t& rep, const md_molecule_t& mol,
                    mol::rep::Color color, const md_bitfield_t& mask,
                    const glm::vec4& uniformColor)
{
    uint32_t count = static_cast<uint32_t>(mol.atom.count);

    uint32_t* colors = reinterpret_cast<uint32_t*>(
        md_alloc(default_allocator, count * sizeof(uint32_t))
    );
    defer {
        md_free(default_allocator, colors, count * sizeof(md_flags_t));
    };

    switch (color) {
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
            color_atoms_uniform(colors, count, convertColor(uniformColor), nullptr);
            break;
    }

    for (uint32_t i = 0; i < count; i++) {
        uint32_t bitmask = md_bitfield_test_bit(&mask, i) ? 0xFFFFFFFF : 0x00FFFFFF;
        colors[i] &= bitmask;
    }

    md_gl_representation_set_color(&rep, 0, count, colors, 0);
}

void interpolateFrame(md_molecule_t& mol, const md_trajectory_i* traj,
                      InterpolationType interp, double time, bool ensurePbc)
{
    if (!mol.atom.count) {
        LERROR("Cannot interpolate coords: Molecule is empty");
        return;
    }

    if (!traj) {
        LERROR("Cannot interpolate coords: Missing trajectory");
        return;
    }

    int64_t nFrames = md_trajectory_num_frames(traj);
    if (!nFrames) {
        LERROR("Cannot interpolate coords: Trajectory is empty");
        return;
    }

    const int64_t lastFrame = std::max(0LL, nFrames - 1);
    // This is not actually time, but the fractional frame representation
    time = std::clamp(time, 0.0, static_cast<double>(lastFrame));

    const float t = static_cast<float>(fract(time));
    const int64_t f = static_cast<int64_t>(time);

    const int64_t fIdx[4] = {
        std::max(0LL, f - 1),
        std::max(0LL, f),
        std::min(f + 1, lastFrame),
        std::min(f + 2, lastFrame)
    };

    md_vec3_soa_t dst = { mol.atom.x, mol.atom.y, mol.atom.z };

    const InterpolationType mode =
        (fIdx[1] != fIdx[2]) ? interp : InterpolationType::Nearest;
    switch (mode) {
        case InterpolationType::Nearest:
        {
            const int64_t nearestFrame =
                std::clamp(static_cast<int64_t>(time + 0.5), 0LL, lastFrame);
            FrameData frame = frameData(traj, nearestFrame);
        
            mol.cell = frame.header->cell;
            std::memcpy(dst.x, frame.x.data(), frame.x.size_bytes());
            std::memcpy(dst.y, frame.y.data(), frame.y.size_bytes());
            std::memcpy(dst.z, frame.z.data(), frame.z.size_bytes());

            if (mol.backbone.count) {
                std::memcpy(
                    mol.backbone.secondary_structure,
                    frame.ss.data(),
                    frame.ss.size_bytes()
                );
            }
            break;
        }
        case InterpolationType::Linear:
        {
            FrameData frame[2] = {
                mol::frameData(traj, fIdx[1]),
                mol::frameData(traj, fIdx[2])
            };

            if (mol.backbone.count) {
                for (int64_t i = 0; i < mol.backbone.count; i++) {
                    const vec4_t ss[2] = {
                        convertColor(frame[0].ss[i]),
                        convertColor(frame[1].ss[i])
                    };
                    const vec4_t ssr = vec4_lerp(ss[0], ss[1], t);
                    mol.backbone.secondary_structure[i] = convertColor(ssr);
                }
            }
        
            // @NOTE(Robin)  It is ugly to interpolate a matrix. It works in this case
            // because only the extent of each axis and not the angles between them change
            mol.cell.basis = lerp(
                frame[0].header->cell.basis,
                frame[0].header->cell.basis,
                t
            );
            mol.cell.inv_basis = lerp(
                frame[0].header->cell.inv_basis,
                frame[1].header->cell.inv_basis,
                t
            );
        
            md_vec3_soa_t src[2] = {
                { frame[0].x.data(), frame[0].y.data(), frame[0].z.data() },
                { frame[1].x.data(), frame[1].y.data(), frame[1].z.data() }
            };
            md_util_linear_interpolation(
                dst,
                src,
                mol.atom.count,
                mol.cell.basis * vec3_set1(1),
                t
            );
            break;
        }
        case InterpolationType::Cubic:
        {
            FrameData frame[4] = {
                mol::frameData(traj, fIdx[0]),
                mol::frameData(traj, fIdx[1]),
                mol::frameData(traj, fIdx[2]),
                mol::frameData(traj, fIdx[3])
            };

            if (mol.backbone.count) {
                for (int64_t i = 0; i < mol.backbone.count; i++) {
                    const vec4_t ss[4] = {
                        convertColor(frame[0].ss[i]),
                        convertColor(frame[1].ss[i]),
                        convertColor(frame[2].ss[i]),
                        convertColor(frame[3].ss[i])
                    };
                    const vec4_t ssr = cubic_spline(ss[0], ss[1], ss[2], ss[3], t, 1.f);
                    mol.backbone.secondary_structure[i] = convertColor(ssr);
                }
            }

            constexpr float Scaling = 1.f;
            mol.cell.basis = cubic_spline(
                frame[0].header->cell.basis,
                frame[1].header->cell.basis,
                frame[2].header->cell.basis,
                frame[3].header->cell.basis,
                t,
                Scaling
            );
            mol.cell.inv_basis = cubic_spline(
                frame[0].header->cell.inv_basis,
                frame[1].header->cell.inv_basis,
                frame[2].header->cell.inv_basis,
                frame[3].header->cell.inv_basis,
                t,
                Scaling
            );

            md_vec3_soa_t src[4] = {
                { frame[0].x.data(), frame[0].y.data(), frame[0].z.data() },
                { frame[1].x.data(), frame[1].y.data(), frame[1].z.data() },
                { frame[2].x.data(), frame[2].y.data(), frame[2].z.data() },
                { frame[3].x.data(), frame[3].y.data(), frame[3].z.data() }
            };
            md_util_cubic_spline_interpolation(
                dst,
                src,
                mol.atom.count,
                mol.cell.basis * vec3_set1(1),
                t,
                Scaling
            );
            break;
        }
    }

    if (ensurePbc) {
        md_util_deperiodize_system(mol.atom.x, mol.atom.y, mol.atom.z, &mol.cell, &mol);
    }
}

} // namespace mol::util
