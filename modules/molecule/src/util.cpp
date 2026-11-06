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

#include <modules/molecule/src/util.h>

#include <modules/molecule/src/cache.h>
#include <modules/molecule/src/coloring.h>
#include <openspace/logging/logmanager.h>
#include <core/md_allocator.h>
#include <core/md_bitset.h>
#include <md_system.h>
#include <md_gl.h>
#include <md_util.h>
#include <algorithm>
#include <string_view>
#include <vector>

namespace {
    constexpr std::string_view _loggerCat = "MolUtil";

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

    double cubicSpline1D(double p0, double p1, double p2, double p3, float t, float s) {
        double t2 = t * t;
        double t3 = t2 * t;
        double m1 = s * (p2 - p0) * 0.5;
        double m2 = s * (p3 - p1) * 0.5;
        return (2.0 * t3 - 3.0 * t2 + 1.0) * p1 +
               (t3 - 2.0 * t2 + t) * m1 +
               (-2.0 * t3 + 3.0 * t2) * p2 +
               (t3 - t2) * m2;
    }
} // namespace

namespace molecule {

void setDrawOpTypeAndArgs(md_gl_draw_op_t& op, rep::Type type, float scale) {
    switch (type) {
        case rep::Type::SpaceFill:
            op.type = MD_GL_REP_SPACE_FILL;
            op.args.space_fill.radius_scale = scale;
            break;
        case rep::Type::Licorice:
            op.type = MD_GL_REP_LICORICE;
            op.args.licorice.radius = scale * 0.5f;
            op.args.licorice.color_mode = MD_GL_BOND_MODE_NEAREST;
            op.args.licorice.sharpness = 1.0f;
            op.args.licorice.uniform_color = 0;
            op.args.licorice.bond_orders = true;
            break;
        case rep::Type::Ribbons:
            op.type = MD_GL_REP_RIBBONS;
            op.args.ribbons.width_scale = scale * 5.f;
            op.args.ribbons.thickness_scale = scale * 5.f;
            break;
        case rep::Type::Cartoon:
            op.type = MD_GL_REP_CARTOON;
            op.args.cartoon.coil_scale = scale * 5.f;
            op.args.cartoon.sheet_scale = scale * 5.f;
            op.args.cartoon.helix_scale = scale * 5.f;
            op.args.cartoon.raycast = false;
            break;
    }
}

void updateRepColor(md_gl_rep_t rep, const md_system_t& sys,
                    rep::Color color, const md_bitmask_t& mask,
                    const glm::vec4& uniformColor,
                    const md_secondary_structure_t* ss)
{
    const uint32_t count = static_cast<uint32_t>(sys.atom.count);
    if (count == 0) {
        return;
    }

    std::vector<uint32_t> colors(count);

    switch (color) {
        case rep::Color::Cpk:
            cpk(colors.data(), count, sys);
            break;
        case rep::Color::AtomIndex:
            idx(colors.data(), count, sys);
            break;
        case rep::Color::ResId:
            residueId(colors.data(), count, sys);
            break;
        case rep::Color::ResIndex:
            residueIndex(colors.data(), count, sys);
            break;
        case rep::Color::ChainId:
            chainId(colors.data(), count, sys);
            break;
        case rep::Color::ChainIndex:
            chainIndex(colors.data(), count, sys);
            break;
        case rep::Color::SecondaryStructure:
            secondaryStructure(colors.data(), count, sys, ss);
            break;
        case rep::Color::Uniform:
            uniform(colors.data(), count, convertColor(uniformColor));
            break;
    }

    for (uint32_t i = 0; i < count; i++) {
        uint32_t bitmask = md_bitmask_test_bit(&mask, i) ? 0xFFFFFFFF : 0x00FFFFFF;
        colors[i] &= bitmask;
    }

    md_gl_rep_set_atom_colors(rep, 0, count, colors.data(), 0);
}

void interpolateFrame(md_system_state_t& state, const Trajectory* traj,
                      InterpolationType interp, double time, bool ensurePbc)
{
    if (!state.xyz || !state.num_atoms) {
        LERROR("Cannot interpolate coords: State coordinate buffer is empty");
        return;
    }

    if (!traj) {
        LERROR("Cannot interpolate coords: Missing trajectory");
        return;
    }

    const int64_t nFrames = trajectoryNumFrames(traj);
    if (!nFrames) {
        LERROR("Cannot interpolate coords: Trajectory is empty");
        return;
    }

    const int64_t lastFrame = std::max<int64_t>(0, nFrames - 1);
    time = std::clamp(time, 0.0, static_cast<double>(lastFrame));

    const float t = static_cast<float>(fract(time));
    const int64_t f = static_cast<int64_t>(time);

    const int64_t fIdx[4] = {
        std::max<int64_t>(0LL, f - 1),
        std::max<int64_t>(0LL, f),
        std::min<int64_t>(f + 1, lastFrame),
        std::min<int64_t>(f + 2, lastFrame)
    };

    const InterpolationType mode =
        (fIdx[1] != fIdx[2]) ? interp : InterpolationType::Nearest;

    switch (mode) {
        case InterpolationType::Nearest:
        {
            const int64_t nearestFrame =
                std::clamp<int64_t>(static_cast<int64_t>(time + 0.5), 0, lastFrame);
            FrameData frame = frameData(traj, nearestFrame);

            state.unitcell = frame.unitcell;
            state.frame = static_cast<double>(nearestFrame);
            const size_t bytes =
                std::min(state.num_atoms * sizeof(vec3_t), frame.xyz.size_bytes());
            std::memcpy(state.xyz, frame.xyz.data(), bytes);
            break;
        }
        case InterpolationType::Linear:
        {
            FrameData frame[2] = {
                frameData(traj, fIdx[1]),
                frameData(traj, fIdx[2])
            };

            state.unitcell.x = glm::mix(frame[0].unitcell.x, frame[1].unitcell.x, t);
            state.unitcell.y = glm::mix(frame[0].unitcell.y, frame[1].unitcell.y, t);
            state.unitcell.z = glm::mix(frame[0].unitcell.z, frame[1].unitcell.z, t);
            state.unitcell.xy = glm::mix(frame[0].unitcell.xy, frame[1].unitcell.xy, t);
            state.unitcell.xz = glm::mix(frame[0].unitcell.xz, frame[1].unitcell.xz, t);
            state.unitcell.yz = glm::mix(frame[0].unitcell.yz, frame[1].unitcell.yz, t);
            state.unitcell.flags = frame[0].unitcell.flags | frame[1].unitcell.flags;

            const vec3_t* in_xyz[2] = {
                frame[0].xyz.data(),
                frame[1].xyz.data()
            };
            md_util_interpolate_linear(
                state.xyz,
                in_xyz,
                state.num_atoms,
                &state.unitcell,
                t
            );
            state.frame = time;
            break;
        }
        case InterpolationType::Cubic:
        {
            FrameData frame[4] = {
                frameData(traj, fIdx[0]),
                frameData(traj, fIdx[1]),
                frameData(traj, fIdx[2]),
                frameData(traj, fIdx[3])
            };

            constexpr float Scaling = 1.f;
            state.unitcell.x = cubicSpline1D(
                frame[0].unitcell.x, frame[1].unitcell.x,
                frame[2].unitcell.x, frame[3].unitcell.x, t, Scaling
            );
            state.unitcell.y = cubicSpline1D(
                frame[0].unitcell.y, frame[1].unitcell.y,
                frame[2].unitcell.y, frame[3].unitcell.y, t, Scaling
            );
            state.unitcell.z = cubicSpline1D(
                frame[0].unitcell.z, frame[1].unitcell.z,
                frame[2].unitcell.z, frame[3].unitcell.z, t, Scaling
            );
            state.unitcell.xy = cubicSpline1D(
                frame[0].unitcell.xy, frame[1].unitcell.xy,
                frame[2].unitcell.xy, frame[3].unitcell.xy, t, Scaling
            );
            state.unitcell.xz = cubicSpline1D(
                frame[0].unitcell.xz, frame[1].unitcell.xz,
                frame[2].unitcell.xz, frame[3].unitcell.xz, t, Scaling
            );
            state.unitcell.yz = cubicSpline1D(
                frame[0].unitcell.yz, frame[1].unitcell.yz,
                frame[2].unitcell.yz, frame[3].unitcell.yz, t, Scaling
            );
            state.unitcell.flags = frame[1].unitcell.flags | frame[2].unitcell.flags;

            const vec3_t* in_xyz[4] = {
                frame[0].xyz.data(),
                frame[1].xyz.data(),
                frame[2].xyz.data(),
                frame[3].xyz.data()
            };
            md_util_interpolate_cubic_spline(
                state.xyz,
                in_xyz,
                state.num_atoms,
                &state.unitcell,
                t,
                Scaling
            );
            state.frame = time;
            break;
        }
    }

    if (ensurePbc) {
        const md_system_t* sys = trajectorySystem(traj);
        if (sys) {
            md_util_unwrap_system(&state, sys);
        }
    }
}

} // namespace molecule
