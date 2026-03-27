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

#ifndef __OPENSPACE_CORE___DIRECTINPUT_SOLVER___H__
#define __OPENSPACE_CORE___DIRECTINPUT_SOLVER___H__

#include <ghoul/glm.h>
#include <ghoul/misc/levmarqsolver.h>
#include <optional>
#include <vector>

namespace openspace {

class Camera;
class SceneGraphNode;

/**
 * The DirectInputSolver is used to minimize the L2 error of touch input to 3D camera
 * position. It uses the levmarq (Levenberg–Marquardt) algorithm in order to do this.
 */
class DirectInputSolver {
public:
    /**
     * Result of the optimization process, containing the computed camera manipulation
     * parameters. These values represent the transformations needed to align the camera
     * with the touch input.
     */
    struct Result {
        glm::dvec2 orbit = glm::dvec2(0.0);
        double zoom = 0.0;
        double roll = 0.0;
        glm::dvec2 pan = glm::dvec2(0.0);
    };

    /**
     * Stores the selected node, the cursor ID as well as the surface coordinates the
     * cursor touched.
     */
    struct SelectedBody {
        size_t id = 0;
        const SceneGraphNode* node = nullptr;
        glm::dvec3 coordinates = glm::dvec3(0.0);
    };

    /**
     * Represents touched screenspace coordinates (normalized to [0, 1]).
     */
    struct TouchPoint {
        size_t id = 0;
        glm::dvec2 position = glm::dvec2(0.0);
    };

    DirectInputSolver();

    /**
     * Returns a result if the error could be minimized within certain bounds. If the
     * error is found to be outside the bounds after a certain amount of iterations,
     * this function fails and returns `std::nullopt`.
     */
    std::optional<Result> solve(const std::vector<TouchPoint>& touchPoints,
        const std::vector<SelectedBody>& selectedBodies, const Camera& camera);

private:
    ghoul::LMstat _lmstat;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___DIRECTINPUT_SOLVER___H__

