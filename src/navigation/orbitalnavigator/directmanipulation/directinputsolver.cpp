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

#include <openspace/navigation/orbitalnavigator/directmanipulation/directinputsolver.h>

#include <openspace/camera/camera.h>
#include <openspace/camera/camerapose.h>
#include <openspace/navigation/orbitalnavigator/directmanipulation/directmanipulation.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <algorithm>
#include <cmath>
#include <cstdlib>

namespace {
    // Used in the LM algorithm
    struct FunctionData {
        std::vector<glm::dvec3> selectedPoints;
        std::vector<glm::dvec2> screenPoints;
        int nDOF;
        const openspace::Camera* camera;
        const openspace::SceneGraphNode* node;
        ghoul::LMstat stats;
    };

    // Project back a 3D point in model view to clip space [-1,1] coordinates on the view
    // plane
    glm::dvec2 castToNDC(const glm::dvec3& vec, openspace::Camera& camera,
                         const openspace::SceneGraphNode* node)
    {
        glm::dvec3 posInCamSpace = glm::inverse(camera.rotationQuaternion()) *
            (node->worldRotationMatrix() * vec +
                (node->worldPosition() - camera.positionVec3()));

        glm::dvec4 clipspace = camera.projectionMatrix() * glm::dvec4(posInCamSpace, 1.0);
        return (glm::dvec2(clipspace) / clipspace.w);
    }

    // Returns screen point s(xi,par) dependent the transform M(par) and object point xi
    double distToMinimize(double* par, int x, void* fdata, ghoul::LMstat* lmstat) {
        FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);

        // Apply transform to camera and find the screen point of the updated camera state

        // { vec2 globalRot, zoom, roll, vec2 localRot }
        double q[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
        for (int i = 0; i < ptr->nDOF; i++) {
            q[i] = par[i];
        }

        using DirectManipulation = openspace::interaction::DirectManipulation;
        DirectManipulation::VelocityStates velocities = {
            .orbit = glm::dvec2(q[0], q[1]),
            .zoom = q[2],
            .roll = q[3],
            .pan = glm::dvec2(q[4], q[5])
        };

        // @TODO (emmbr, 2026-01-19): Maybe it would be better to put this function
        // somewhere else, to avoid the circular dependency
        openspace::CameraPose pose = DirectManipulation::cameraPoseFromVelocities(
            velocities,
            ptr->camera,
            ptr->node
        );

        // Update the camera state (for a local copy of the camera)
        openspace::Camera cam = *ptr->camera;
        cam.setPose(pose);

        // We now have a new position and orientation of camera, project surfacePoint to
        // the new screen to get distance to minimize
        glm::dvec2 newScreenPoint = castToNDC(
            ptr->selectedPoints.at(x),
            cam,
            ptr->node
        );
        lmstat->pos.push_back(newScreenPoint);
        return glm::length(ptr->screenPoints.at(x) - newScreenPoint);
    }

    // Gradient of distToMinimize w.r.t par (using forward difference)
    void gradient(double* g, double* par, int x, void* fdata, ghoul::LMstat* lmstat) {
        FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
        double f0 = distToMinimize(par, x, fdata, lmstat);
        // scale value to find minimum step size h, dependant on planet size
        double scale = log10(ptr->node->interactionSphere());
        std::vector<double> dPar(ptr->nDOF, 0.0);
        dPar.assign(par, par + ptr->nDOF);

        for (int i = 0; i < ptr->nDOF; i++) {
            // Initial values
            double h = 1e-8;
            double lastG = 1;
            dPar.at(i) += h;
            double f1 = distToMinimize(dPar.data(), x, fdata, lmstat);
            dPar.at(i) = par[i];
            // Iterative process to find the minimum step h that gives a good gradient
            for (int j = 0; j < 100; j++) {
                if ((f1 - f0) != 0 && lastG == 0) { // found minimum step size h
                    // scale up to get a good initial guess value
                    h *= scale * scale * scale;

                    // clamp min step size to a fraction of the incoming parameter
                    if (i == 2) {
                        double epsilon = 1e-3;
                        // make sure incoming parameter is larger than 0
                        h = std::max(std::max(std::abs(dPar.at(i)), epsilon) * 0.001, h);
                    }
                    else if (ptr->nDOF == 2) {
                        h = std::max(std::abs(dPar.at(i)) * 0.001, h);
                    }

                    // calculate f1 with good h for finite difference
                    dPar[i] += h;
                    f1 = distToMinimize(dPar.data(), x, fdata, lmstat);
                    dPar[i] = par[i];
                    break;
                }
                else if ((f1 - f0) != 0 && lastG != 0) {
                    // h too big
                    h /= scale;
                }
                else if ((f1 - f0) == 0) {
                    // h too small
                    h *= scale;
                }
                lastG = f1 - f0;
                dPar.at(i) += h;
                f1 = distToMinimize(dPar.data(), x, fdata, lmstat);
                dPar.at(i) = par[i];
            }
            g[i] = (f1 - f0) / h;
        }
        if (ptr->nDOF == 2) {
            // normalize on 1 finger case to allow for horizontal/vertical movement
            for (int i = 0; i < 2; i++) {
                g[i] = g[i] / std::abs(g[i]);
            }
        }
        else if (ptr->nDOF == 6) {
            for (int i = 0; i < ptr->nDOF; i++) {
                // lock to only pan and zoom on 3 finger case, no roll/orbit
                g[i] = (i == 2) ? g[i] : g[i] / std::abs(g[i]);
            }
        }
    }
} // namespace

namespace openspace::interaction {

DirectInputSolver::DirectInputSolver() {
    ghoul::initializeLevmarqStats(&_lmstat);
}

bool DirectInputSolver::solve(const std::vector<TouchPoint>& touchPoints,
                              const std::vector<SelectedBody>& selectedBodies,
                              std::vector<double>* parameters, const Camera& camera)
{
    ZoneScopedN("Direct touch input solver");

    ghoul_assert(
        selectedBodies.size() >= touchPoints.size(),
        "Number of touch inputs must match the number of 'selected bodies'"
    );

    int nFingers = std::min(static_cast<int>(touchPoints.size()), 3);
    _nDof = std::min(nFingers * 2, 6);

    // Parse input data to be used in the LM algorithm
    std::vector<glm::dvec3> selectedPoints;
    std::vector<glm::dvec2> screenPoints;

    for (int i = 0; i < nFingers; i++) {
        const SelectedBody& sb = selectedBodies.at(i);
        selectedPoints.push_back(sb.coordinates);
        screenPoints.emplace_back(
            2.0 * (touchPoints[i].x - 0.5),
            -2.0 * (touchPoints[i].y - 0.5)
        );
    }

    FunctionData fData = {
        .selectedPoints = selectedPoints,
        .screenPoints = screenPoints,
        .nDOF = _nDof,
        .camera = &camera,
        .node = selectedBodies.at(0).node,
        .stats = _lmstat
    };
    void* dataPtr = reinterpret_cast<void*>(&fData);

    bool result = ghoul::levmarq(
        _nDof,
        parameters->data(),
        static_cast<int>(screenPoints.size()),
        nullptr,
        distToMinimize,
        gradient,
        dataPtr,
        &_lmstat
    );

    return result;
}

int DirectInputSolver::nDof() const {
    return _nDof;
}

} // namespace openspace::interaction

