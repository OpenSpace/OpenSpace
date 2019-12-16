/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/touch/include/touchinteraction.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>

namespace {
    // Used in the LM algorithm
    struct FunctionData {
        std::vector<glm::dvec3> selectedPoints;
        std::vector<glm::dvec2> screenPoints;
        int nDOF;
        const openspace::Camera* camera;
        openspace::SceneGraphNode* node;
        LMstat stats;
    };
}

namespace openspace {

DirectInputSolver::DirectInputSolver() {
    levmarq_init(&_lmstat);
}

// project back a 3D point in model view to clip space [-1,1] coordinates on the view plane
glm::dvec2 castToNDC(const glm::dvec3& vec, Camera& camera, SceneGraphNode* node) {
    glm::dvec3 posInCamSpace = glm::inverse(camera.rotationQuaternion()) *
        (node->worldRotationMatrix() * vec +
        (node->worldPosition() - camera.positionVec3()));

    glm::dvec4 clipspace = camera.projectionMatrix() * glm::dvec4(posInCamSpace, 1.0);
    return (glm::dvec2(clipspace) / clipspace.w);
}

// Returns the screen point s(xi,par) dependent the transform M(par) and object point xi
double distToMinimize(double* par, int x, void* fdata, LMstat* lmstat) {
    FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);

    // Apply transform to camera and find the new screen point of the updated camera state

    // { vec2 globalRot, zoom, roll, vec2 localRot }
    double q[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    for (int i = 0; i < ptr->nDOF; ++i) {
        q[i] = par[i];
    }

    using namespace glm;
    // Create variables from current state
    dvec3 camPos = ptr->camera->positionVec3();
    dvec3 centerPos = ptr->node->worldPosition();

    dvec3 directionToCenter = normalize(centerPos - camPos);
    dvec3 lookUp = ptr->camera->lookUpVectorWorldSpace();
    dvec3 camDirection = ptr->camera->viewDirectionWorldSpace();

    // Make a representation of the rotation quaternion with local and global
    // rotations
    dmat4 lookAtMat = lookAt(
        dvec3(0, 0, 0),
        directionToCenter,
        // To avoid problem with lookup in up direction
        normalize(camDirection + lookUp));
    dquat globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
    dquat localCamRot = inverse(globalCamRot) * ptr->camera->rotationQuaternion();

    { // Roll
        dquat rollRot = angleAxis(q[3], dvec3(0.0, 0.0, 1.0));
        localCamRot = localCamRot * rollRot;
    }
    { // Panning (local rotation)
        dvec3 eulerAngles(q[5], q[4], 0);
        dquat panRot = dquat(eulerAngles);
        localCamRot = localCamRot * panRot;
    }
    { // Orbit (global rotation)
        dvec3 eulerAngles(q[1], q[0], 0);
        dquat rotationDiffCamSpace = dquat(eulerAngles);

        dvec3 centerToCamera = camPos - centerPos;

        dquat rotationDiffWorldSpace =
            globalCamRot * rotationDiffCamSpace * inverse(globalCamRot);
        dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace - centerToCamera;
        camPos += rotationDiffVec3;

        centerToCamera = camPos - centerPos;
        directionToCenter = normalize(-centerToCamera);
        dvec3 lookUpWhenFacingCenter =
            globalCamRot * dvec3(ptr->camera->lookUpVectorCameraSpace());
        lookAtMat = lookAt(
            dvec3(0, 0, 0),
            directionToCenter,
            lookUpWhenFacingCenter
        );
        globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
    }
    { // Zooming
        camPos += directionToCenter * q[2];
    }
    // Update the camera state
    Camera cam = *(ptr->camera);
    cam.setPositionVec3(camPos);
    cam.setRotation(globalCamRot * localCamRot);

    // we now have a new position and orientation of camera, project surfacePoint to
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
void gradient(double* g, double* par, int x, void* fdata, LMstat* lmstat) {
    FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
    double f0 = distToMinimize(par, x, fdata, lmstat);
    // scale value to find minimum step size h, dependant on planet size
    double scale = log10(ptr->node->boundingSphere());
    std::vector<double> dPar(ptr->nDOF, 0.0);
    dPar.assign(par, par + ptr->nDOF);

    for (int i = 0; i < ptr->nDOF; ++i) {
        // Initial values
        double h = 1e-8;
        double lastG = 1;
        dPar.at(i) += h;
        double f1 = distToMinimize(dPar.data(), x, fdata, lmstat);
        dPar.at(i) = par[i];
        // Iterative process to find the minimum step h that gives a good gradient
        for (int j = 0; j < 100; ++j) {
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
                dPar.at(i) += h;
                f1 = distToMinimize(dPar.data(), x, fdata, lmstat);
                dPar.at(i) = par[i];
                break;
            }
            else if ((f1 - f0) != 0 && lastG != 0) { // h too big
                h /= scale;
            }
            else if ((f1 - f0) == 0) { // h too small
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
        for (int i = 0; i < 2; ++i) {
            g[i] = g[i] / std::abs(g[i]);
        }
    }
    else if (ptr->nDOF == 6) {
        for (int i = 0; i < ptr->nDOF; ++i) {
            // lock to only pan and zoom on 3 finger case, no roll/orbit
            g[i] = (i == 2) ? g[i] : g[i] / std::abs(g[i]);
        }
    }
}

bool DirectInputSolver::solve(const std::vector<TUIO::TuioCursor>& list, 
                              const std::vector<SelectedBody>& selectedBodies,
                              std::vector<double>* parameters, const Camera& camera) 
{
    int nFingers = std::min(static_cast<int>(list.size()), 3);
    _nDof = std::min(nFingers * 2, 6);

    // Parse input data to be used in the LM algorithm
    std::vector<glm::dvec3> selectedPoints;
    std::vector<glm::dvec2> screenPoints;

    for (int i = 0; i < nFingers; ++i) {
        const SelectedBody& sb = selectedBodies.at(i);
        selectedPoints.push_back(sb.coordinates);
        screenPoints.emplace_back(
            2 * (list[i].getX() - 0.5),
            -2 * (list[i].getY() - 0.5)
        );

        // This might be needed when we're directing the touchtable from another screen?
        //    std::vector<TuioCursor>::const_iterator c = std::find_if(
        //        list.begin(),
        //        list.end(),
        //        [&sb](const TuioCursor& c) { return c.getSessionID() == sb.id; }
        //    );
        //    if (c != list.end()) {
        //        // normalized -1 to 1 coordinates on screen
        //        screenPoints.emplace_back(2 * (c->getX() - 0.5), -2 * (c->getY() - 0.5));
        //    }
        //    else {
        //        global::moduleEngine.module<ImGUIModule>()->touchInput = {
        //            true,
        //            glm::dvec2(0.0, 0.0),
        //            1
        //        };
        //        resetAfterInput();
        //        return;
        //    }
    }

    FunctionData fData = {
        selectedPoints,
        screenPoints,
        _nDof,
        &camera,
        selectedBodies.at(0).node,
        _lmstat
    };
    void* dataPtr = reinterpret_cast<void*>(&fData);

    bool result = levmarq(
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

int DirectInputSolver::getNDof() const {
    return _nDof;
}

const LMstat& DirectInputSolver::getLevMarqStat() {
    return _lmstat;
}

void DirectInputSolver::setLevMarqVerbosity(bool verbose) {
    _lmstat.verbose = verbose;
}

} // openspace namespace

