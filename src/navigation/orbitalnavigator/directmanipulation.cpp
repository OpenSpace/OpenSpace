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

#include <openspace/navigation/orbitalnavigator/directmanipulation.h>

#include <openspace/camera/camera.h>
#include <openspace/camera/camerapose.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator/orbitalnavigator.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/levmarqsolver.h>
#include <glm/gtx/intersect.hpp>
#include <algorithm>
#include <format>
#include <utility>

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4310) // cast truncates constant value
#endif // WIN32

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "DirectManipulation";

    /**
     * Used in the LM algorithm/
     */
    struct FunctionData {
        std::vector<glm::dvec3> selectedPoints;
        std::vector<glm::dvec2> screenPoints;
        int nDOF = 0;
        const Camera* camera = nullptr;
        const SceneGraphNode* node = nullptr;
        ghoul::LMstat stats;
    };

    /**
     * Extract the camera parameters from the lsit of parameters, `par`.
     *
     * \param par The list of parameters, in the order of orbit.x, orbit.y, zoom, roll,
     *        pan.x, pan.y. The number of parameters should match the number of degrees
     *        of freedom (`nDof`).
     * \param nDof The number of degrees of freedom, which determines how many parameters
     *        to read from `par` and how to interpret them. For example, if `nDof` is 2,
     *        only the first two parameters (orbit.x and orbit.y) will be read, and the
     *        rest will be ignored.
     * \return The VelocityStates struct containing the parsed camera parameters, with
     *         default values for the non-parsed parameters.
     */
    DirectManipulation::VelocityStates parseCameraParameterList(std::vector<double>& par,
                                                                int nDof)
    {
        ghoul_assert(par.size() >= 2, "There should be at least 2 camera parameters");

        DirectManipulation::VelocityStates result;
        result.orbit = glm::dvec2(par[0], par[1]);

        if (nDof > 2) {
            ghoul_assert(par.size() >= 4, "There should be at least 4 camera parameters");
            result.zoom = par[2];
            result.roll = par[3];

            if (nDof > 4) {
                ghoul_assert(par.size() == 6, "There should be 6 camera parameters");
                result.roll = 0.0;
                result.pan = glm::dvec2(par[4], par[5]);
            }
        }
        return result;
    }

    /**
     * Project back a 3D point in model view to clip space [-1, 1] coordinates on the view
     * plane.
     */
    glm::dvec2 castToNormalizedDeviceCoordinates(const glm::dvec3& pos,
                                                 const Camera& camera,
                                                 const SceneGraphNode* node)
    {
        glm::dvec3 posInCamSpace = glm::inverse(camera.rotationQuaternion()) *
            (node->worldRotationMatrix() * pos +
                (node->worldPosition() - camera.position()));

        glm::dvec4 clipspace = camera.projectionMatrix() * glm::dvec4(posInCamSpace, 1.0);
        return glm::dvec2(clipspace) / clipspace.w;
    }

    /**
     * Computes the residual for the Levenberg-Marquardt optimization. Given camera
     * parameters `par`, applies the camera transformation and projects the selected 3D
     * point (at index `x`) to screen space. Returns the distance between the projected
     * screen point and the target screen point. This distance is minimized during the
     * optimization to find camera parameters that keep touch points stationary on the
     * object's surface.
     */
    double distToMinimize(double* par, int x, void* fdata, ghoul::LMstat* lmstat) {
        FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);

        // Apply transform to camera and find the screen point of the updated camera state

        // { vec2 globalRot, zoom, roll, vec2 localRot }
        std::vector<double> q(6, 0.0);
        for (int i = 0; i < ptr->nDOF; i++) {
            q[i] = par[i];
        }

        DirectManipulation::VelocityStates velocities =
            parseCameraParameterList(q, ptr->nDOF);

        CameraPose pose = DirectManipulation::cameraPoseFromVelocities(
            velocities,
            ptr->camera,
            ptr->node
        );

        // Update the camera state (for a local copy of the camera)
        Camera camera = *ptr->camera;
        camera.setPose(pose);

        // We now have a new position and orientation of camera, project surfacePoint to
        // the new screen to get distance to minimize
        glm::dvec2 newScreenPoint = castToNormalizedDeviceCoordinates(
            ptr->selectedPoints.at(x),
            camera,
            ptr->node
        );
        lmstat->pos.push_back(newScreenPoint);
        return glm::length(ptr->screenPoints.at(x) - newScreenPoint);
    }

    /**
     * Gradient of distToMinimize w.r.t `par` (using forward difference).
     */
    void gradient(double* g, double* par, int x, void* fdata, ghoul::LMstat* lmstat) {
        FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
        double f0 = distToMinimize(par, x, fdata, lmstat);
        // Scale value to find minimum step size h, dependent on planet size
        double scale = std::log10(ptr->node->interactionSphere());
        int nDof = ptr->nDOF;

        std::vector<double> dPar(nDof, 0.0);
        dPar.assign(par, par + nDof);

        for (int i = 0; i < nDof; i++) {
            // Initial values
            double h = 1e-8;
            double prevDiff = 1;

            // Iterative process to find the minimum step `h` that gives a good gradient
            constexpr int MaxIterations = 100;
            for (int step = 0; step < MaxIterations; step++) {
                dPar[i] += h;
                double f1 = distToMinimize(dPar.data(), x, fdata, lmstat);
                dPar[i] = par[i];

                double diff = f1 - f0;

                // Found good minimum step size
                if (diff != 0 && prevDiff == 0) {
                    // Scale up to get a good initial guess value
                    h *= scale * scale * scale;

                    // Clamp min step size to a fraction of the incoming parameter
                    if (i == 2) {
                        // Zoom
                        // Make sure incoming parameter is larger than 0
                        constexpr double Epsilon = 1e-3;
                        h = std::max(std::max(std::abs(dPar.at(i)), Epsilon) * 0.001, h);
                    }
                    else if (nDof == 2) {
                        h = std::max(std::abs(dPar.at(i)) * 0.001, h);
                    }
                    break;
                }
                // Adapt step size (keep testing)
                if (diff != 0 && prevDiff != 0) {
                    // Step too big
                    h /= scale;
                }
                else if (diff == 0) {
                    // Step too small
                    h *= scale;
                }
                prevDiff = diff;
            }

            // Compute finite difference using the adapted step size `h`
            dPar[i] += h;
            double f1 = distToMinimize(dPar.data(), x, fdata, lmstat);
            dPar[i] = par[i];
            g[i] = (f1 - f0) / h;
        }

        if (nDof == 2) {
            // 1-finger: Normalize to allow for horizontal/vertical movement
            for (int i = 0; i < nDof; i++) {
                g[i] = g[i] / std::abs(g[i]);
            }
        }
        else if (nDof == 6) {
            // 3-fingers: Lock to only pan and zoom (no roll/orbit)
            for (int i = 0; i < nDof; i++) {
                g[i] = (i == 2) ? g[i] : g[i] / std::abs(g[i]);
            }
        }
    }

    constexpr Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enable direct manipulation",
        "Decides whether the direct manipulation mode should be enabled or not.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo IsActiveInfo = {
        "IsActive",
        "Is active",
        "True if the direct manipulation interaction scheme is currently being applied.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo AllowMouseInputInfo = {
        "AllowMouseInput",
        "Allow mouse input",
        "Decides whether direct manipulation should be applied when using mouse input."
        "Otherwise, it is only applied for touch input.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo DistanceThresholdInfo = {
        "DistanceThreshold",
        "Distance threshold factor",
        "This threshold affects the distance from the interaction sphere at which the "
        "direct manipulation interaction mode starts being active. The value is given as "
        "a factor times the interaction sphere.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo DefaultRenderableTypesInfo = {
        "DefaultRenderableTypes",
        "Default renderable types",
        "A list of renderable types that will automatically use the direct manipulation "
        "scheme when interacted with, keeping the finger on a static position on the "
        "interaction sphere of the object when touching. Good for relatively spherical "
        "objects. Per default, it's set to RenderableGlobes.",
        Property::Visibility::AdvancedUser
    };

    constexpr std::string_view Description =
        "Direct manipulation is an interaction scheme that allows rotating an object in "
        "a way so that, while touching, each finger will kept on a static position on "
        "the interaction sphere of the object. Per default, the scheme is only active "
        "for touch interaction. Note that the camera has to be within a certain distance "
        "of the object, and that direct manipulation will only be applied for specified "
        "renderable types.";
} // namespace

namespace openspace {

DirectManipulation::DirectManipulation()
    : PropertyOwner({
        "DirectManipulation",
        "Direct Manipulation",
        std::string(Description)
      })
    , _enabled(EnabledInfo, true)
    , _isActive(IsActiveInfo, false)
    , _allowMouseInput(AllowMouseInputInfo, false)
    , _distanceThreshold(DistanceThresholdInfo, 5.f, 0.f, 10.f)
    , _defaultRenderableTypes(DefaultRenderableTypesInfo)
{
    addProperty(_enabled);

    _isActive.setReadOnly(true);
    addProperty(_isActive);

    addProperty(_allowMouseInput);

    addProperty(_distanceThreshold);

    // @TODO (2026-03-31, emmbr) This is a bit of a hack, to apply this setting to
    // RenderableGlobes per default. It's done manually before the onchange, as the
    // RenderableGlobe type has not yet been registered when this constructor is run, so
    // the existance check fails... In the future we want to remove this and ideally,
    // this property should not be needed at all. Direct manipulation should be made to
    // work fine for all renderable types
    _defaultRenderableTypes = std::vector<std::string>({ "RenderableGlobe" });
    _sortedDefaultRenderableTypes.insert("RenderableGlobe");

    _defaultRenderableTypes.onChange([this]() {
        _sortedDefaultRenderableTypes.clear();
        for (const std::string& s : _defaultRenderableTypes.value()) {
            ghoul::TemplateFactory<Renderable>* fRenderable =
                FactoryManager::ref().factory<Renderable>();

            if (!fRenderable->hasClass(s)) {
                LWARNING(std::format(
                    "In property 'DefaultDirectTouchRenderableTypes': '{}' is not a "
                    "registered renderable type. Ignoring", s
                ));
                continue;
            }

            _sortedDefaultRenderableTypes.insert(s);
        }
    });
    addProperty(_defaultRenderableTypes);

    global::callback::touchDetected->emplace_back(
        [this](TouchInput input) {
            const SceneGraphNode* anchor = global::navigationHandler->anchorNode();
            ghoul_assert(anchor != nullptr, "Must have an anchor");

            std::optional<glm::dvec3> surfacePoint =
                computeSurfacePoint(input.pos, anchor);

            // Check if this and all previous points are valid
            _isValidFirstTouch = surfacePoint.has_value() &&
                std::all_of(
                    _firstTouchPoints.begin(),
                    _firstTouchPoints.end(),
                    [](const FirstTouchPoint& ftp) { return ftp.isValid; }
                );

            _firstTouchPoints.push_back({
                .id = input.fingerId,
                .isValid = surfacePoint.has_value()
            });

            return false;
        }
    );

    global::callback::touchExit->emplace_back(
        [this](TouchInput input) {
            auto it = std::find_if(
                _firstTouchPoints.begin(),
                _firstTouchPoints.end(),
                [&input](const FirstTouchPoint& point) {
                    return point.id == input.fingerId;
                }
            );

            if (it != _firstTouchPoints.end()) {
                _firstTouchPoints.erase(it);
            }
            return false;
        }
    );
}

void DirectManipulation::updateCameraFromInput() {
    std::vector<TouchInputHolder> touchInputs =
        global::interactionHandler->touchInputState().touchPoints();

    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();
    ghoul_assert(anchor != nullptr, "Must have an anchor");

    std::vector<TouchPoint> touchPoints;
    touchPoints.reserve(touchInputs.size());
    for (const TouchInputHolder& input : touchInputs) {
        touchPoints.push_back({
            .id = input.latestInput().fingerId,
            .position = glm::dvec2(input.latestInput().pos)
        });
    }

    if (_allowMouseInput && touchPoints.empty()) {
        // Translate mouse input to touch input
        const MouseInputState& mouseState = global::interactionHandler->mouseInputState();
        MouseButton primaryButton = global::interactionHandler->primaryMouseButton();

        if (mouseState.isMouseButtonPressed(primaryButton)) {
            glm::dvec2 mousePos = mouseState.mousePosition();
            const glm::ivec2 screenSize = global::windowDelegate->currentWindowSize();
            mousePos.x /= static_cast<double>(screenSize.x);
            mousePos.y /= static_cast<double>(screenSize.y);

            if (global::interactionHandler->isMouseFirstPress()) {
                std::optional<glm::dvec3> p = computeSurfacePoint(mousePos, anchor);
                _isValidFirstTouch = p.has_value();
            }

            touchPoints.push_back({ .position = mousePos });
        }
    }

    if (!_enabled || touchPoints.empty()) {
        // No fingers, no input
        _selectedNodeSurfacePoints.clear();
        _isActive = false;
        _isValidFirstTouch = true;
        return;
    }

    bool isValidNode = isValidDirectTouchNode();
    bool isCloseEnough = isWithinDirectTouchDistance();

    bool isValidTouchPoints = _isValidFirstTouch &&
        !_selectedNodeSurfacePoints.empty() &&
        touchPoints.size() == _selectedNodeSurfacePoints.size() &&
        std::equal(
            touchPoints.begin(),
            touchPoints.end(),
            _selectedNodeSurfacePoints.begin(),
            [](TouchPoint& tp, SelectedBody& sb) {
                return tp.id == sb.id;
            }
        );

    bool shouldApply = isCloseEnough && isValidNode && isValidTouchPoints;

    if (shouldApply) {
        applyDirectControl(touchPoints);
        _isActive = true;
    }
    else {
        _isActive = false;
    }

    if (isCloseEnough && isValidNode) {
        updateNodeSurfacePoints(touchPoints);
    }
}

void DirectManipulation::applyDirectControl(const std::vector<TouchPoint>& touchPoints) {
    Camera* camera = global::navigationHandler->camera();
    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();

    if (!anchor || !camera) {
        return;
    }

    // Find best transform values for the new camera state
    std::optional<VelocityStates> result = solveVelocitiesFromTouchPoints(
        touchPoints,
        *camera
    );

    if (!result.has_value()) {
        return;
    }

    // If good values were found set new camera state
    VelocityStates velocities = *result;

    CameraPose pose = cameraPoseFromVelocities(velocities, camera, anchor);

    camera->setPose(pose);

    // Mark that a camera interaction happened and then reset velocities
    global::navigationHandler->orbitalNavigator().markCameraInteraction();
    global::navigationHandler->orbitalNavigator().resetVelocities();
}

void DirectManipulation::updateNodeSurfacePoints(
                                               const std::vector<TouchPoint>& touchPoints)
{
    _selectedNodeSurfacePoints.clear();

    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();

    for (const TouchPoint& touchPoint : touchPoints) {
        std::optional<glm::dvec3> surfacePoint =
            computeSurfacePoint(touchPoint.position, anchor);

        if (surfacePoint.has_value()) {
            // Note that node is saved as the direct input solver was initially
            // implemented to handle touch contact points on multiple nodes
            _selectedNodeSurfacePoints.push_back({
                .id = touchPoint.id,
                .node = anchor,
                .coordinates = *surfacePoint
            });
        }
    }
}

std::optional<glm::dvec3> DirectManipulation::computeSurfacePoint(
                                                          const glm::dvec2& touchPosition,
                                                         const SceneGraphNode* node) const
{
    const Camera* camera = global::navigationHandler->camera();
    const glm::dquat camRotation = camera->rotationQuaternion();
    const glm::dvec3 camPos = camera->position();

    // Normalized -1 to 1 coordinates on screen
    const double xCo = 2.0 * (touchPosition.x - 0.5);
    const double yCo = -2.0 * (touchPosition.y - 0.5);

    glm::dvec2 co = 2.0 * glm::dvec2(touchPosition - 0.5);
    co.y *= -1.0;

    const glm::dvec3 cursorInWorldSpace = camRotation *
        glm::dvec3(glm::inverse(camera->projectionMatrix()) *
            glm::dvec4(xCo, yCo, -1.0, 1.0));

    const glm::dvec3 rayDirection = glm::normalize(cursorInWorldSpace);

    // Compute positions on anchor node, by checking if touch input intersect
    // interaction sphere
    double intersectionDist = 0.0;
    const double interactionSphere = node->interactionSphere();
    const bool intersected = glm::intersectRaySphere(
        camPos,
        rayDirection,
        node->worldPosition(),
        interactionSphere * interactionSphere,
        intersectionDist
    );

    if (intersected) {
        glm::dvec3 intersectionPos = camPos + rayDirection * intersectionDist;
        glm::dvec3 pointInModelView = glm::inverse(node->worldRotationMatrix()) *
            (intersectionPos - node->worldPosition());

        return pointInModelView;
    }

    return std::nullopt;
}

CameraPose DirectManipulation::cameraPoseFromVelocities(const VelocityStates& velocities,
                                                        const Camera* camera,
                                                        const SceneGraphNode* anchor)
{
    ghoul_assert(camera != nullptr, "Camera must not be null");
    ghoul_assert(anchor != nullptr, "Anchor node must not be null");

    const glm::dvec3 anchorPos = anchor->worldPosition();

    CameraPose pose = camera->pose();

    // Make a representation of the rotation quaternion with local and global rotations
    CameraRotationDecomposition rot = decomposeCameraRotation(pose, anchorPos);

    {
        // Roll (local rotation)
        const glm::dvec3 zAxis = glm::dvec3(0.0, 0.0, 1.0);
        const glm::dquat camRollRot = glm::angleAxis(velocities.roll, zAxis);
        rot.localRotation = rot.localRotation * camRollRot;
    }
    {
        // Panning (local rotation)
        const glm::dvec3 eulerAngles =
            glm::dvec3(velocities.pan.y, velocities.pan.x, 0.0);

        const glm::dquat rotationDiff = glm::dquat(eulerAngles);
        rot.localRotation = rot.localRotation * rotationDiff;
    }
    {
        // Orbit (global rotation)

        // Rotate position
        const glm::dvec3 eulerAngles =
            glm::dvec3(velocities.orbit.y, velocities.orbit.x, 0.0);

        const glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);
        const glm::dquat rotationDiffWorldSpace =
            rot.globalRotation * rotationDiffCamSpace * glm::inverse(rot.globalRotation);

        const glm::dvec3 centerToCamera = pose.position - anchorPos;
        const glm::dvec3 rotationDiffVec3 =
            centerToCamera * rotationDiffWorldSpace - centerToCamera;

        pose.position += rotationDiffVec3;

        // Rotate camera to look at center again
        const glm::dvec3 newPositionToCenter = anchorPos - pose.position;
        const glm::dvec3 lookUpWhenFacingCenter = rot.globalRotation *
            camera->lookUpVectorCameraSpace();

        rot.globalRotation = ghoul::lookAtQuaternion(
            glm::dvec3(0.0),
            newPositionToCenter,
            lookUpWhenFacingCenter
        );
    }
    {
        // Zooming
        const glm::dvec3 directionToCenter = normalize(anchorPos - pose.position);
        pose.position += directionToCenter * velocities.zoom;
    }

    pose.rotation = composeCameraRotation(rot);

    return pose;
}

bool DirectManipulation::isValidDirectTouchNode() const {
    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();

    if (!anchor || !anchor->renderable()) {
        return false;
    }

    // Check if current anchor is valid for direct touch
    std::string renderableType = std::string(anchor->renderable()->typeAsString());

    bool isDirectTouchRenderable = _sortedDefaultRenderableTypes.find(renderableType) !=
        _sortedDefaultRenderableTypes.end();

    return anchor->supportsDirectInteraction() || isDirectTouchRenderable;
}

bool DirectManipulation::isWithinDirectTouchDistance() const {
    const SceneGraphNode* anchor = global::navigationHandler->anchorNode();
    const Camera* camera = global::navigationHandler->camera();

    if (!anchor || !camera) {
        return false;
    }

    const double interactionSphere = anchor->interactionSphere();
    const glm::dvec3 centerToCamera = camera->position() - anchor->worldPosition();

    // Check if camera is within distance for direct manipulation to be applicable
    if (interactionSphere > 0.0) {
        const double distance = std::max(
            length(centerToCamera) - interactionSphere,
            0.0
        );
        const double maxDistance = interactionSphere * _distanceThreshold;
        return distance <= maxDistance;
    }

    return false;
}

std::optional<DirectManipulation::VelocityStates>
DirectManipulation::solveVelocitiesFromTouchPoints(
                           const std::vector<DirectManipulation::TouchPoint>& touchPoints,
                                                                     const Camera& camera)
{
    ZoneScopedN("Direct touch input solver");

    ghoul_assert(
        _selectedNodeSurfacePoints.size() >= touchPoints.size(),
        "Number of touch inputs must match the number of 'selected bodies'"
    );

    // Initialize LM solver
    ghoul::LMstat lmStat;
    initializeLevmarqStats(&lmStat);

    int nFingers = std::min(static_cast<int>(touchPoints.size()), 3);
    int nDof = std::min(nFingers * 2, 6);

    // Parse input data to be used in the LM algorithm
    std::vector<glm::dvec3> selectedPoints;
    std::vector<glm::dvec2> screenPoints;

    for (int i = 0; i < nFingers; i++) {
        const SelectedBody& sb = _selectedNodeSurfacePoints.at(i);
        selectedPoints.push_back(sb.coordinates);
        screenPoints.emplace_back(
            2.0 * (touchPoints[i].position.x - 0.5),
            -2.0 * (touchPoints[i].position.y - 0.5)
        );
    }

    FunctionData fData = {
        .selectedPoints = selectedPoints,
        .screenPoints = screenPoints,
        .nDOF = nDof,
        .camera = &camera,
        .node = _selectedNodeSurfacePoints.at(0).node,
        .stats = lmStat
    };

    // Find best transform values for the new camera state and store them in parameters
    std::vector<double> param(6, 0.0);

    bool lmSuccess = ghoul::levmarq(
        nDof,
        param.data(),
        static_cast<int>(screenPoints.size()),
        nullptr,
        distToMinimize,
        gradient,
        reinterpret_cast<void*>(&fData),
        &lmStat
    );

    if (!lmSuccess) {
        return std::nullopt;
    }

    return parseCameraParameterList(param, nDof);
}

} // namespace openspace
