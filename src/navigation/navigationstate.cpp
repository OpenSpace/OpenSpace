/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/camera/camerapose.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>

namespace {
    constexpr const char* _loggerCat = "NavigationState";

    const double Epsilon = 1E-7;

    struct [[codegen::Dictionary(NavigationState)]] Parameters {
        // The identifier of the anchor node
        std::string anchor;

        // The identifier of the aim node, if used
        std::optional<std::string> aim;

        // The identifier of the scene graph node to use as reference frame. If not
        // specified, this will be the same as the anchor
        std::optional<std::string> referenceFrame;

        // The position of the camera relative to the anchor node, expressed in meters in
        // the specified reference frame
        glm::dvec3 position;

        // The up vector expressed in the coordinate system of the reference frame
        std::optional<glm::dvec3> up;

        // The yaw angle in radians. Positive angle means yawing camera to the right
        std::optional<double> yaw;

        // The pitch angle in radians. Positive angle means pitching camera upwards
        std::optional<double> pitch;
    };
#include "navigationstate_codegen.cpp"
} // namespace

namespace openspace::interaction {

NavigationState::NavigationState(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    anchor = p.anchor;
    position = p.position;

    referenceFrame = p.referenceFrame.value_or(anchor);
    aim = p.aim.value_or(aim);

    if (p.up.has_value()) {
        up = *p.up;

        yaw = p.yaw.value_or(yaw);
        pitch = p.pitch.value_or(pitch);
    }
}

NavigationState::NavigationState(std::string anchor_, std::string aim_,
                                 std::string referenceFrame_, glm::dvec3 position_,
                                 std::optional<glm::dvec3> up_,
                                 double yaw_, double pitch_)
    : anchor(std::move(anchor_))
    , aim(std::move(aim_))
    , referenceFrame(std::move(referenceFrame_))
    , position(std::move(position_))
    , up(std::move(up_))
    , yaw(yaw_)
    , pitch(pitch_)
{}

CameraPose NavigationState::cameraPose() const {
    const SceneGraphNode* referenceFrameNode = sceneGraphNode(referenceFrame);
    const SceneGraphNode* anchorNode = sceneGraphNode(anchor);

    if (!anchorNode) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' used as anchor.", referenceFrame
        ));
        return CameraPose();
    }
    if (!aim.empty() && !sceneGraphNode(aim)) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' used as aim.", referenceFrame
        ));
        return CameraPose();
    }
    if (!referenceFrameNode) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' used as reference frame.",
            referenceFrame)
        );
        return CameraPose();
    }

    CameraPose resultingPose;

    const glm::dvec3 anchorWorldPosition = anchorNode->worldPosition();
    const glm::dmat3 referenceFrameTransform = referenceFrameNode->worldRotationMatrix();

    resultingPose.position = anchorWorldPosition + referenceFrameTransform * position;

    glm::dvec3 upVector = up.has_value() ?
        glm::normalize(referenceFrameTransform * up.value()) :
        glm::dvec3(0.0, 1.0, 0.0);

    // Construct vectors of a "neutral" view, i.e. when the aim is centered in view.
    glm::dvec3 neutralView =
        glm::normalize(anchorWorldPosition - resultingPose.position);

    glm::dquat neutralCameraRotation = glm::inverse(glm::quat_cast(glm::lookAt(
        glm::dvec3(0.0),
        neutralView,
        upVector
    )));

    glm::dquat pitchRotation = glm::angleAxis(pitch, glm::dvec3(1.f, 0.f, 0.f));
    glm::dquat yawRotation = glm::angleAxis(yaw, glm::dvec3(0.f, -1.f, 0.f));

    resultingPose.rotation = neutralCameraRotation * yawRotation * pitchRotation;

    return resultingPose;
}

ghoul::Dictionary NavigationState::dictionary() const {
    constexpr const char* KeyAnchor = "Anchor";
    constexpr const char* KeyAim = "Aim";
    constexpr const char* KeyPosition = "Position";
    constexpr const char* KeyUp = "Up";
    constexpr const char* KeyYaw = "Yaw";
    constexpr const char* KeyPitch = "Pitch";
    constexpr const char* KeyReferenceFrame = "ReferenceFrame";

    ghoul::Dictionary cameraDict;
    cameraDict.setValue(KeyPosition, position);
    cameraDict.setValue(KeyAnchor, anchor);

    if (anchor != referenceFrame) {
        cameraDict.setValue(KeyReferenceFrame, referenceFrame);
    }
    if (!aim.empty()) {
        cameraDict.setValue(KeyAim, aim);
    }
    if (up.has_value()) {
        cameraDict.setValue(KeyUp, *up);

        if (std::abs(yaw) > Epsilon) {
            cameraDict.setValue(KeyYaw, yaw);
        }
        if (std::abs(pitch) > Epsilon) {
            cameraDict.setValue(KeyPitch, pitch);
        }
    }

    return cameraDict;
}

documentation::Documentation NavigationState::Documentation() {
    return codegen::doc<Parameters>("core_navigation_state");
}

} // namespace openspace::interaction
