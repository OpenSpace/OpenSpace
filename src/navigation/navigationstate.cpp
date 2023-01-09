/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
    constexpr std::string_view _loggerCat = "NavigationState";

    constexpr double Epsilon = 1E-7;

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
            "Could not find scene graph node '{}' used as anchor", anchor
        ));
        return CameraPose();
    }

    if (!referenceFrameNode) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' used as reference frame",
            referenceFrame
        ));
        return CameraPose();
    }

    CameraPose resultingPose;

    const glm::dmat3 referenceFrameTransform = referenceFrameNode->modelTransform();

    resultingPose.position = anchorNode->worldPosition() +
        referenceFrameTransform * glm::dvec3(position);

    glm::dvec3 upVector = up.has_value() ?
        glm::normalize(referenceFrameTransform * *up) :
        glm::dvec3(0.0, 1.0, 0.0);

    // Construct vectors of a "neutral" view, i.e. when the anchor is centered in view
    glm::dvec3 neutralView =
        glm::normalize(anchorNode->worldPosition() - resultingPose.position);

    glm::dquat neutralCameraRotation = glm::inverse(glm::quat_cast(glm::lookAt(
        glm::dvec3(0.0),
        neutralView,
        upVector
    )));

    glm::dquat pitchRotation = glm::angleAxis(pitch, glm::dvec3(1.0, 0.0, 0.0));
    glm::dquat yawRotation = glm::angleAxis(yaw, glm::dvec3(0.0, -1.0, 0.0));

    resultingPose.rotation = neutralCameraRotation * yawRotation * pitchRotation;

    return resultingPose;
}

ghoul::Dictionary NavigationState::dictionary() const {
    ghoul::Dictionary cameraDict;
    cameraDict.setValue("Position", position);
    cameraDict.setValue("Anchor", anchor);

    if (anchor != referenceFrame) {
        cameraDict.setValue("ReferenceFrame", referenceFrame);
    }
    if (!aim.empty()) {
        cameraDict.setValue("Aim", aim);
    }
    if (up.has_value()) {
        cameraDict.setValue("Up", *up);

        if (std::abs(yaw) > Epsilon) {
            cameraDict.setValue("Yaw", yaw);
        }
        if (std::abs(pitch) > Epsilon) {
            cameraDict.setValue("Pitch", pitch);
        }
    }

    return cameraDict;
}

documentation::Documentation NavigationState::Documentation() {
    return codegen::doc<Parameters>("core_navigation_state");
}

} // namespace openspace::interaction
