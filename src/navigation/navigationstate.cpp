/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/spicemanager.h>

namespace {
    constexpr std::string_view _loggerCat = "NavigationState";

    constexpr double Epsilon = 1E-7;

    // A NavigationState is an object describing an exact camera position and rotation,
    // in a certain reference frame (per default, the one of the specified Anchor node).
    // It can be used to set the same camera position at a later point in time, or
    // navigating to a specific camera position using the pathnavigation system.
    //
    // The camera rotation is specified using Euler angles, in radians. It is also
    // possible to specify a node to be used as Aim, but note that this will not affect
    // the actual camera position or view direction.
    //
    // To get the current navigation state of the camera, use the
    // `openspace.navigation.getNavigationState()` function in the Scripting API.
    //
    // Note that when loading a NavigationState, the visuals may be different depending
    // on what the simulation timestamp is, as the relative positions of objects in the
    // scene may have changed. The get the exact same visuals as when the NavigationState
    // was saved you need to also set the simulation time to correpsond to the timestamp.
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

        // The timestamp for when the navigation state was captured or is valid. Specified
        // either as seconds past the J2000 epoch, or as a date string in ISO 8601 format:
        // 'YYYY MM DD HH:mm:ss.xxx'
        std::optional<std::variant<double, std::string>> timestamp;
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

    up = p.up;
    yaw = p.yaw.value_or(yaw);
    pitch = p.pitch.value_or(pitch);

    if (p.timestamp.has_value()) {
        if (std::holds_alternative<double>(*p.timestamp)) {
            timestamp = std::get<double>(*p.timestamp);
        }
        else {
            timestamp = SpiceManager::ref().ephemerisTimeFromDate(
                std::get<std::string>(*p.timestamp)
            );
        }
    }
}

NavigationState::NavigationState(const nlohmann::json& json) {
    position.x = json["position"]["x"].get<double>();
    position.y = json["position"]["y"].get<double>();
    position.z = json["position"]["z"].get<double>();

    anchor = json["anchor"];

    if (auto it = json.find("referenceframe");  it != json.end()) {
        referenceFrame = it->get<std::string>();
    }
    else {
        referenceFrame = anchor;
    }

    if (auto it = json.find("aim");  it != json.end()) {
        aim = it->get<std::string>();
    }

    if (auto it = json.find("up");  it != json.end()) {
        up = glm::dvec3();
        up->x = it->at("x").get<double>();
        up->y = it->at("y").get<double>();
        up->z = it->at("z").get<double>();
    }

    if (auto it = json.find("yaw");  it != json.end()) {
        yaw = it->get<double>();
    }

    if (auto it = json.find("pitch");  it != json.end()) {
        pitch = it->get<double>();
    }

    if (auto it = json.find("timestamp");  it != json.end()) {
        if (it->is_string()) {
            timestamp = SpiceManager::ref().ephemerisTimeFromDate(
                it->get<std::string>()
            );
        }
        else {
            timestamp = it->get<double>();
        }
    }
}

NavigationState::NavigationState(std::string anchor_, std::string aim_,
                                 std::string referenceFrame_, glm::dvec3 position_,
                                 std::optional<glm::dvec3> up_,
                                 double yaw_, double pitch_,
                                 std::optional<double> timestamp_)
    : anchor(std::move(anchor_))
    , aim(std::move(aim_))
    , referenceFrame(std::move(referenceFrame_))
    , position(std::move(position_))
    , up(std::move(up_))
    , yaw(yaw_)
    , pitch(pitch_)
    , timestamp(timestamp_)
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

    // @TODO (2023-05-16, emmbr) This computation is wrong and has to be fixed! Only
    // works if the reference frame is also the anchor node. I remember that fixing it
    // was not as easy as using referenceFrameNode instead of anchor node though..
    resultingPose.position = anchorNode->worldPosition() +
        referenceFrameTransform * glm::dvec3(position);

    const glm::dvec3 upVector = up.has_value() ?
        glm::normalize(referenceFrameTransform * *up) :
        glm::dvec3(0.0, 1.0, 0.0);

    // Construct vectors of a "neutral" view, i.e. when the anchor is centered in view
    const glm::dvec3 neutralView =
        glm::normalize(anchorNode->worldPosition() - resultingPose.position);

    const glm::dquat neutralCameraRotation = glm::inverse(glm::quat_cast(
        glm::lookAt(glm::dvec3(0.0), neutralView, upVector)
    ));

    const glm::dquat pitchRotation = glm::angleAxis(pitch, glm::dvec3(1.0, 0.0, 0.0));
    const glm::dquat yawRotation = glm::angleAxis(yaw, glm::dvec3(0.0, -1.0, 0.0));

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
    }
    if (std::abs(yaw) > Epsilon) {
        cameraDict.setValue("Yaw", yaw);
    }
    if (std::abs(pitch) > Epsilon) {
        cameraDict.setValue("Pitch", pitch);
    }
    if (timestamp.has_value()) {
        cameraDict.setValue(
            "Timestamp",
            SpiceManager::ref().dateFromEphemerisTime(
                *timestamp,
                "YYYY MON DD HR:MN:SC"
            )
        );
    }
    return cameraDict;
}

nlohmann::json NavigationState::toJson() const {
    nlohmann::json result = nlohmann::json::object();

    // Obligatory version number
    result["version"] = 1;

    {
        nlohmann::json posObj = nlohmann::json::object();
        posObj["x"] = position.x;
        posObj["y"] = position.y;
        posObj["z"] = position.z;
        result["position"] = posObj;
    }

    result["anchor"] = anchor;

    if (anchor != referenceFrame) {
        result["referenceframe"] = referenceFrame;
    }

    if (!aim.empty()) {
        result["aim"] = aim;
    }

    if (up.has_value()) {
        nlohmann::json upObj = nlohmann::json::object();
        upObj["x"] = up->x;
        upObj["y"] = up->y;
        upObj["z"] = up->z;
        result["up"] = upObj;
    }

    if (std::abs(yaw) > Epsilon) {
        result["yaw"] = yaw;
    }
    if (std::abs(pitch) > Epsilon) {
        result["pitch"] = pitch;
    }

    if (timestamp.has_value()) {
        result["timestamp"] = SpiceManager::ref().dateFromEphemerisTime(
            *timestamp,
            "YYYY MON DD HR:MN:SC"
        );
    }

    return result;
}

documentation::Documentation NavigationState::Documentation() {
    return codegen::doc<Parameters>("core_navigation_state");
}

} // namespace openspace::interaction
