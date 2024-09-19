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

#include <openspace/interaction/keyframerecording.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <algorithm>
#include <iostream>

#include "keyframerecording_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "KeyframeRecording";
} // namespace

namespace openspace::interaction::keys {
    // These keys are const char* since nlohmann::json do not support string_view in .at()
    constexpr const char* Camera = "camera";
    constexpr const char* Position = "position";
    constexpr const char* Rotation = "rotation";
    constexpr const char* Scale = "scale";
    constexpr const char* FollowFocusNodeRotation = "followFocusNodeRotation";
    constexpr const char* FocusNode = "focusNode";

    constexpr const char* Timestamp = "timestamp";
    constexpr const char* Application = "application";
    constexpr const char* Sequence = "sequence";
    constexpr const char* Simulation = "simulation";
    constexpr const char* X = "x";
    constexpr const char* Y = "y";
    constexpr const char* Z = "z";
    constexpr const char* W = "w";
}

namespace openspace::interaction {

void to_json(nlohmann::json& j, const KeyframeRecording::Keyframe& keyframe) {
    nlohmann::json position = {
        { keys::X, keyframe.camera.position.x },
        { keys::Y, keyframe.camera.position.y },
        { keys::Z, keyframe.camera.position.z },
    };
    nlohmann::json rotation = {
        { keys::X, keyframe.camera.rotation.x },
        { keys::Y, keyframe.camera.rotation.y },
        { keys::Z, keyframe.camera.rotation.z },
        { keys::W, keyframe.camera.rotation.w },
    };
    nlohmann::json camera = {
        { keys::Position, position },
        { keys::Rotation, rotation },
        { keys::Scale, keyframe.camera.scale },
        { keys::FollowFocusNodeRotation, keyframe.camera.followFocusNodeRotation },
        { keys::FocusNode, keyframe.camera.focusNode }
    };

    nlohmann::json timestamp = {
        { keys::Application, keyframe.timestamp.application },
        { keys::Sequence, keyframe.timestamp.sequenceTime },
        { keys::Simulation, keyframe.timestamp.simulation }
    };

    j = {
        { keys::Camera, camera },
        { keys::Timestamp, timestamp }
    };
}

void from_json(const nlohmann::json& j, KeyframeRecording::Keyframe::TimeStamp& ts) {
    j.at(keys::Application).get_to(ts.application);
    j.at(keys::Sequence).get_to(ts.sequenceTime);
    j.at(keys::Simulation).get_to(ts.simulation);
}

void from_json(const nlohmann::json& j, KeyframeNavigator::CameraPose& pose) {
    j.at(keys::Position).at(keys::X).get_to(pose.position.x);
    j.at(keys::Position).at(keys::Y).get_to(pose.position.y);
    j.at(keys::Position).at(keys::Z).get_to(pose.position.z);

    j.at(keys::Rotation).at(keys::X).get_to(pose.rotation.x);
    j.at(keys::Rotation).at(keys::Y).get_to(pose.rotation.y);
    j.at(keys::Rotation).at(keys::Z).get_to(pose.rotation.z);
    j.at(keys::Rotation).at(keys::W).get_to(pose.rotation.w);

    j.at(keys::FocusNode).get_to(pose.focusNode);
    j.at(keys::Scale).get_to(pose.scale);
    j.at(keys::FollowFocusNodeRotation).get_to(pose.followFocusNodeRotation);
}

void from_json(const nlohmann::json& j, KeyframeRecording::Keyframe& keyframe) {
    j.at(keys::Camera).get_to(keyframe.camera);
    j.at(keys::Timestamp).get_to(keyframe.timestamp);
}

KeyframeRecording::KeyframeRecording()
    : properties::PropertyOwner({ "KeyframeRecording", "Keyframe Recording" })
{}

void KeyframeRecording::newSequence() {
    _keyframes.clear();
    _filename.clear();
    LINFO("Created new sequence");
}

void KeyframeRecording::addKeyframe(double sequenceTime) {
    ghoul_assert(sequenceTime >= 0, "Sequence time must be positive");

    Keyframe keyframe = newKeyframe(sequenceTime);

    auto it = std::find_if(
        _keyframes.begin(),
        _keyframes.end(),
        [&sequenceTime](const Keyframe& entry) {
            return sequenceTime < entry.timestamp.sequenceTime;
        }
    );
    _keyframes.insert(it, keyframe);
    LINFO(std::format(
        "Added new keyframe {} at time: {}",
        _keyframes.size() - 1 , sequenceTime
    ));
}

void KeyframeRecording::removeKeyframe(int index) {
    ghoul_assert(hasKeyframeRecording(), "Can't remove keyframe on empty sequence");

    if (!isInRange(index)) {
        LERROR(std::format("Index {} out of range", index));
        return;
    }
    _keyframes.erase(_keyframes.begin() + index);
    LINFO(std::format("Removed keyframe with index {}", index));
}

void KeyframeRecording::updateKeyframe(int index) {
    ghoul_assert(hasKeyframeRecording(), "Can't update keyframe on empty sequence");

    if (!isInRange(index)) {
        LERROR(std::format("Index {} out of range", index));
        return;
    }
    Keyframe old = _keyframes[index];
    _keyframes[index] = newKeyframe(old.timestamp.sequenceTime);
     LINFO(std::format("Update camera position of keyframe {}", index));
}

void KeyframeRecording::moveKeyframe(int index, double sequenceTime) {
    ghoul_assert(hasKeyframeRecording(), "can't move keyframe on empty sequence");
    ghoul_assert(sequenceTime >= 0, "Sequence time must be positive");

    if (!isInRange(index)) {
        LERROR(std::format("Index {} out of range", index));
        return;
    }
    double oldSequenceTime = _keyframes[index].timestamp.sequenceTime;
    _keyframes[index].timestamp.sequenceTime = sequenceTime;
    sortKeyframes();
    LINFO(std::format(
        "Moved keyframe {} from sequence time: {} to {}",
        index, oldSequenceTime, sequenceTime
    ));
}

bool KeyframeRecording::saveSequence(std::optional<std::string> filename) {
    ghoul_assert(hasKeyframeRecording(), "Keyframe sequence can't be empty");

    // If we didn't specify any filename we save the one we currently have stored
    if (filename.has_value()) {
        _filename = filename.value();
    }

    if (_filename.empty()) {
        LERROR("Failed to save file, reason: Invalid empty file name");
        return false;
    }

    nlohmann::json sequence = _keyframes;
    std::filesystem::path path = absPath(
        std::format("${{RECORDINGS}}/{}.json", _filename)
    );
    std::ofstream ofs(path);
    ofs << sequence.dump(2);
    LINFO(std::format("Saved keyframe sequence to '{}'", path.string()));
    return true;
}

void KeyframeRecording::loadSequence(std::string filename) {
    std::filesystem::path path = absPath(
        std::format("${{RECORDINGS}}/{}.json", filename)
    );
    if (!std::filesystem::exists(path)) {
        LERROR(std::format("File '{}' does not exist", path));
        return;
    }

    LINFO(std::format("Loading keyframe sequence from '{}'", path));
    _keyframes.clear();
    std::ifstream file(path);
    std::vector<nlohmann::json> jsonKeyframes =
        nlohmann::json::parse(file).get<std::vector<nlohmann::json>>();

    for (const nlohmann::json& keyframeJson : jsonKeyframes) {
        Keyframe keyframe = keyframeJson;
        _keyframes.push_back(keyframe);
    }
    _filename = filename;
}

void KeyframeRecording::play() {
    ghoul_assert(hasKeyframeRecording(), "Keyframe sequence can't be empty");

    LINFO("Keyframe sequence playing");
    _isPlaying = true;
}

void KeyframeRecording::pause() {
    LINFO("Keyframe sequence paused");
    _isPlaying = false;
}

void KeyframeRecording::setSequenceTime(double sequenceTime) {
    ghoul_assert(sequenceTime >= 0, "Sequence time must be positive");

    _sequenceTime = sequenceTime;
    _hasStateChanged = true;
    LINFO(std::format("Set sequence time to {}", sequenceTime));
}

void KeyframeRecording::jumpToKeyframe(int index) {
    if (!isInRange(index)) {
        LERROR(std::format("Index {} out of range", index));
        return;
    }
    const double time = _keyframes[index].timestamp.sequenceTime;
    LINFO(std::format("Jumped to keyframe {}", index));
    setSequenceTime(time);
}

bool KeyframeRecording::hasKeyframeRecording() const {
    return !_keyframes.empty();
}

std::vector<ghoul::Dictionary> KeyframeRecording::keyframes() const {
    std::vector<ghoul::Dictionary> result;
    for (const auto& keyframe : _keyframes) {
        ghoul::Dictionary camera;
        ghoul::Dictionary timestamp;
        ghoul::Dictionary entry;
        ghoul::Dictionary position;
        ghoul::Dictionary rotation;

        // Add each entry to position & rotation to avoid ambiguity on the client side
        position.setValue(keys::X, keyframe.camera.position.x);
        position.setValue(keys::Y, keyframe.camera.position.y);
        position.setValue(keys::Z, keyframe.camera.position.z);
        camera.setValue(keys::Position, position);

        rotation.setValue(keys::X, static_cast<double>(keyframe.camera.rotation.x));
        rotation.setValue(keys::Y, static_cast<double>(keyframe.camera.rotation.y));
        rotation.setValue(keys::Z, static_cast<double>(keyframe.camera.rotation.z));
        rotation.setValue(keys::W, static_cast<double>(keyframe.camera.rotation.w));
        camera.setValue(keys::Rotation, rotation);

        camera.setValue(keys::Scale, static_cast<double>(keyframe.camera.scale));
        camera.setValue(keys::FocusNode, keyframe.camera.focusNode);
        camera.setValue(keys::FollowFocusNodeRotation, keyframe.camera.followFocusNodeRotation);

        timestamp.setValue(keys::Application, keyframe.timestamp.application);
        timestamp.setValue(keys::Sequence, keyframe.timestamp.sequenceTime);
        timestamp.setValue(keys::Simulation, keyframe.timestamp.simulation);

        entry.setValue(keys::Camera, camera);
        entry.setValue(keys::Timestamp, timestamp);
        result.push_back(entry);
    }
    return result;
}

void KeyframeRecording::preSynchronization(double dt) {
    if (_hasStateChanged) {
        auto it = std::find_if(
            _keyframes.rbegin(),
            _keyframes.rend(),
            [timestamp = _sequenceTime](const Keyframe& entry) {
                return timestamp >= entry.timestamp.sequenceTime;
            }
        );

        Keyframe currKeyframe;
        Keyframe nextKeyframe;
        double factor = 0.0;

        // Before first keyframe
        if (it == _keyframes.rend()) {
            currKeyframe = nextKeyframe = _keyframes.front();
        }
        // At or after last keyframe
        else if (it == _keyframes.rbegin()) {
            currKeyframe = nextKeyframe = _keyframes.back();
            _isPlaying = false;
        }
        else {
            currKeyframe = *it;
            nextKeyframe = *(--it);
            double t0 = currKeyframe.timestamp.sequenceTime;
            double t1 = nextKeyframe.timestamp.sequenceTime;
            factor = (_sequenceTime - t0) / (t1 - t0);
        }

        interaction::KeyframeNavigator::CameraPose curr = currKeyframe.camera;
        interaction::KeyframeNavigator::CameraPose next = nextKeyframe.camera;

        Camera* camera = global::navigationHandler->camera();
        Scene* scene = camera->parent()->scene();
        SceneGraphNode* node = scene->sceneGraphNode(curr.focusNode);

        global::navigationHandler->orbitalNavigator().setFocusNode(node);
        interaction::KeyframeNavigator::updateCamera(
            global::navigationHandler->camera(),
            curr,
            next,
            factor,
            false
        );

        _hasStateChanged = false;
    }

    if (_isPlaying) {
        _sequenceTime += dt;
        _hasStateChanged = true;
    }
}

scripting::LuaLibrary KeyframeRecording::luaLibrary() {
    return {
        "keyframeRecording",
        {
            codegen::lua::NewSequence,
            codegen::lua::AddKeyframe,
            codegen::lua::RemoveKeyframe,
            codegen::lua::UpdateKeyframe,
            codegen::lua::MoveKeyframe,
            codegen::lua::SaveSequence,
            codegen::lua::LoadSequence,
            codegen::lua::Play,
            codegen::lua::Pause,
            codegen::lua::Resume,
            codegen::lua::SetTime,
            codegen::lua::JumpToKeyframe,
            codegen::lua::HasKeyframeRecording,
            codegen::lua::Keyframes
        }
    };
}

void KeyframeRecording::sortKeyframes() {
    std::sort(
        _keyframes.begin(),
        _keyframes.end(),
        [](Keyframe lhs, Keyframe rhs) {
            return lhs.timestamp.sequenceTime < rhs.timestamp.sequenceTime;
        }
    );
}

KeyframeRecording::Keyframe KeyframeRecording::newKeyframe(double sequenceTime) {
    interaction::NavigationHandler& handler = *global::navigationHandler;
    interaction::OrbitalNavigator& navigator = handler.orbitalNavigator();
    const SceneGraphNode* node = navigator.anchorNode();

    glm::dvec3 position = navigator.anchorNodeToCameraVector();
    glm::dquat rotation = handler.camera()->rotationQuaternion();
    float scale = handler.camera()->scaling();
    bool followNodeRotation = navigator.followingAnchorRotation();

    if (followNodeRotation) {
        position = glm::inverse(node->worldRotationMatrix()) * position;
        rotation = navigator.anchorNodeToCameraRotation();
    }

    Keyframe keyframe;
    keyframe.camera.position = position;
    keyframe.camera.rotation = rotation;
    keyframe.camera.focusNode = navigator.anchorNode()->identifier();
    keyframe.camera.scale = scale;
    keyframe.camera.followFocusNodeRotation = followNodeRotation;

    keyframe.timestamp.application = global::windowDelegate->applicationTime();
    keyframe.timestamp.sequenceTime = sequenceTime;
    keyframe.timestamp.simulation = global::timeManager->time().j2000Seconds();

    return keyframe;
}

bool KeyframeRecording::isInRange(int index) const {
    return index >= 0 && index < _keyframes.size();
}

} // namespace openspace::interaction
