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

namespace openspace::interaction {

void to_json(nlohmann::json& j, const KeyframeRecording::Keyframe& keyframe) {
    j = {
        { "camera", {
                { "position", {
                        { "x", keyframe.camera.position.x },
                        { "y", keyframe.camera.position.y },
                        { "z", keyframe.camera.position.z },
                    }
                },
                { "rotation", {
                        { "x", keyframe.camera.rotation.x },
                        { "y", keyframe.camera.rotation.y },
                        { "z", keyframe.camera.rotation.z },
                        { "w", keyframe.camera.rotation.w },
                    }
                },
                { "scale", keyframe.camera.scale },
                { "followFocusNodeRotation", keyframe.camera.followFocusNodeRotation },
                { "focusNode", keyframe.camera.focusNode }
            },
        },
        { "timestamp", {
                { "application", keyframe.timestamp.application },
                { "sequence", keyframe.timestamp.sequenceTime },
                { "simulation", keyframe.timestamp.simulation }
            }
        }
    };
}

void from_json(const nlohmann::json& j, KeyframeRecording::Keyframe::TimeStamp& ts) {
    j.at("application").get_to(ts.application);
    j.at("sequence").get_to(ts.sequenceTime);
    j.at("simulation").get_to(ts.simulation);
}

void from_json(const nlohmann::json& j, KeyframeNavigator::CameraPose& pose) {
    j.at("position").at("x").get_to(pose.position.x);
    j.at("position").at("y").get_to(pose.position.y);
    j.at("position").at("z").get_to(pose.position.z);

    j.at("rotation").at("x").get_to(pose.rotation.x);
    j.at("rotation").at("y").get_to(pose.rotation.y);
    j.at("rotation").at("z").get_to(pose.rotation.z);
    j.at("rotation").at("w").get_to(pose.rotation.w);

    j.at("focusNode").get_to(pose.focusNode);
    j.at("scale").get_to(pose.scale);
    j.at("followFocusNodeRotation").get_to(pose.followFocusNodeRotation);
}

void from_json(const nlohmann::json& j, KeyframeRecording::Keyframe& keyframe) {
    j.at("camera").get_to(keyframe.camera);
    j.at("timestamp").get_to(keyframe.timestamp);
}

KeyframeRecording::KeyframeRecording()
    : properties::PropertyOwner({ "KeyframeRecording", "Keyframe Recording" })
{}

void KeyframeRecording::newSequence() {
    _keyframes.clear();
    _filename.clear();
}

void KeyframeRecording::addKeyframe(double sequenceTime) {
    Keyframe keyframe = newKeyframe(sequenceTime);

    auto it = std::find_if(
        _keyframes.begin(),
        _keyframes.end(),
        [sequenceTime](const Keyframe& entry) {
            return sequenceTime < entry.timestamp.sequenceTime;
        }
    );
    _keyframes.insert(it, keyframe);
}

void KeyframeRecording::removeKeyframe(int index)
{
    if (index < 0 || static_cast<size_t>(index) >(_keyframes.size() - 1)) {
        LERROR(std::format("Index {} out of range", index));
        return;
    }
    _keyframes.erase(_keyframes.begin() + index);
}

void KeyframeRecording::updateKeyframe(int index) {
    if (index < 0 || static_cast<size_t>(index) > (_keyframes.size() - 1)) {
        LERROR(std::format("Index {} out of range", index));
        return;
    }
    Keyframe old = _keyframes[index];
    _keyframes[index] = newKeyframe(old.timestamp.sequenceTime);
}

void KeyframeRecording::moveKeyframe(int index, double sequenceTime) {
    if (index < 0 || static_cast<size_t>(index) >(_keyframes.size() - 1)) {
        LERROR(std::format("Index {} out of range", index));
        return;
    }

    _keyframes[index].timestamp.sequenceTime = sequenceTime;
    sortKeyframes();
}

bool KeyframeRecording::saveSequence(std::optional<std::string> filename) {
    // If we didn't specify any filename we save the one we currently have stored
    if (filename.has_value()) {
        _filename = filename.value();
    }

    if (_filename.empty()) {
        LERROR("Failed to save file, reason: Invalid empty file name");
        return false;
    }

    nlohmann::json sequence = _keyframes;
    std::filesystem::path path = absPath("${RECORDINGS}/" + _filename + ".json");
    std::ofstream ofs(path);
    ofs << sequence.dump(2);
    return true;
}

void KeyframeRecording::loadSequence(std::string filename) {
    std::filesystem::path path = absPath("${RECORDINGS}/" + filename + ".json");
    if (!std::filesystem::exists(path)) {
        LERROR(std::format("File '{}' does not exist", path));
        return;
    }

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
    _isPlaying = true;
}

void KeyframeRecording::pause() {
    _isPlaying = false;
}

void KeyframeRecording::setSequenceTime(double sequenceTime) {
    _sequenceTime = sequenceTime;
    _hasStateChanged = true;
}

bool KeyframeRecording::hasKeyframeRecording() const
{
    return !_keyframes.empty();
}

std::vector<ghoul::Dictionary> KeyframeRecording::getKeyframes() const
{
    std::vector<ghoul::Dictionary> result;
    for (const auto& keyframe : _keyframes) {
        ghoul::Dictionary camera;
        ghoul::Dictionary timestamp;
        ghoul::Dictionary entry;
        ghoul::Dictionary position;
        ghoul::Dictionary rotation;

        // Add each entry to position & rotation to avoid ambiguity on the client side
        position.setValue("x", keyframe.camera.position.x);
        position.setValue("y", keyframe.camera.position.y);
        position.setValue("z", keyframe.camera.position.z);
        camera.setValue("position", position);

        rotation.setValue("x", static_cast<double>(keyframe.camera.rotation.x));
        rotation.setValue("y", static_cast<double>(keyframe.camera.rotation.y));
        rotation.setValue("z", static_cast<double>(keyframe.camera.rotation.z));
        rotation.setValue("w", static_cast<double>(keyframe.camera.rotation.w));
        camera.setValue("rotation", rotation);

        camera.setValue("scale", static_cast<double>(keyframe.camera.scale));
        camera.setValue("focusNode", keyframe.camera.focusNode);
        camera.setValue("followFocusNodeRotation", keyframe.camera.followFocusNodeRotation);

        timestamp.setValue("application", keyframe.timestamp.application);
        timestamp.setValue("sequence", keyframe.timestamp.sequenceTime);
        timestamp.setValue("simulation", keyframe.timestamp.simulation);

        entry.setValue("camera", camera);
        entry.setValue("timestamp", timestamp);
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
        bool success = interaction::KeyframeNavigator::updateCamera(
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
            codegen::lua::SetTime,
            codegen::lua::GetKeyframes
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

} // namespace openspace::interaction
