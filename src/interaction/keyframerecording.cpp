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

#include <algorithm>
#include <iostream>

#include <glm/glm.hpp>

#include <ghoul/filesystem/filesystem.h>

#include <openspace/interaction/keyframerecording.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/timemanager.h>

#include "keyframerecording_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "KeyframeRecording";
} // namespace

namespace openspace::interaction {

KeyframeRecording::KeyframeRecording()
    : properties::PropertyOwner({ "KeyframeRecording", "Keyframe Recording" }) {

}

KeyframeRecording::~KeyframeRecording() {
}

bool KeyframeRecording::newSequence(std::string filename) {
    saveSequence();

    _keyframes.clear();
    _filename = filename;
    return true;
}

bool KeyframeRecording::addKeyframe(double sequenceTime) {

    nlohmann::json keyframe = newKeyframe(sequenceTime);

    auto it = std::find_if(_keyframes.begin(), _keyframes.end(),
        [sequenceTime](const nlohmann::json& entry) {
            return sequenceTime < static_cast<double>(entry["timestamp"]["sequence"]);
        }
    );
    _keyframes.insert(it, keyframe);

    return true;
}

bool KeyframeRecording::updateKeyframe(int index)
{
    nlohmann::json old = _keyframes.at(index);
    _keyframes[index] = newKeyframe(static_cast<double>(old["timestamp"]["sequence"]));
    return false;
}

bool KeyframeRecording::moveKeyframe(int index, double sequenceTime)
{
    _keyframes[index]["timestamp"]["sequence"] = sequenceTime;
    sortKeyframes();
    return false;
}

bool KeyframeRecording::saveSequence()
{
    if (_filename.length() < 1) {
        // @TODO(jockekilby) Throw some error about having to create sequence with a valid filename first
        return false;
    }

    nlohmann::json sequence = _keyframes;
    std::filesystem::path path = absPath("${RECORDINGS}/" + _filename + ".json");
    std::ofstream ofs(path);
    ofs << sequence.dump(2);
    ofs.close();
    return true;
}

bool KeyframeRecording::loadSequence(std::string filename)
{
    std::filesystem::path path = absPath("${RECORDINGS}/" + filename + ".json");
    std::ifstream file(path);
    _keyframes = nlohmann::json::parse(file).get<std::vector<nlohmann::json>>();
    _filename = filename;
    return true;
}

void KeyframeRecording::play() {
    _playing = true;
}

void KeyframeRecording::pause() {
    _playing = false;
}

void KeyframeRecording::setSequenceTime(double sequenceTime) {
    _sequenceTime = sequenceTime;
    _stateChanged = true;
}

void KeyframeRecording::preSynchronization(double dt) {
    if (_stateChanged) {
        auto it = std::find_if(
            _keyframes.rbegin(), _keyframes.rend(),
            [timestamp = _sequenceTime](const nlohmann::json& entry) {
                return timestamp >= static_cast<double>(entry["timestamp"]["sequence"]);
            }
        );

        nlohmann::json currKeyframe = nullptr;
        nlohmann::json nextKeyframe = nullptr;
        double factor = 0.0;

        // Before first keyframe
        if (it == _keyframes.rend()) {
            currKeyframe = nextKeyframe = _keyframes.front();
        }
        // At or after last keyframe
        else if(it == _keyframes.rbegin()) {
            currKeyframe = nextKeyframe = _keyframes.back();
            _playing = false;
        }
        else {
            currKeyframe = *it;
            nextKeyframe = *(--it);
            double t0 = currKeyframe["timestamp"]["sequence"];
            double t1 = nextKeyframe["timestamp"]["sequence"];
            factor = (_sequenceTime - t0) / (t1 - t0);
        }

        interaction::KeyframeNavigator::CameraPose curr = keyframeToPose(currKeyframe);
        interaction::KeyframeNavigator::CameraPose next = keyframeToPose(nextKeyframe);

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

        _stateChanged = false;
    }

    if (_playing) {
        _sequenceTime += dt;
        _stateChanged = true;
    }
}

scripting::LuaLibrary KeyframeRecording::luaLibrary() {
    return {
        "keyframeRecording",
        {
            codegen::lua::NewSequence,
            codegen::lua::AddKeyframe,
            codegen::lua::UpdateKeyframe,
            codegen::lua::MoveKeyframe,
            codegen::lua::SaveSequence,
            codegen::lua::LoadSequence,
            codegen::lua::Play,
            codegen::lua::Pause,
            codegen::lua::SetTime
        }
    };
}

void KeyframeRecording::sortKeyframes() {
    std::sort(_keyframes.begin(), _keyframes.end(),
        [](nlohmann::json lhs, nlohmann::json rhs) {
            return static_cast<double>(lhs["timestamp"]["sequence"]) <
                static_cast<double>(rhs["timestamp"]["sequence"]);
        });
}

nlohmann::json KeyframeRecording::newKeyframe(double sequenceTime)
{
    interaction::NavigationHandler& handler = *global::navigationHandler;
    interaction::OrbitalNavigator& navigator = handler.orbitalNavigator();
    const SceneGraphNode* node = navigator.anchorNode();
    if (!node) {
        // @TODO(jockekilby) Display error about erraneous state
        return false;
    }

    glm::dvec3 position = navigator.anchorNodeToCameraVector();
    glm::dquat rotation = handler.camera()->rotationQuaternion();
    float scale = handler.camera()->scaling();
    bool followNodeRotation = navigator.followingAnchorRotation();

    if (followNodeRotation) {
        position = glm::inverse(node->worldRotationMatrix()) * position;
        rotation = navigator.anchorNodeToCameraRotation();
    }

    nlohmann::json keyframe = {
        { "camera", {
                { "position", {
                        { "x", position.x },
                        { "y", position.y },
                        { "z", position.z },
                    }
                },
                { "rotation", {
                        { "x", rotation.x },
                        { "y", rotation.y },
                        { "z", rotation.z },
                        { "w", rotation.w },
                    }
                },
                { "scale", scale },
                { "followFocusNodeRotation", followNodeRotation },
                { "focusNode", navigator.anchorNode()->identifier() }
            },
        },
        { "timestamp", {
                { "application", global::windowDelegate->applicationTime() },
                { "sequence", sequenceTime },
                { "simulation", global::timeManager->time().j2000Seconds() }
            }
        }
    };

    return keyframe;
}

KeyframeNavigator::CameraPose KeyframeRecording::keyframeToPose(const nlohmann::json& keyframe) const
{
    KeyframeNavigator::CameraPose pose;

    pose.position.x = keyframe["camera"]["position"]["x"];
    pose.position.y = keyframe["camera"]["position"]["y"];
    pose.position.z = keyframe["camera"]["position"]["z"];

    pose.rotation.x = keyframe["camera"]["rotation"]["x"];
    pose.rotation.y = keyframe["camera"]["rotation"]["y"];
    pose.rotation.z = keyframe["camera"]["rotation"]["z"];
    pose.rotation.w = keyframe["camera"]["rotation"]["w"];

    pose.followFocusNodeRotation = keyframe["camera"]["followFocusNodeRotation"];
    pose.scale = keyframe["camera"]["scale"];
    pose.focusNode = keyframe["camera"]["focusNode"];
    return pose;
}

} // namespace openspace::interaction
