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

#include <openspace/interaction/keyframerecordinghandler.h>

#include <openspace/engine/globals.h>
#include <openspace/interaction/sessionrecordinghandler.h>
#include <openspace/navigation/keyframenavigator.h>
#include <openspace/network/messagestructures.h>
#include <openspace/network/messagestructureshelper.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/timemanager.h>
#include <ghoul/format.h>
#include <ghoul/io/camera/camerareader.h>
#include <ghoul/io/model/modelanimation.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <algorithm>
#include <optional>
#include <utility>
#include <variant>

#include "keyframerecordinghandler_lua.inl"

namespace openspace {

KeyframeRecordingHandler::KeyframeRecordingHandler()
    : PropertyOwner({ "KeyframeRecording", "Keyframe Recording" })
{}

void KeyframeRecordingHandler::newSequence() {
    _timeline = SessionRecording();
}

void KeyframeRecordingHandler::addCameraKeyframe(double sequenceTime) {
    using namespace datamessagestructures;
    CameraKeyframe kf = datamessagestructures::generateCameraKeyframe();

    SessionRecording::Entry entry = {
        sequenceTime,
        global::timeManager->time().j2000Seconds(),
        KeyframeNavigator::CameraPose(std::move(kf))
    };

    auto it = std::upper_bound(
        _timeline.entries.begin(),
        _timeline.entries.end(),
        sequenceTime,
        [](double value, const SessionRecording::Entry& e) {
            return value < e.timestamp;
        }
    );
    _timeline.entries.insert(it, std::move(entry));
}

void KeyframeRecordingHandler::addScriptKeyframe(double sequenceTime, std::string script)
{
    SessionRecording::Entry entry = {
        sequenceTime,
        global::timeManager->time().j2000Seconds(),
        std::move(script)
    };

    auto it = std::upper_bound(
        _timeline.entries.begin(),
        _timeline.entries.end(),
        sequenceTime,
        [](double value, const SessionRecording::Entry& e) {
            return value < e.timestamp;
        }
    );
    _timeline.entries.insert(it, std::move(entry));
}

void KeyframeRecordingHandler::removeKeyframe(int index) {
    if (index < 0 || static_cast<size_t>(index) >(_timeline.entries.size() - 1)) {
        throw ghoul::RuntimeError(std::format("Index {} out of range", index));
    }
    _timeline.entries.erase(_timeline.entries.begin() + index);
}

void KeyframeRecordingHandler::updateKeyframe(int index) {
    using namespace datamessagestructures;
    if (index < 0 || static_cast<size_t>(index) > (_timeline.entries.size() - 1)) {
        throw ghoul::RuntimeError(std::format("Index {} out of range", index));
    }

    SessionRecording::Entry& entry = _timeline.entries[index];
    if (!std::holds_alternative<SessionRecording::Entry::Camera>(entry.value)) {
        throw ghoul::RuntimeError(std::format("Index {} is not a camera frame", index));
    }
    auto& camera = std::get<SessionRecording::Entry::Camera>(entry.value);
    camera = KeyframeNavigator::CameraPose(generateCameraKeyframe());
}

void KeyframeRecordingHandler::moveKeyframe(int index, double sequenceTime) {
    if (index < 0 || static_cast<size_t>(index) >(_timeline.entries.size() - 1)) {
        throw ghoul::RuntimeError(std::format("Index {} out of range", index));
    }

    _timeline.entries[index].timestamp = sequenceTime;
    std::sort(
        _timeline.entries.begin(),
        _timeline.entries.end(),
        [](const SessionRecording::Entry& lhs, const SessionRecording::Entry& rhs) {
            return lhs.timestamp < rhs.timestamp;
        }
    );
}

void KeyframeRecordingHandler::saveSequence(std::filesystem::path filename) {
    if (filename.empty()) {
        throw ghoul::RuntimeError("Failed to save file, reason: Invalid empty file name");
    }

    saveSessionRecording(filename, _timeline, DataMode::Ascii);
}

void KeyframeRecordingHandler::loadSequence(std::filesystem::path filename) {
    _timeline = loadSessionRecording(filename);
}

void KeyframeRecordingHandler::play() {
    global::sessionRecordingHandler->startPlayback(_timeline, false, false, std::nullopt);
}

bool KeyframeRecordingHandler::hasKeyframeRecording() const {
    return !_timeline.entries.empty();
}

std::vector<ghoul::Dictionary> KeyframeRecordingHandler::keyframes() const {
    return sessionRecordingToDictionary(_timeline);
}

void KeyframeRecordingHandler::loadCameraFBX(const std::filesystem::path& path,
                                             const std::string& focusNode,
                                             double sequenceTime, double scale)
{
    using namespace ghoul::io;
     CameraReader::CameraPathData cameraPathData = CameraReader::loadCameraPath(
        path
    );
    ModelAnimation::NodeAnimation cameraPath = cameraPathData.animation->nodeAnimations()[0];
    SessionRecording timeline = SessionRecording();

    // It is not certain position, rotation and scale will have the same number of
    // keyframes
    const bool hasSameNumberOfKeyframes =
        cameraPath.positions.size() == cameraPath.rotations.size() &&
        cameraPath.positions.size() == cameraPath.scales.size();

    const size_t maxSafeIndex = std::min(
        std::min(cameraPath.positions.size(), cameraPath.rotations.size()),
        cameraPath.scales.size()
    );

    const size_t maxIndex = std::max(
        std::max(cameraPath.positions.size(), cameraPath.rotations.size()),
        cameraPath.scales.size()
    );
    if (!hasSameNumberOfKeyframes) {
        LWARNINGC("KeyframeRecording", std::format(
            "Camera path does not have the same nubmer of keyframes for position, "
            "rotation, and scale. Using {} keyframe(s) from total {} keyframes",
            maxSafeIndex, maxIndex
        ));
    }
    timeline.entries.reserve(maxSafeIndex);

    // @TODO (anden88 2026-03-03): Unclear if this is needed since the path is baked the
    // transforms might already be applied
    //glm::mat4 modelTransform = glm::mat4(1.f);
    //for (int i = 0; i <= cameraPath.node; i++) {
    //    const ghoul::io::ModelNode* node = &cameraNodes[i];
    //    if (cameraNodes[i].hasAnimation()) {
    //        modelTransform = modelTransform * node->animationTransform();
    //    }
    //    else {
    //        modelTransform = modelTransform * node->transform();
    //    }
    //}

    for (size_t i = 0; i < maxSafeIndex; i++) {
        SessionRecording::Entry entry;
        entry.timestamp = cameraPath.positions[i].time;
        entry.simulationTime = sequenceTime + entry.timestamp;

        SessionRecording::Entry::Camera camera;
        camera.focusNode = focusNode;
        camera.followFocusNodeRotation = false;
        glm::dvec3 pos =
            cameraPathData.unitScale *
            scale *
            static_cast<glm::dvec3>(cameraPath.positions[i].position);
        camera.position = pos;
        camera.rotation = cameraPath.rotations[i].rotation;
        camera.scale = cameraPath.scales[i].scale.x;

        entry.value = camera;
        timeline.entries.emplace_back(std::move(entry));
    }

    _timeline = std::move(timeline);
}

LuaLibrary KeyframeRecordingHandler::luaLibrary() {
    return {
        "keyframeRecording",
        {
            codegen::lua::NewSequence,
            codegen::lua::AddCameraKeyframe,
            codegen::lua::AddScriptKeyframe,
            codegen::lua::RemoveKeyframe,
            codegen::lua::UpdateKeyframe,
            codegen::lua::MoveKeyframe,
            codegen::lua::SaveSequence,
            codegen::lua::LoadSequence,
            codegen::lua::Play,
            codegen::lua::Pause,
            codegen::lua::Keyframes,
            codegen::lua::LoadCameraFBX
        }
    };
}

} // namespace openspace
