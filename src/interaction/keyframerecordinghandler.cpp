/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/interaction/sessionrecordinghandler.h>
#include <openspace/network/messagestructureshelper.h>
#include <openspace/util/timemanager.h>

#include "keyframerecordinghandler_lua.inl"

namespace openspace::interaction {

KeyframeRecordingHandler::KeyframeRecordingHandler()
    : properties::PropertyOwner({ "KeyframeRecording", "Keyframe Recording" })
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

scripting::LuaLibrary KeyframeRecordingHandler::luaLibrary() {
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
            codegen::lua::Keyframes
        }
    };
}

} // namespace openspace::interaction
