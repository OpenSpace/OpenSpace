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

#include <openspace/interaction/sessionrecordinghandler.h>
#include <openspace/network/messagestructureshelper.h>
#include <openspace/util/timemanager.h>

#include "keyframerecording_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "KeyframeRecording";
} // namespace

namespace openspace::interaction {

KeyframeRecording::KeyframeRecording()
    : properties::PropertyOwner({ "KeyframeRecording", "Keyframe Recording" })
{}

void KeyframeRecording::newSequence() {
    _timeline = SessionRecording();
}

void KeyframeRecording::addCameraKeyframe(double sequenceTime) {
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

void KeyframeRecording::addScriptKeyframe(double sequenceTime, std::string script) {
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

void KeyframeRecording::removeKeyframe(int index) {
    if (index < 0 || static_cast<size_t>(index) >(_timeline.entries.size() - 1)) {
        throw ghoul::RuntimeError(std::format("Index {} out of range", index));
    }
    _timeline.entries.erase(_timeline.entries.begin() + index);
}

void KeyframeRecording::updateKeyframe(int index) {
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

void KeyframeRecording::moveKeyframe(int index, double sequenceTime) {
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

void KeyframeRecording::saveSequence(std::filesystem::path filename) {
    if (filename.empty()) {
        throw ghoul::RuntimeError("Failed to save file, reason: Invalid empty file name");
    }

    saveSessionRecording(filename, _timeline, DataMode::Ascii);
}

void KeyframeRecording::loadSequence(std::filesystem::path filename) {
    _timeline = loadSessionRecording(filename);
}

void KeyframeRecording::play() {
    global::sessionRecordingHandler->startPlayback(_timeline, false, false, std::nullopt);
}

bool KeyframeRecording::hasKeyframeRecording() const {
    return !_timeline.entries.empty();
}

std::vector<ghoul::Dictionary> KeyframeRecording::keyframes() const {
    return sessionRecordingToDictionary(_timeline);
}

scripting::LuaLibrary KeyframeRecording::luaLibrary() {
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
