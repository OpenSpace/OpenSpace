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
#include <glm/gtc/quaternion.hpp>

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
        KeyframeNavigator::CameraPose(std::move(kf)),
        _id++
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
        std::move(script),
        _id++
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

void KeyframeRecordingHandler::removeKeyframeById(int id) {
    auto entry = std::find_if(
        _timeline.entries.begin(),
        _timeline.entries.end(),
        [id](const SessionRecording::Entry& e) {
            return e.id == id;
        }
    );

    if (entry != _timeline.entries.end()) {
        _timeline.entries.erase(entry);
    }
    else {
        throw ghoul::RuntimeError(std::format("Could not find keyframe with id '{}'", id));
    }
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

void KeyframeRecordingHandler::updateKeyframeById(int id) {
    auto entry = std::find_if(
        _timeline.entries.begin(),
        _timeline.entries.end(),
        [id](const SessionRecording::Entry& e) {
        return e.id == id;
    }
    );

    if (entry != _timeline.entries.end()) {
        int index = static_cast<int>(entry - _timeline.entries.begin());
        updateKeyframeById(index);
    }
    else {
        throw ghoul::RuntimeError(std::format("Could not find keyframe with id '{}'", id));
    }
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

void KeyframeRecordingHandler::moveKeyframeById(int id, double sequenceTime) {
    auto entry = std::find_if(
        _timeline.entries.begin(),
        _timeline.entries.end(),
        [id](const SessionRecording::Entry& e) {
            return e.id == id;
        }
    );

    if (entry != _timeline.entries.end()) {
        int index = static_cast<int>(entry - _timeline.entries.begin());
        moveKeyframe(index, sequenceTime);
    }
}

void KeyframeRecordingHandler::saveSequence(std::filesystem::path filename) {
    if (filename.empty()) {
        throw ghoul::RuntimeError("Failed to save file, reason: Invalid empty file name");
    }

    saveSessionRecording(filename, _timeline, DataMode::Ascii);
}

void KeyframeRecordingHandler::loadSequence(std::filesystem::path filename) {
    _timeline = loadSessionRecording(filename);
    _id = 1;
    for (auto& entry : _timeline.entries) {
        entry.id = _id++;
    }
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

SessionRecording::Entry::Camera interpolate(const SessionRecording::Entry::Camera& a,
                                            const SessionRecording::Entry::Camera& b,
                                            double t)

{
    // Linear interpolate for position and scale and slerp for quaternion rotation
    glm::dvec3 pos = glm::mix(a.position, b.position, t);
    double scale = glm::mix(a.scale, b.scale, t);
    glm::quat quat = glm::slerp(a.rotation, b.rotation, static_cast<float>(t));

    SessionRecording::Entry::Camera c;
    c.position = std::move(pos);
    c.rotation = std::move(quat);
    c.scale = scale;
    c.focusNode = a.focusNode;
    c.followFocusNodeRotation = a.followFocusNodeRotation;
    return c;
}

double isInterpolatedKeyframe(const SessionRecording::Entry::Camera& a,
    const SessionRecording::Entry::Camera& b)
{
    // a - b if all of the components are ideally = 0
    // abs(a - b) / b < some %
    double aLen = glm::length(a.position);
    double bLen = glm::length(b.position);
    double min = std::min(aLen, bLen);
    double max = std::max(aLen, bLen);
    double frac = min / max;
    // If we're within 1% error margin the position is regarded as interpolated
    bool isPositionInterpolated = (1.0 - frac) < 1;

    bool isScaleInterpolated = std::abs(a.scale - b.scale) < 1e4;

    //glm::angle()

    return isPositionInterpolated && isScaleInterpolated;

    double posDiff = glm::distance(a.position, b.position);
    //auto rotDiff = glm::
    return posDiff < 1e-5;
}

std::vector<ghoul::Dictionary> KeyframeRecordingHandler::reduceKeyframes() const {
    SessionRecording timeline;
    timeline.entries.reserve(_timeline.entries.size());
    const auto& entries = _timeline.entries;

    auto& r = timeline.entries;

    r.push_back(entries[0]);

    for (size_t i = 1; i < entries.size() - 1; i++) {
        const SessionRecording::Entry& A = entries[i - 1];
        const SessionRecording::Entry& B = entries[i];
        const SessionRecording::Entry& C = entries[i + 1];

        // Always add the scripts
        if (std::holds_alternative<SessionRecording::Entry::Script>(B.value)) {
            r.push_back(B);
            continue;
        }

        // A, B, and C are not all camera keyframes we cant interpolate so we need to add B
        if (std::holds_alternative<SessionRecording::Entry::Script>(A.value) ||
            std::holds_alternative<SessionRecording::Entry::Script>(C.value))
        {
            r.push_back(B);
            continue;
        }

        // If the timestamp is large between keyframes we don't want to lose potential
        // camera acceleration. E.g., If the movement between A and B is fast and
        // between B and C slow, we will lose temporal acceleration even if B is an
        // interpolated keyframe. TODO: potentially look at the acceleration between
        // keyframes? Also should the threshold be 1 second, less, more?
        // TODO: remove timestamps where the t value is between 0.4 and 0.6
        if (B.timestamp - A.timestamp > 1.0) {
            r.push_back(B);
            continue;
        }

        const auto& a = std::get<SessionRecording::Entry::Camera>(A.value);
        const auto& b = std::get<SessionRecording::Entry::Camera>(B.value);
        const auto& c = std::get<SessionRecording::Entry::Camera>(C.value);

        // If the keyframes are not on the same focus node keep B
        if (a.focusNode != b.focusNode || b.focusNode != c.focusNode) {
            r.push_back(B);
            continue;
        }

        // If the following changes we want to keep B
        if (a.followFocusNodeRotation != b.followFocusNodeRotation ||
            b.followFocusNodeRotation != c.followFocusNodeRotation)
        {
            r.push_back(B);
            continue;
        }
        // Compute where B lies in time between A and C
        const double t = (B.timestamp - A.timestamp) / (C.timestamp - A.timestamp);
        // Compute an interpolated keyframe
        SessionRecording::Entry::Camera interpolated = interpolate(a, c, t);
        // Compare the interpolated keyframe with the existing keyframe, we only keep
        // keyframes that are sufficiently different
        if (!isInterpolatedKeyframe(b, interpolated)) {
            r.push_back(B);
        }
    }

    r.push_back(entries[entries.size() - 1]);


    return sessionRecordingToDictionary(timeline);
}

scripting::LuaLibrary KeyframeRecordingHandler::luaLibrary() {
    return {
        "keyframeRecording",
        {
            codegen::lua::NewSequence,
            codegen::lua::AddCameraKeyframe,
            codegen::lua::AddScriptKeyframe,
            codegen::lua::RemoveKeyframe,
            codegen::lua::RemoveKeyframeById,
            codegen::lua::UpdateKeyframe,
            codegen::lua::UpdateKeyframeById,
            codegen::lua::MoveKeyframe,
            codegen::lua::MoveKeyframeById,
            codegen::lua::SaveSequence,
            codegen::lua::LoadSequence,
            codegen::lua::Play,
            codegen::lua::Pause,
            codegen::lua::Keyframes,
            codegen::lua::ReduceKeyframes
        }
    };
}

} // namespace openspace::interaction
