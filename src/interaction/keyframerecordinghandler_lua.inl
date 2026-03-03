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

#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <optional>
#include <utility>

using namespace openspace;

namespace {

// Starts a new sequence of keyframes, any previously loaded sequence is discarded
[[codegen::luawrap]] void newSequence() {
    global::keyframeRecording->newSequence();
}

// Adds a keyframe at the specified sequence-time
[[codegen::luawrap]] void addCameraKeyframe(double sequenceTime) {
    global::keyframeRecording->addCameraKeyframe(sequenceTime);
}

// Adds a keyframe at the specified sequence-time
[[codegen::luawrap]] void addScriptKeyframe(double sequenceTime, std::string script) {
    global::keyframeRecording->addScriptKeyframe(sequenceTime, std::move(script));
}

// Removes a keyframe at the specified 0-based index
[[codegen::luawrap]] void removeKeyframe(int index) {
    global::keyframeRecording->removeKeyframe(index);
}

// Update the camera position at keyframe specified by the 0-based index
[[codegen::luawrap]] void updateKeyframe(int index) {
    global::keyframeRecording->updateKeyframe(index);
}

// Move keyframe of `index` to the new specified `sequenceTime`
[[codegen::luawrap]] void moveKeyframe(int index, double sequenceTime) {
    global::keyframeRecording->moveKeyframe(index, sequenceTime);
}

// Saves the current sequence of keyframes to disk by the optionally specified `filename`.
[[codegen::luawrap]] void saveSequence(std::filesystem::path filename) {
    global::keyframeRecording->saveSequence(std::move(filename));
}

// Loads a sequence from the specified file
[[codegen::luawrap]] void loadSequence(std::filesystem::path filename) {
    global::keyframeRecording->loadSequence(std::move(filename));
}

// Playback sequence optionally from the specified `sequenceTime` or if not specified
// starts playing from the current time set within the sequence
[[codegen::luawrap]] void play(std::optional<double> sequenceTime) {
    global::keyframeRecording->play();
    if (sequenceTime.has_value()) {
        global::sessionRecordingHandler->seek(*sequenceTime);
    }
}

// Pauses a playing sequence
[[codegen::luawrap]] void pause() {
    global::sessionRecordingHandler->setPlaybackPause(true);
}

// Returns `true` if there currently is a sequence loaded, otherwise `false`
[[codegen::luawrap]] bool hasKeyframeRecording() {
    return global::keyframeRecording->hasKeyframeRecording();
}

[[codegen::luawrap]] std::vector<ghoul::Dictionary> keyframes() {
    return global::keyframeRecording->keyframes();
}

[[codegen::luawrap]] void loadCameraFBX(std::filesystem::path filename,
    std::optional<std::string> focusNode, std::optional<std::string> dateTime,
    std::optional<double> scale) {

    std::string focus;
    if (focusNode.has_value()) {
        focus = *focusNode;
    }
    else {
        // Get current focus
        const SceneGraphNode* anchorNode =
            global::navigationHandler->orbitalNavigator().anchorNode();

        if (!anchorNode) {
            throw ghoul::RuntimeError("Unable to get current anchor node");
        }
        focus = anchorNode->identifier();
    }

    double sequenceTime = 0.0;
    // Use the specified time otherwise fallback to current simulation time
    if (dateTime.has_value()) {
        const Time time = Time(*dateTime);
        sequenceTime = time.j2000Seconds();
    }
    else {
        sequenceTime = global::timeManager->time().j2000Seconds();
    }


    global::keyframeRecording->loadCameraFBX(
        std::move(filename),
        focus,
        sequenceTime,
        scale.value_or(1.0)
    );
}

} // namespace

#include "keyframerecordinghandler_lua_codegen.cpp"
