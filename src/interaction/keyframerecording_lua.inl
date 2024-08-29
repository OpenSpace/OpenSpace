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

#include <openspace/engine/globals.h>
#include <optional>

namespace {

/**
 * Starts a new sequence of keyframes. Any previously loaded sequence is discarded.
 */
[[codegen::luawrap]] void newSequence() {
    openspace::global::keyframeRecording->newSequence();
}

/**
 * Adds a keyframe at the specified time in the sequence.
 *
 * \param sequenceTime The time at which to add the new keyframe in the sequence given in
 *                     seconds
 */
[[codegen::luawrap]] void addKeyframe(double sequenceTime) {
    if (sequenceTime < 0) {
        throw ghoul::lua::LuaError("Error can't add keyframe with negative time value");
    }
    openspace::global::keyframeRecording->addKeyframe(sequenceTime);
}

/**
 * Removes a keyframe at the specified index.
 *
 * \param index The 0-based index of the keyframe to remove
 */
[[codegen::luawrap]] void removeKeyframe(int index) {
    if (!openspace::global::keyframeRecording->hasKeyframeRecording()) {
        throw ghoul::lua::LuaError("Can't remove keyframe on empty sequence");
    }
    if (index < 0) {
        throw ghoul::lua::LuaError("Index value must be positive");
    }
    openspace::global::keyframeRecording->removeKeyframe(index);
}

/**
 * Update the camera position of a keyframe at the specified index.
 *
 * \param index The 0-based index of e keyframe to update
 */
[[codegen::luawrap]] void updateKeyframe(int index) {
    if (!openspace::global::keyframeRecording->hasKeyframeRecording()) {
        throw ghoul::lua::LuaError("Can't update keyframe on empty sequence");
    }
    if (index < 0) {
        throw ghoul::lua::LuaError("Index value must be positive");
    }
    openspace::global::keyframeRecording->updateKeyframe(index);
}

/**
 * Move an existing keyframe in time.
 *
 * \param index The index of the keyframe to move
 * \param sequenceTime The new time in seconds to update the keyframe to
 */
[[codegen::luawrap]] void moveKeyframe(int index, double sequenceTime) {
    if (!openspace::global::keyframeRecording->hasKeyframeRecording()) {
        throw ghoul::lua::LuaError("Can't move keyframe on empty sequence");
    }
    if (index < 0) {
        throw ghoul::lua::LuaError("Index value must be positive");
    }
    if (sequenceTime < 0) {
        throw ghoul::lua::LuaError("Error can't add keyframe with negative time value");
    }
    openspace::global::keyframeRecording->moveKeyframe(index, sequenceTime);
}

/**
 * Saves the current sequence of keyframes to disk by the optionally specified `filename`.
 * `filename` can be omitted if the sequence was previously saved or loaded from file.
 *
 * \param filename The name of the file to save
 */
[[codegen::luawrap]] void saveSequence(std::optional<std::string> filename) {
    if (!openspace::global::keyframeRecording->hasKeyframeRecording()) {
        throw ghoul::lua::LuaError("No keyframe sequence to save");
    }
    openspace::global::keyframeRecording->saveSequence(filename);
}

/**
 * Loads a keyframe recording sequence from the specified file.
 *
 * \param filename The name of the file to load
 */
[[codegen::luawrap]] void loadSequence(std::string filename) {
    openspace::global::keyframeRecording->loadSequence(std::move(filename));
}

/**
 * Playback keyframe recording sequence optionally from the specified `sequenceTime` or if
 * not specified starts playing from the beginning.
 *
 * \param sequenceTime The time in seconds at which to start playing the sequence. If
 *                     omitted, the playback starts at the beginning of the sequence.
 */
[[codegen::luawrap]] void play(std::optional<double> sequenceTime) {
    if (!openspace::global::keyframeRecording->hasKeyframeRecording()) {
        throw ghoul::lua::LuaError("No keyframe sequence to play");
    }
    openspace::global::keyframeRecording->setSequenceTime(sequenceTime.value_or(0.0));
    openspace::global::keyframeRecording->play();
}

/**
 * Pauses a playing keyframe recording sequence.
 */
[[codegen::luawrap]] void pause() {
    openspace::global::keyframeRecording->pause();
}

/**
 * Resume playing a keyframe recording sequence that has been paused.
 */
[[codegen::luawrap]] void resume() {
    openspace::global::keyframeRecording->play();
}

/**
 * Jumps to a specified time within the keyframe recording sequence.
 *
 * \param sequenceTime The time in seconds to jump to
 */
[[codegen::luawrap]] void setTime(double sequenceTime) {
    if (sequenceTime < 0) {
        throw ghoul::lua::LuaError("Sequence time must be greater or equal than 0");
    }
    openspace::global::keyframeRecording->setSequenceTime(sequenceTime);
}

[[codegen::luawrap]] void jumpToKeyframe(int index) {
    if (index < 0) {
        throw ghoul::lua::LuaError("Index must be positive");
    }
    openspace::global::keyframeRecording->jumpToKeyframe(index);
}

/**
 * Returns true if there currently is a sequence loaded, otherwise false.
 */
[[codegen::luawrap]] bool hasKeyframeRecording() {
    return openspace::global::keyframeRecording->hasKeyframeRecording();
}

/**
 * Fetches the sequence keyframes as a JSON object.
 */
[[codegen::luawrap]] std::vector<ghoul::Dictionary> keyframes() {
    return openspace::global::keyframeRecording->keyframes();
}

#include "keyframerecording_lua_codegen.cpp"

} // namespace
