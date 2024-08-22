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

#include <openspace/engine/globals.h>

#include <optional>

namespace {

// Starts a new sequence of keyframes with the specified filename.
[[codegen::luawrap]] void newSequence() {

    openspace::global::keyframeRecording->newSequence();
}

// Adds a keyframe at the specified sequence-time.
[[codegen::luawrap]] void addKeyframe(double sequenceTime) {
    openspace::global::keyframeRecording->addKeyframe(sequenceTime);
}

// Removes a keyframe at the specified 0-based index
[[codegen::luawrap]] void removeKeyframe(int index) {
    openspace::global::keyframeRecording->removeKeyframe(index);
}

//
[[codegen::luawrap]] void updateKeyframe(int index) {
    openspace::global::keyframeRecording->updateKeyframe(index);
}

//
[[codegen::luawrap]] void moveKeyframe(int index, double sequenceTime) {
    openspace::global::keyframeRecording->moveKeyframe(index, sequenceTime);
}

// Saves the current sequence of keyframes to disk under the last filename supplied to
// 'newSequence'.
[[codegen::luawrap]] void saveSequence(std::optional<std::string> filename) {
    openspace::global::keyframeRecording->saveSequence(filename);
}

// Loads a sequence from the supplied file.
[[codegen::luawrap]] void loadSequence(std::string filename) {
    openspace::global::keyframeRecording->loadSequence(std::move(filename));
}

//
[[codegen::luawrap]] void play(std::optional<double> sequenceTime) {
    if (sequenceTime.has_value()) {
        openspace::global::keyframeRecording->setSequenceTime(*sequenceTime);
    }

    openspace::global::keyframeRecording->play();
}

//
[[codegen::luawrap]] void pause() {
    openspace::global::keyframeRecording->pause();
}

//
[[codegen::luawrap]] void setTime(double sequenceTime) {
    openspace::global::keyframeRecording->setSequenceTime(sequenceTime);
}

[[codegen::luawrap]] bool hasKeyframeRecording() {
    return openspace::global::keyframeRecording->hasKeyframeRecording();
}

#include "keyframerecording_lua_codegen.cpp"

} // namespace
