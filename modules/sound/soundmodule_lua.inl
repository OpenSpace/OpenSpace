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

#include <openspace/engine/moduleengine.h>

namespace {

/**
 * Starts playing an audio file. The file must be something that is supported by the
 * SoLoud library, which are WAV, FLAC, MP3, or Ogg Vorbis files. The function returns a
 * number which can be used in other functions to refer to this audio.
 *
 * \param path The address of the file that should be played
 * \param shouldLoop If this value is `true`, the audio file will be played in a loop
 *        until explicitly stopped. If it is `false`, the audio file will be played once
 * \return A numerical handle that can be used to refer to the playing audio in later
 *         function calls. Each number is only returned once
 */
[[codegen::luawrap]] int playAudio(std::filesystem::path path, bool shouldLoop) {
    using namespace openspace;

    int handle = global::moduleEngine->module<SoundModule>()->playAudio(
        std::move(path),
        SoundModule::ShouldLoop(shouldLoop)
    );
    return handle;
}

/**
 * Stops the audio playing of the provided \p handle. If the sound has already stopped,
 * this function does not do anything.
 *
 * \param handle The handle previously returned by a `playAudio` call that will be stopped
 */
[[codegen::luawrap]] void stopAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->stopAudio(handle);
}

/**
 * Returns whether the audio file referred to by the \p handle is currently playing.
 *
 * \param handle The handle for which we want to know whether it is playing
 * \return `true` if the audio file is playing. `false` otherwise
 */
[[codegen::luawrap]] bool isPlaying(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isPlaying(handle);
}

/**
 * Returns a list of all handles that are currently playing.
 */
[[codegen::luawrap]] std::vector<int> currentlyPlaying() {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->currentlyPlaying();
}

/**
 * Sets the volume for the audio file referred to by the provided to the new value. The
 * volume is provided as a number between 0 and 1.
 *
 * \param handle The handle for which we want to change the volume
 * \param volume The new volume value that we want to audio file to have. This value has
 *        to be between 0 and 1
 */
[[codegen::luawrap]] void setVolume(int handle, float volume, float interpolation = 0.5f)
{
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setVolume(handle, volume, interpolation);
}

/**
 * Returns the volume for the audio file referred to by the handle.
 *
 * \return The volume at which the audio file is playing, provided as a value between 0
 *         and 1
 */
[[codegen::luawrap]] float volume(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->volume(handle);
}

[[codegen::luawrap]] void pauseAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->pauseAudio(handle);
}

[[codegen::luawrap]] void resumeAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->resumeAudio(handle);
}

[[codegen::luawrap]] bool isPaused(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isPaused(handle);
}

[[codegen::luawrap]] void setLooping(int handle, bool shouldLoop) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setLooping(
        handle,
        SoundModule::ShouldLoop(shouldLoop)
    );
}

[[codegen::luawrap]] bool isLooping(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isLooping(handle);
}

[[codegen::luawrap]] int playAudio3d(std::string path, glm::vec3 position,
                                     bool shouldLoop = true)
{
    using namespace openspace;

    int handle = global::moduleEngine->module<SoundModule>()->playAudio3d(
        std::move(path),
        position,
        SoundModule::ShouldLoop(shouldLoop)
    );
    return handle;
}

[[codegen::luawrap]] void set3dListenerPosition(glm::vec3 position,
                                                  std::optional<glm::vec3> lookAt,
                                                  std::optional<glm::vec3> up)
{
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->set3dListenerParameters(
        position,
        lookAt,
        up
    );
}

[[codegen::luawrap]] void set3dSourcePosition(int handle, glm::vec3 position) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->set3dSourcePosition(handle, position);
}

[[codegen::luawrap]] void setSpeakerPosition(int handle, glm::vec3 position) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setSpeakerPosition(handle, position);
}

[[codegen::luawrap]] glm::vec3 speakerPosition(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->speakerPosition(handle);
}

#include "soundmodule_lua_codegen.cpp"

} // namespace
